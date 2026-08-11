#' Bulk export of rendered TLG outputs (issue #1344).
#'
#' After a TLG order is submitted, every output the user can see is held in the
#' corresponding module's `tlg_list()` reactive.  `tab_tlg_server()` collects those into a
#' registry; these helpers turn the registry into a directory tree that
#' `zip::zipr()` can archive:
#'
#' ```
#' Graphs/pkcg01_lin_01-701-1015.png
#' Tables/pkct01_PARAM_DrugA__PCSPEC_SERUM.csv
#' Listings/pkcl01.csv
#' manifest.csv
#' ```
#'
#' Writing is delegated to `save_dispatch()` in `zip-utils.R`, which already knows how to
#' put a ggplot, a data.frame or a plotly object on disk -- graphs reach it as plotly
#' objects carrying their source ggplot (see `.with_ggplot()`), so PNG works without a
#' headless browser.

# Type -> folder name, per the issue's requested structure.
.TLG_EXPORT_DIRS <- c(table = "Tables", listing = "Listings", graph = "Graphs")

#' Prepare a table or listing for writing to a flat file.
#'
#' Two adjustments so the file matches what the user is looking at:
#'   * an rlistings `listing_df` keeps every column of the source data -- including working
#'     columns like `is.excluded.hl` and `id_list` -- but only *displays* those in
#'     `listing_dispcols()`.  Export the displayed set.
#'   * grouped summary tables get their two-level header flattened (see below).
#'
#' @param df A data frame.
#' @returns A plain data frame ready for `save_dispatch()`.
#' @noRd
.prepare_export_frame <- function(df) {
  if (inherits(df, "listing_df") && requireNamespace("rlistings", quietly = TRUE)) {
    disp <- tryCatch(rlistings::listing_dispcols(df), error = function(e) NULL)
    if (length(disp) > 0 && all(disp %in% names(df))) {
      df <- as.data.frame(df)[, disp, drop = FALSE]
    }
  }
  .flatten_export_cols(df)
}

#' Flatten a two-level "Compare in columns" header into single-level names.
#'
#' The summary tables built with `col_group_var` prefix each statistic column with
#' `"<level><.GROUP_SEP>"` and carry a `col_groups` attribute; the app splits those back
#' into a two-level reactable header.  CSV and XLSX have one header row, so the raw names
#' would reach the file with a `\037` control character embedded in them.  Fold the group
#' and statistic into one readable label instead: `"F - Mean"`.
#'
#' @param df A data frame (returned unchanged if it has no grouped columns).
#' @returns `df` with flattened column names.
#' @noRd
.flatten_export_cols <- function(df) {
  sep <- aNCA:::.GROUP_SEP
  nms <- names(df)
  if (length(nms) == 0 || !any(grepl(sep, nms, fixed = TRUE))) return(df)
  names(df) <- vapply(nms, function(nm) {
    if (!grepl(sep, nm, fixed = TRUE)) return(nm)
    parts <- strsplit(nm, sep, fixed = TRUE)[[1]]
    # Prefer the display label the stats builder attached to the leaf, so the export reads
    # the same as the on-screen header ("Geometric Mean", not "GeoMean").
    leaf <- attr(df[[nm]], "label")
    if (is.null(leaf) || !nzchar(leaf)) leaf <- parts[length(parts)]
    paste(parts[1], leaf, sep = " - ")
  }, character(1), USE.NAMES = FALSE)
  df
}

#' Make a string safe to use inside a file name.
#'
#' Split keys arrive as human-readable labels -- `"PARAM: DrugA / PCSPEC: SERUM"`,
#' `"01-701-1015"` -- which contain separators that are not portable in paths.
#'
#' @param x Character scalar (or anything coercible).
#' @returns A slug with runs of non-alphanumeric characters collapsed to `_`, trimmed of
#'   leading/trailing `_`, and capped at 60 characters.  `""` for empty/NA input.
#' @noRd
.tlg_export_slug <- function(x) {
  if (length(x) == 0 || is.na(x[1]) || !nzchar(as.character(x[1]))) return("")
  s <- gsub("[^A-Za-z0-9]+", "_", as.character(x[1]))
  s <- gsub("^_+|_+$", "", s)
  substr(s, 1, 60)
}

#' Build the file name (without extension) for one exported TLG output.
#'
#' The catalog id already carries the pkid and variant (`g_pkcg01_lin` -> `pkcg01_lin`), so
#' the leading type prefix is dropped and the split key appended, giving names like
#' `pkcg01_lin_DrugA` as the issue asks for.  `split_and_apply()` names un-split output
#' `"all"`; that is a sentinel, not a group, so it contributes no suffix.
#'
#' @param g_id      Catalog id (the `tlg.yaml` key), e.g. `"g_pkcg01_lin"`.
#' @param split_key Name of the list element, or `NULL`/`NA` when the list is unnamed.
#' @returns Character scalar.
#' @noRd
.tlg_export_basename <- function(g_id, split_key = NULL) {
  base <- sub("^[gltp]_", "", g_id)
  if (is.null(split_key) || identical(as.character(split_key)[1], "all")) return(base)
  slug <- .tlg_export_slug(split_key)
  if (!nzchar(slug)) base else paste0(base, "_", slug)
}

#' Write every rendered TLG in the registry to `target_dir`.
#'
#' A TLG that failed to render is a character scalar rather than a plot/table (see the
#' `tryCatch` in `tlg_module_server()`).  Those are recorded in the manifest and skipped
#' rather than aborting the download -- one broken output should not cost the user the
#' other thirty.
#'
#' @param entries A named list of `list(def =, type =, items =)`, where `items` is the
#'   already-resolved output list for that TLG (resolve the reactive at the call site so
#'   this stays testable outside Shiny).
#' @param target_dir Directory to write into; created if absent.
#' @param ggplot_formats Formats for graphs, passed to `save_dispatch()`.  Defaults to PNG
#'   only, deliberately: `htmlwidgets::saveWidget()` writes a `_files/` directory of
#'   dependencies next to every widget, so adding `"html"` costs roughly 4 MB of duplicated
#'   jQuery and plotly JavaScript *per graph* -- measured at 8.21 MB versus 0.06 MB for a
#'   single plot.  A large order would produce a several-hundred-MB archive that is almost
#'   entirely repeated JavaScript.  Callers that want the interactive versions can still
#'   ask for `"html"`.
#' @param table_formats  Formats for tables and listings.
#' @returns A data frame manifest, one row per output, invisibly written to
#'   `manifest.csv` in `target_dir`.
#' @noRd
write_tlg_exports <- function(entries,
                              target_dir,
                              ggplot_formats = "png",
                              table_formats  = c("csv", "xlsx")) {
  dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)

  rows <- unlist(
    lapply(names(entries), function(g_id) {
      .export_one_tlg(g_id, entries[[g_id]], target_dir, ggplot_formats, table_formats)
    }),
    recursive = FALSE
  )

  # A type whose TLGs all failed leaves its folder behind empty, which reads as though the
  # outputs were lost rather than skipped.  Drop it -- manifest.csv is the record of what
  # did not make it.  (Same reasoning as .clean_export_dir() in zip-utils.R.)
  for (d in list.dirs(target_dir, recursive = TRUE, full.names = TRUE)[-1]) {
    if (length(list.files(d, recursive = TRUE)) == 0) unlink(d, recursive = TRUE)
  }

  manifest <- if (length(rows) == 0) .tlg_manifest_row()[0, ] else do.call(rbind, rows)
  write.csv(manifest, file.path(target_dir, "manifest.csv"), row.names = FALSE)
  manifest
}

#' Build the TLG archive and report anything that could not be written.
#'
#' Split out of `tab_tlg_server()`'s `downloadHandler` so the module server stays under the
#' cyclomatic complexity limit (the same reason `tlg_add_picker.R` exists).
#'
#' @param entries Resolved registry entries, as built by `.collect_tlg_outputs()`.
#' @param fname   Destination path supplied by `downloadHandler`.
#' @param session Shiny session, used for the progress bar and notifications.
#' @returns Invisibly, the manifest (or `NULL` when there was nothing to export).
#' @noRd
.run_tlg_export <- function(entries, fname, session) {
  progress <- shiny::Progress$new(session)
  on.exit(progress$close(), add = TRUE)
  progress$set(message = "Preparing TLG export...", detail = "Collecting outputs...",
               value = 0.1)

  if (length(entries) == 0) {
    showNotification(
      "No rendered TLGs to export. Submit an order first.",
      type = "warning", duration = 8
    )
    return(invisible(NULL))
  }

  # tempfile(), not a fixed name under tempdir(): Shiny can serve several sessions from one
  # R process, and a fixed directory that is unlink()ed at the start of every download
  # would let two concurrent exports corrupt each other.  Cleaned up afterwards so a large
  # order does not sit in the temp directory for the life of the process.
  target_dir <- tempfile("tlg_export_")
  on.exit(unlink(target_dir, recursive = TRUE), add = TRUE)

  progress$set(detail = "Writing files...", value = 0.4)
  manifest <- write_tlg_exports(entries, target_dir)

  progress$set(detail = "Creating archive...", value = 0.85)
  # `root` rather than setwd(): the working directory is process-global, so changing it
  # mid-download would affect every other session in the same process.
  zip::zipr(
    zipfile = fname,
    files   = list.files(target_dir, recursive = TRUE),
    root    = target_dir,
    mode    = "mirror"
  )

  progress$set(message = "Complete!", detail = "", value = 1)

  skipped <- sum(manifest$status != "ok")
  if (skipped > 0) {
    showNotification(
      paste0(
        skipped, " of ", nrow(manifest), " outputs could not be exported and were left ",
        "out; see manifest.csv in the archive for details."
      ),
      type = "warning", duration = 10
    )
  }
  invisible(manifest)
}

#' Work out a distinct file name (no extension) for each output of one TLG.
#'
#' @param g_id  Catalog id.
#' @param items The TLG's output list.
#' @returns A character vector of unique base names, one per item.
#' @noRd
.tlg_export_basenames <- function(g_id, items) {
  keys    <- names(items)
  unsplit <- .tlg_export_basename(g_id, NULL)
  bases   <- vapply(seq_along(items), function(i) {
    b <- .tlg_export_basename(g_id, if (is.null(keys)) NULL else keys[i])
    # Some builders return an unnamed list (pkcg01's per-subject plots, for one), so there
    # is no split key to name the file after.  Number every one of them, rather than
    # leaving the first bare and suffixing the rest, which reads as if the bare file were
    # a combined output.
    if (length(items) > 1 && identical(b, unsplit)) paste0(b, "_", i) else b
  }, character(1))
  # Guards two split keys that slug down to the same string (e.g. "DrugA / SERUM" and
  # "DrugA - SERUM"), which would otherwise overwrite each other.
  make.unique(bases, sep = "_")
}

#' Write a single TLG's outputs and return its manifest rows.
#'
#' @param g_id  Catalog id.
#' @param entry Registry entry: `list(def =, type =, items =)`.
#' @param target_dir,ggplot_formats,table_formats As for `write_tlg_exports()`.
#' @returns A list of one-row data frames.
#' @noRd
.export_one_tlg <- function(g_id, entry, target_dir, ggplot_formats, table_formats) {
  items <- entry$items
  if (is.null(items) || length(items) == 0) {
    return(list(.tlg_manifest_row(g_id, entry, NA_character_, "empty")))
  }

  # `[[` on a missing name errors, so match first: an unrecognised type lands in Other/
  # rather than taking the whole download down.
  sub_dir <- if (isTRUE(entry$type %in% names(.TLG_EXPORT_DIRS))) {
    .TLG_EXPORT_DIRS[[entry$type]]
  } else {
    "Other"
  }
  out_dir <- file.path(target_dir, sub_dir)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  bases <- .tlg_export_basenames(g_id, items)

  lapply(seq_along(items), function(i) {
    item <- items[[i]]
    # A failed or empty TLG renders as an explanatory string; keep it out of the zip but
    # say so in the manifest.
    if (is.null(item) || is.character(item)) {
      return(.tlg_manifest_row(
        g_id, entry, NA_character_, "skipped", if (is.character(item)) item[1] else ""
      ))
    }

    if (is.data.frame(item)) item <- .prepare_export_frame(item)

    status <- "ok"
    note   <- ""
    tryCatch(
      save_dispatch(item, file.path(out_dir, bases[i]), ggplot_formats, table_formats),
      error = function(e) {
        status <<- "error"
        note   <<- conditionMessage(e)
      }
    )
    .tlg_manifest_row(g_id, entry, file.path(sub_dir, bases[i]), status, note)
  })
}

#' One manifest row. Called with no arguments it yields the empty prototype.
#' @noRd
.tlg_manifest_row <- function(g_id = character(), entry = NULL, file = character(),
                              status = character(), note = "") {
  # Defined locally rather than via rlang/base %||%: base's arrived in R 4.4 and the
  # package declares R (>= 4.1), which CI exercises via the oldrel job.
  def_chr <- function(x) if (is.null(x) || length(x) == 0) "" else as.character(x)[1]
  data.frame(
    id      = if (length(g_id)) g_id else character(),
    label   = if (!is.null(entry)) def_chr(entry$def$label) else character(),
    type    = if (!is.null(entry)) def_chr(entry$def$type) else character(),
    dataset = if (!is.null(entry)) def_chr(entry$def$dataset) else character(),
    file    = if (length(file)) file else character(),
    status  = if (length(status)) status else character(),
    note    = if (length(g_id)) note else character(),
    stringsAsFactors = FALSE
  )
}
