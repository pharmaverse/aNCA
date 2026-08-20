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

  if (identical(entry$type, "graph")) {
    .export_graph_items(g_id, entry, sub_dir, target_dir, ggplot_formats)
  } else {
    .export_tabular_items(g_id, entry, sub_dir, target_dir, table_formats)
  }
}

#' Strip the TLG stem off each base name, leaving just the split key.
#'
#' Inside a per-TLG directory the stem is redundant.  An unsplit TLG has nothing left after
#' the strip, so it falls back to the stem rather than producing an extension-only file.
#' @noRd
.tlg_leaf_names <- function(bases, stem) {
  leaf <- sub(paste0("^", stem, "_?"), "", bases)
  ifelse(nzchar(leaf), leaf, stem)
}

#' Write one TLG's graphs, one directory per format.
#'
#' Formats are kept apart -- `Graphs/png/pkcg01_lin/` rather than PNG and HTML interleaved
#' in one listing -- so that "give me the plots for my report" is a single folder.  This
#' mirrors the `csv/` and `xlsx/` split the tables already use.  Every TLG gets its own
#' subdirectory even when it produced a single output, so the layout is predictable instead
#' of mixing loose files and directories at the top level (#1344).
#'
#' @param g_id,entry,sub_dir,target_dir,ggplot_formats See `.export_one_tlg()`.
#' @returns A list of one-row data frames, one per output per format.
#' @noRd
.export_graph_items <- function(g_id, entry, sub_dir, target_dir, ggplot_formats) {
  items <- entry$items
  stem  <- .tlg_export_basename(g_id, NULL)
  leaf  <- .tlg_leaf_names(.tlg_export_basenames(g_id, items), stem)

  # PDF is written as one multi-page document per TLG rather than a file per plot: paging
  # through pkcg01_lin.pdf beats opening twenty-odd separate files, and it is smaller.
  per_file <- setdiff(ggplot_formats, "pdf")

  rows <- lapply(seq_along(items), function(i) {
    # Emitted once per output, not once per format: a TLG that failed to render did so
    # regardless of what it would have been written as.
    skip <- .tlg_skip_row(g_id, entry, items[[i]])
    if (!is.null(skip)) return(list(skip))

    lapply(per_file, function(fmt) {
      rel_dir <- file.path(sub_dir, fmt, stem)
      dir.create(file.path(target_dir, rel_dir), showWarnings = FALSE, recursive = TRUE)
      .tlg_write_one(
        items[[i]],
        file.path(target_dir, rel_dir, leaf[i]),
        file.path(rel_dir, paste0(leaf[i], ".", fmt)),
        g_id, entry, fmt, character()
      )
    })
  })
  rows <- unlist(rows, recursive = FALSE)

  if ("pdf" %in% ggplot_formats) {
    renderable <- Filter(function(x) !is.null(x) && !is.character(x), items)
    if (length(renderable) > 0) {
      rows <- c(rows, list(
        .tlg_write_graph_pdf(renderable, stem, sub_dir, target_dir, g_id, entry)
      ))
    }
  }
  rows
}

#' Write one TLG's graphs as a single multi-page PDF.
#'
#' Pages come from the ggplot each plot was built from -- `.with_ggplot()` stashes it on the
#' plotly object precisely so raster and vector output do not need a headless browser.  A
#' plot with no stashed ggplot cannot be drawn and is reported in the manifest note rather
#' than silently producing a blank page.
#'
#' @param plots  The TLG's renderable outputs (no error strings).
#' @param stem   Base name for the TLG, e.g. `"pkcg01_lin"`.
#' @param sub_dir,target_dir,g_id,entry As for `.export_one_tlg()`.
#' @returns A single manifest row for the document.
#' @noRd
.tlg_write_graph_pdf <- function(plots, stem, sub_dir, target_dir, g_id, entry) {
  rel_dir <- file.path(sub_dir, "pdf")
  dir.create(file.path(target_dir, rel_dir), showWarnings = FALSE, recursive = TRUE)
  rel <- file.path(rel_dir, paste0(stem, ".pdf"))

  gg <- lapply(plots, function(x) if (inherits(x, "ggplot")) x else attr(x, "ggplot"))
  drawable <- Filter(function(x) inherits(x, "ggplot"), gg)

  status <- "ok"
  note   <- paste(length(drawable), if (length(drawable) == 1) "page" else "pages")
  if (length(drawable) < length(gg)) {
    note <- paste0(note, "; ", length(gg) - length(drawable), " could not be drawn")
  }
  if (length(drawable) == 0) {
    return(.tlg_manifest_row(g_id, entry, NA_character_, "skipped",
                             "none of the plots could be rendered to PDF"))
  }

  tryCatch({
    grDevices::pdf(file.path(target_dir, rel), width = 10, height = 6, onefile = TRUE)
    on.exit(grDevices::dev.off(), add = TRUE)
    for (p in drawable) print(p)
  }, error = function(e) {
    status <<- "error"
    note   <<- conditionMessage(e)
  })
  .tlg_manifest_row(g_id, entry, rel, status, note)
}

#' Write one TLG's tables or listings, in each requested format.
#'
#' CSV and XLSX go to their own subdirectories so neither listing is cluttered by the
#' other's files.  They are shaped differently on purpose: a split TLG becomes one workbook
#' with a sheet per split under `xlsx/`, which is what you would hand to someone, while
#' `csv/` keeps one file per split because CSV has no notion of sheets -- grouped into a
#' per-TLG directory, the same way the graphs are (#1344).
#'
#' @param g_id,entry,sub_dir,target_dir,table_formats See `.export_one_tlg()`.
#' @returns A list of one-row data frames.
#' @noRd
.export_tabular_items <- function(g_id, entry, sub_dir, target_dir, table_formats) {
  items <- entry$items
  bases <- .tlg_export_basenames(g_id, items)
  stem  <- .tlg_export_basename(g_id, NULL)
  leaf  <- .tlg_leaf_names(bases, stem)

  # Prepare every frame up front: both formats write the same content.
  prepared <- lapply(items, function(x) if (is.data.frame(x)) .prepare_export_frame(x) else x)
  usable   <- vapply(prepared, is.data.frame, logical(1))

  csv_dir <- file.path(sub_dir, "csv", stem)

  rows <- lapply(seq_along(items), function(i) {
    skip <- .tlg_skip_row(g_id, entry, items[[i]])
    if (!is.null(skip)) return(skip)
    if (!"csv" %in% table_formats) return(NULL)
    dir.create(file.path(target_dir, csv_dir), showWarnings = FALSE, recursive = TRUE)
    .tlg_write_one(
      prepared[[i]], file.path(target_dir, csv_dir, leaf[i]),
      file.path(csv_dir, paste0(leaf[i], ".csv")), g_id, entry, character(), "csv"
    )
  })

  if ("xlsx" %in% table_formats && any(usable)) {
    rows <- c(rows, list(.tlg_write_workbook(
      prepared[usable], names(items)[usable], bases[usable],
      stem, sub_dir, target_dir, g_id, entry
    )))
  }
  Filter(Negate(is.null), rows)
}

#' Manifest row for an output that cannot be written, or `NULL` if it can.
#'
#' A failed or empty TLG arrives as an explanatory string rather than an object (see the
#' `tryCatch` in `tlg_module_server()`); keep it out of the archive but say so.
#' @noRd
.tlg_skip_row <- function(g_id, entry, item) {
  if (!is.null(item) && !is.character(item)) return(NULL)
  .tlg_manifest_row(
    g_id, entry, NA_character_, "skipped", if (is.character(item)) item[1] else ""
  )
}

#' Write one object and return its manifest row.
#' @noRd
.tlg_write_one <- function(item, path, rel_path, g_id, entry, ggplot_formats, table_formats) {
  status <- "ok"
  note   <- ""
  tryCatch(
    save_dispatch(item, path, ggplot_formats, table_formats),
    error = function(e) {
      status <<- "error"
      note   <<- conditionMessage(e)
    }
  )
  .tlg_manifest_row(g_id, entry, rel_path, status, note)
}

#' Write one TLG's splits as a single multi-sheet workbook.
#'
#' @param frames Prepared data frames.
#' @param keys   Their split keys (may be `NULL` for an unnamed list).
#' @param bases  Their unique base names, used when there is no split key.
#' @returns A single manifest row for the workbook.
#' @noRd
.tlg_write_workbook <- function(frames, keys, bases, stem, sub_dir, target_dir, g_id, entry) {
  out_dir <- file.path(target_dir, sub_dir, "xlsx")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  rel <- file.path(sub_dir, "xlsx", paste0(stem, ".xlsx"))

  names(frames) <- .tlg_sheet_names(keys, bases, stem)
  status <- "ok"
  note   <- if (length(frames) > 1) paste(length(frames), "sheets") else ""
  tryCatch(
    writexl::write_xlsx(frames, path = file.path(target_dir, rel)),
    error = function(e) {
      status <<- "error"
      note   <<- conditionMessage(e)
    }
  )
  .tlg_manifest_row(g_id, entry, rel, status, note)
}

#' Excel-safe, unique sheet names for a TLG's splits.
#'
#' Excel rejects `[]:*?/\` and caps names at 31 characters, so the split key cannot be used
#' verbatim -- the full key stays in the file name and the manifest.
#' @noRd
.tlg_sheet_names <- function(keys, bases, stem) {
  raw <- if (is.null(keys)) bases else ifelse(is.na(keys) | !nzchar(keys), bases, keys)
  # "all" is split_and_apply()'s un-split sentinel, not a group.
  raw <- ifelse(raw == "all", stem, raw)
  # Whitelist rather than blacklist: Excel rejects [ ] : * ? / \ , and a bracket expression
  # spelling all of those out is easy to get subtly wrong (`[:` opens a POSIX class).
  nm  <- gsub("[^A-Za-z0-9 _.()+-]", "-", raw)
  nm  <- gsub("-{2,}", "-", nm)
  nm  <- gsub("^[ -]+|[ -]+$", "", nm)
  nm  <- substr(nm, 1, 31)
  nm  <- ifelse(nzchar(nm), nm, "Sheet")
  # make.unique can push past 31 again, so trim once more from the left of the suffix.
  substr(make.unique(nm, sep = "_"), 1, 31)
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
