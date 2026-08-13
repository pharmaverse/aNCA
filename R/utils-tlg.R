#' Raise a user-facing TLG warning.
#'
#' Marks a warning as one worth showing the user in the app.  The Shiny layer surfaces
#' conditions of class `tlg_warning` as notifications and ignores everything else, so
#' incidental warnings from ggplot2/plotly never become UI noise.
#'
#' @param ... Passed to `paste0()` to build the message.
#' @returns Invisibly `NULL`; called for the warning condition it signals.
#' @noRd
.tlg_warn <- function(...) {
  warning(warningCondition(paste0(...), class = "tlg_warning"))
}

#' Keep the source ggplot alongside its plotly conversion.
#'
#' `ggplotly()` produces an htmlwidget that can only be written to HTML -- rendering it to
#' PNG or PDF needs a headless browser (kaleido/webshot2), which aNCA does not depend on.
#' Stashing the pre-conversion ggplot on the returned object lets the export layer write
#' raster formats with plain `ggsave()` while still serving HTML from the plotly (#1344).
#'
#' Call this **last**, after any `layout()` chain: `layout()` rebuilds the object and
#' silently drops attributes set before it.
#'
#' @param p  A plotly object returned by `ggplotly()` (and possibly `layout()`).
#' @param gg The ggplot `p` was built from.
#' @returns `p`, with `gg` attached as the `"ggplot"` attribute.
#' @noRd
.with_ggplot <- function(p, gg) {
  attr(p, "ggplot") <- gg
  p
}

#' Split a data frame by grouping variables and apply a function to each subset
#'
#' Common pattern used by all TLG functions that return one output object per
#' analyte/visit/specimen combination.  When `list_vars` is empty (or none of
#' the variables are present in `data`), `fn` is called on the full data frame
#' and the result is returned as a single-element named list
#' `list(all = ...)`.  Otherwise the data is split by the interaction of
#' `list_vars` columns and `fn` is applied to each subset; the results are
#' named by the interaction key.
#'
#' @param data A data frame.
#' @param list_vars Character vector of column names to split by. Absent
#'   columns are silently skipped.
#' @param fn A function that takes a data frame and returns a single output
#'   object (plot, table, listing, ...).
#'
#' @return A named list of `fn` outputs.
#' @noRd
split_and_apply <- function(data, list_vars, fn) {
  present <- intersect(list_vars, names(data))

  if (length(present) == 0) {
    return(list(all = fn(data)))
  }

  # Rows where any split column is NA are excluded: they cannot be assigned to a
  # meaningful group and would otherwise appear as a spurious "NA / PLASMA" page.
  complete_rows <- rowSums(is.na(data[, present, drop = FALSE])) == 0
  if (!all(complete_rows)) {
    .tlg_warn(
      "split_and_apply: ", sum(!complete_rows), " row(s) with NA in split ",
      "variable(s) [", paste(present, collapse = ", "), "] were excluded."
    )
    data <- data[complete_rows, , drop = FALSE]
  }

  if (nrow(data) == 0) return(list(all = fn(data)))

  # Keys carry the variable name as well as the value ("PPCAT: A" rather than "A") because they
  # become the group header above each split output, where a bare value gives the reader no way
  # to tell which variable it came from.
  split_keys <- do.call(
    interaction,
    c(
      lapply(present, function(v) paste0(v, ": ", as.character(data[[v]]))),
      list(sep = " / ", drop = TRUE)
    )
  )

  results <- lapply(levels(split_keys), function(key) {
    fn(data[split_keys == key, , drop = FALSE])
  })
  setNames(results, levels(split_keys))
}

#' Filter ADPP rows to metabolite records
#'
#' Applies a three-tier fallback to identify metabolite rows in ADPP:
#' 1. `METABFL` column -- preferred when included as a grouping variable in
#'    the NCA run (non-missing, non-empty values are kept).
#' 2. `PPCAT` containing "metab" (case-insensitive) -- used when `METABFL`
#'    is absent or all-missing.
#' 3. `PARAM` containing "metab" (case-insensitive) -- final fallback.
#'
#' Throws an informative error when no metabolite data can be found.
#'
#' @param data A CDISC ADPP data frame.
#' @param caller Character string naming the calling function, used in the
#'   error message. Default: `"filter_metabolite_rows"`.
#'
#' @return A filtered data frame containing only metabolite rows.
#' @noRd
filter_metabolite_rows <- function(data, caller = "filter_metabolite_rows") {
  # Preferred: explicit METABFL flag set by the NCA grouping variable
  if ("METABFL" %in% names(data) &&
        any(!is.na(data$METABFL) & data$METABFL != "")) {
    return(
      data[!is.na(data$METABFL) & data$METABFL != "", , drop = FALSE]
    )
  }

  # Fallback: PPCAT or PARAM column containing "metab"
  for (col in c("PPCAT", "PARAM")) {
    if (col %in% names(data) &&
          any(grepl("metab", data[[col]], ignore.case = TRUE))) {
      return(
        data[grepl("metab", data[[col]], ignore.case = TRUE), ,
             drop = FALSE]
      )
    }
  }

  stop(
    caller, ": no metabolite data found. ",
    "METABFL is absent or all missing, and no PPCAT/PARAM values ",
    "contain 'metab'. To use this output, include METABFL as a ",
    "grouping variable in your NCA run, or ensure metabolite rows ",
    "are labelled with 'metab' in PPCAT or PARAM."
  )
}

#' Compute descriptive statistics for a numeric vector of PK values.
#'
#' Returns a one-row data frame of n, Mean, SD, CV%, GeoMean, GeoCV%, Median,
#' Min, Max.  When `include_geo = FALSE`, the GeoMean and GeoCV_pct columns
#' are omitted (used for urine parameters that are not log-normally distributed
#' by convention).
#'
#' Used by [t_pkpt03_col()], [t_pkpt08_uri()], and [t_pkpt11_gmr()] via
#' [.build_pkpp_table()].  Placed here so future table functions can reuse it
#' without duplicating the stat logic.
#'
#' @param vals Numeric vector (NAs already handled by caller or this function).
#' @param include_geo Logical. Include GeoMean and GeoCV_pct columns.
#'   Default: `TRUE`.
#' @noRd
.summarise_adpp <- function(vals, include_geo = TRUE) { # nolint: cyclocomp_linter
  vals <- vals[!is.na(vals)]
  pos  <- vals[vals > 0]
  n    <- length(vals)
  mn   <- if (n > 0) mean(vals) else NA_real_
  s    <- if (n > 1) sd(vals)   else NA_real_
  out  <- data.frame(
    n      = n,
    Mean   = round(mn, 3),
    SD     = round(s,  3),
    CV_pct = if (!is.na(mn) && mn != 0 && !is.na(s))
      round(s / mn * 100, 1) else NA_real_,
    stringsAsFactors = FALSE
  )
  if (include_geo) {
    gm  <- if (length(pos) > 0) exp(mean(log(pos))) else NA_real_
    gs  <- if (length(pos) > 1) sd(log(pos))        else NA_real_
    out <- cbind(out, data.frame(
      GeoMean   = round(gm, 3),
      GeoCV_pct = if (!is.na(gs)) round(sqrt(exp(gs^2) - 1) * 100, 1) else NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  cbind(out, data.frame(
    Median = if (n > 0) round(median(vals), 3) else NA_real_,
    Min    = if (n > 0) round(min(vals),    3) else NA_real_,
    Max    = if (n > 0) round(max(vals),    3) else NA_real_,
    stringsAsFactors = FALSE
  ))
}

#' Human-readable display labels for the descriptive-statistic columns shared by
#' the summary tables (`t_pkct01`, and everything built via [.build_pkpp_table()]).
#'
#' The data frames keep their terse programmatic column names (`GeoMean`,
#' `CV_pct`, ...) so downstream code and tests can reference them; these labels
#' are attached as the `label` attribute and promoted to the rendered column
#' header by `define_cols(header_from_label = TRUE)`.
#' @noRd
.STAT_LABELS <- c(
  n         = "n",
  n_blq     = "Number BLQ",
  Mean      = "Mean",
  SD        = "SD",
  CV_pct    = "CV%",
  Median    = "Median",
  GeoMean   = "Geometric Mean",
  GeoCV_pct = "Geometric CV%",
  Min       = "Min",
  Max       = "Max"
)

#' Attach readable labels to the statistic columns of a summary table.
#'
#' Only statistic columns are touched; grouping/key columns (already labelled via
#' [apply_labels()]) are left as-is.  Group-comparison tables prefix each
#' statistic column with `"<level><.GROUP_SEP>"` (see [.pivot_group_blocks()]);
#' the prefix is stripped before the [.STAT_LABELS] lookup so a prefixed leaf
#' such as `"Male<SEP>Mean"` still gets the "Mean" label.  Applied AFTER the
#' `rbind` in the builders because `rbind` drops per-column attributes.
#'
#' @param df A summary-table data frame.
#' @return `df` with `label` attributes set on its statistic columns.
#' @noRd
.apply_stat_labels <- function(df) {
  for (col in names(df)) {
    base <- sub(paste0("^.*", .GROUP_SEP), "", col)
    if (base %in% names(.STAT_LABELS)) {
      attr(df[[col]], "label") <- unname(.STAT_LABELS[[base]])
    }
  }
  df
}

#' Keep only the user-selected statistic columns of a summary table.
#'
#' Summary builders always compute the full statistic block; this trims the
#' result to the statistics the user asked for, leaving every key / grouping
#' column (and, for group-comparison tables, the column order) intact.  A column
#' is a statistic column when its base name -- the part after any
#' `"<level><.GROUP_SEP>"` prefix, matching [.apply_stat_labels()] -- is one of
#' the names in [.STAT_LABELS].  Requested statistics that a given table never
#' produces (e.g. `n_blq` on ADPP tables, `GeoMean` on `t_pkpt08_uri`) are
#' silently ignored.
#'
#' When a `col_groups` attribute is present (group-comparison tables) it is
#' rebuilt against the surviving columns so the rendered two-level header never
#' references a dropped leaf; groups left with no columns are removed.
#'
#' @param df A labelled summary-table data frame.
#' @param stats Character vector of terse statistic names to keep (e.g.
#'   `c("n", "Mean", "SD")`). `NULL` or empty keeps every column unchanged.
#' @return `df` restricted to key columns plus the selected statistic columns.
#' @noRd
.select_stats <- function(df, stats = NULL) {
  if (is.null(stats) || length(stats) == 0 || ncol(df) == 0) return(df)
  base_name  <- function(col) sub(paste0("^.*", .GROUP_SEP), "", col)
  is_stat    <- vapply(names(df), function(c) base_name(c) %in% names(.STAT_LABELS), logical(1))
  keep_stat  <- vapply(names(df), function(c) base_name(c) %in% stats, logical(1))
  keep       <- !is_stat | keep_stat
  cg <- attr(df, "col_groups")
  out <- df[, keep, drop = FALSE]
  if (!is.null(cg)) {
    kept_cols <- names(df)[keep]
    cg <- lapply(cg, function(leaves) intersect(leaves, kept_cols))
    cg <- cg[vapply(cg, length, integer(1)) > 0]
    attr(out, "col_groups") <- cg
  }
  out
}

#' Sentinel separator used to prefix per-group statistic column names.
#'
#' A control character (unit separator) is used so it can never collide with
#' real column-label text (arm names, RACE strings, ...).  The prefixed names
#' are only a uniqueness device -- the group->columns mapping is carried
#' explicitly via the `col_groups` attribute (see [.make_col_groups()]), so
#' render code never has to parse these names back apart.
#' @noRd
.GROUP_SEP <- ""

#' Ordered levels of a column-group variable.
#'
#' Natural (numeric-aware) sort of the non-missing values, with `NA` coerced to
#' a literal `"NA"` level appended last -- mirroring how [.natural_sort_key()]
#' and the row-key handling keep missing values visible rather than dropping
#' them.
#'
#' @param x A vector.
#' @return A character vector of ordered levels. Blank (`""`/whitespace) and `NA`
#'   values collapse into a single `"NA"` level appended last; a literal `"NA"`
#'   string value is merged into that same level (never duplicated).
#' @noRd
.group_levels <- function(x) {
  gv <- as.character(x)
  # Blank / whitespace-only values are treated as missing so they never become
  # an empty-string ("") level -- reactable cannot group an "" column, and
  # `list(...)[[""]]` returns NULL, which crashes the render.
  gv[is.na(gv) | trimws(gv) == ""] <- NA
  has_na <- anyNA(gv)
  lv     <- unique(gv[!is.na(gv)])
  lv     <- lv[order(.natural_sort_key(lv))]
  if (has_na) lv <- c(setdiff(lv, "NA"), "NA")  # exactly one "NA" level, last
  lv
}

#' Validate a column-group (comparison) variable and return its ordered levels.
#'
#' Errors when the variable is absent, or when it collides with a variable that
#' already defines rows or table splits (which would produce degenerate,
#' duplicated groups).  Warns when the resulting table would be very wide.
#'
#' @param col_group_var Character scalar column name (the comparison variable).
#' @param data The data frame the table is built from.
#' @param reserved Character vector of variables that must NOT be reused as the
#'   comparison variable (stratification / parameter / split columns).
#' @return A character vector of ordered group levels (from [.group_levels()]).
#' @noRd
.resolve_col_group <- function(col_group_var, data, reserved) {
  if (!col_group_var %in% names(data)) {
    stop("Cannot compare in columns by '", col_group_var,
         "': it is not a column in the data.")
  }
  if (col_group_var %in% reserved) {
    stop(
      "Cannot compare in columns by '", col_group_var,
      "': it is already used to define the table rows or splits (",
      paste(reserved, collapse = ", "),
      "). Pick a different variable, or change the row/split variables."
    )
  }
  levels <- .group_levels(data[[col_group_var]])
  if (length(levels) > 6L) {
    .tlg_warn(
      "col_group_var '", col_group_var, "' has ", length(levels),
      " levels; the comparison table will be very wide and may overflow ",
      "horizontally."
    )
  }
  levels
}

#' Pivot a per-cell statistic block into side-by-side per-group blocks.
#'
#' For each level in `group_levels`, `block_fn` is applied to the matching subset
#' of `cell_df` and the resulting one-row statistic block is renamed with a
#' `"<level><.GROUP_SEP><stat>"` prefix; all blocks are then `cbind`'d together.
#' Empty subsets still yield a rectangular `n = 0 / NA` block (both
#' [.summarise_adpp()] and `.summarise_group()` handle empty input), so the
#' result is the same width regardless of which levels are populated in this cell.
#'
#' Readable leaf labels are (re-)applied by [.apply_stat_labels()] on the final
#' wide table AFTER the builder's `rbind` (which drops column attributes), so this
#' helper only computes and prefixes the blocks.
#'
#' @param cell_df Rows for a single (stratum x parameter) / row-key cell.
#' @param group_var Column whose levels become side-by-side groups.
#' @param group_levels Ordered levels defining the column order
#'   (from [.resolve_col_group()]).
#' @param block_fn Function taking a data-frame subset and returning a one-row
#'   statistic data frame (e.g. `.summarise_group`, or a `summary_fn` wrapper).
#' @return A one-row data frame of all per-group statistic blocks, side by side.
#' @noRd
.pivot_group_blocks <- function(cell_df, group_var, group_levels, block_fn) {
  gvals <- as.character(cell_df[[group_var]])
  # Match the level coercion in .group_levels(): blank/whitespace and NA both
  # map to the single "NA" level so every row lands in exactly one group.
  gvals[is.na(gvals) | trimws(gvals) == ""] <- "NA"
  blocks <- lapply(group_levels, function(lvl) {
    block <- block_fn(cell_df[gvals == lvl, , drop = FALSE])
    names(block) <- paste0(lvl, .GROUP_SEP, names(block))
    block
  })
  do.call(cbind, blocks)
}

#' Build the `col_groups` attribute map: group level -> prefixed leaf names.
#'
#' Consumed at render time by `define_col_groups()` to emit
#' `reactable::colGroup()` spanners.  Storing the exact leaf names (rather than
#' re-deriving them by parsing) means a group value containing [.GROUP_SEP] can
#' never corrupt the grouping.
#'
#' @param group_levels Ordered group levels.
#' @param stat_names Terse statistic column names of a single block
#'   (e.g. `c("n", "Mean", ...)`).
#' @return A named list (one entry per level) of prefixed leaf-name vectors.
#' @noRd
.make_col_groups <- function(group_levels, stat_names) {
  setNames(
    lapply(group_levels, function(lvl) paste0(lvl, .GROUP_SEP, stat_names)),
    group_levels
  )
}

#' Build an `order()` key that sorts embedded numbers numerically.
#'
#' `order()` on a character vector is lexical, so "DOSE 10" would sort before
#' "DOSE 2" and arms like "100 mg" before "50 mg".  This returns a key whose
#' lexical order matches natural order: numeric columns are returned unchanged
#' (already numeric-sortable), factors are returned as their level codes (so an
#' upstream-defined order is respected), and for character values each run of
#' digits is zero-padded to a fixed width.  `NA` keys sort last, as with the
#' default `order()`.
#'
#' @param x A vector (numeric, factor, or character).
#' @return A vector suitable as an argument to [order()].
#' @noRd
.natural_sort_key <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.factor(x))  return(as.integer(x))
  vapply(as.character(x), function(s) {
    if (is.na(s)) return(NA_character_)
    parts  <- regmatches(s, gregexpr("[0-9]+|[^0-9]+", s))[[1]]
    is_num <- grepl("^[0-9]+$", parts)
    parts[is_num] <- formatC(parts[is_num], width = 12, flag = "0")
    paste(parts, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

#' Return the label attribute of a column, falling back to the column name.
#'
#' Used by TLG plot functions to label axes.  When a column has a
#' `formatters`-style `label` attribute it is used; otherwise the column name
#' string is returned unchanged.
#'
#' @param data A data frame.
#' @param var  Character scalar column name.
#' @return A character scalar label.
#' @noRd
.get_var_label <- function(data, var) {
  lbl <- attr(data[[var]], "label")
  if (!is.null(lbl)) lbl else var
}
