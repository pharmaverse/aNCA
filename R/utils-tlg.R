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
    warning(
      "split_and_apply: ", sum(!complete_rows), " row(s) with NA in split ",
      "variable(s) [", paste(present, collapse = ", "), "] were excluded."
    )
    data <- data[complete_rows, , drop = FALSE]
  }

  if (nrow(data) == 0) return(list(all = fn(data)))

  split_keys <- do.call(
    interaction,
    c(
      lapply(present, function(v) as.character(data[[v]])),
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
filter_metabolite_rows <- function(
    data, caller = "filter_metabolite_rows") {
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
#' Only columns present in both the data frame and [.STAT_LABELS] are touched;
#' grouping/key columns (already labelled via [apply_labels()]) are left as-is.
#'
#' @param df A summary-table data frame.
#' @return `df` with `label` attributes set on its statistic columns.
#' @noRd
.apply_stat_labels <- function(df) {
  for (col in intersect(names(df), names(.STAT_LABELS))) {
    attr(df[[col]], "label") <- unname(.STAT_LABELS[[col]])
  }
  df
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
