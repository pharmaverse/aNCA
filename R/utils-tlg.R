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
#' 1. `METABFL` column — preferred when included as a grouping variable in
#'    the NCA run (non-missing, non-empty values are kept).
#' 2. `PPCAT` containing "metab" (case-insensitive) — used when `METABFL`
#'    is absent or all-missing.
#' 3. `PARAM` containing "metab" (case-insensitive) — final fallback.
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
