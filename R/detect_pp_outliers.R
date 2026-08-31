#' Flag Tukey/IQR outliers within groups
#'
#' Marks values that fall below `Q1 - 1.5 * IQR` or above `Q3 + 1.5 * IQR`
#' within each group defined by `group_cols`. This is the same rule the
#' boxplot whiskers use, so a flagged value corresponds to a point drawn
#' beyond a whisker.
#'
#' @param data A data frame.
#' @param value_col Name of the numeric column to test.
#' @param group_cols Character vector of columns defining comparison groups.
#'   When empty or `NULL`, all rows form a single group. Columns not present in
#'   `data` are ignored.
#'
#' @returns A logical vector, one element per row of `data`, `TRUE` where the
#'   value is a Tukey/IQR outlier within its group. `NA` values are `FALSE`.
#'   Groups with fewer than 4 non-`NA` values yield all `FALSE` (an IQR is not
#'   meaningful for very small samples).
#' @export
#' @examples
#' d <- data.frame(
#'   g = rep(c("a", "b"), each = 5),
#'   v = c(1, 2, 3, 4, 100, 10, 11, 12, 13, 14)
#' )
#' is_iqr_outlier(d, "v", "g")
is_iqr_outlier <- function(data, value_col, group_cols = character(0)) {
  n <- nrow(data)
  if (n == 0) return(logical(0))

  if (is.null(group_cols)) group_cols <- character(0)
  group_cols <- intersect(group_cols, names(data))

  x <- data[[value_col]]

  flag_group <- function(idx) {
    vals <- x[idx]
    ok <- !is.na(vals)
    out <- rep(FALSE, length(idx))
    if (sum(ok) < 4) return(out)
    q <- stats::quantile(vals[ok], probs = c(0.25, 0.75), names = FALSE)
    iqr <- q[2] - q[1]
    out[ok] <- vals[ok] < (q[1] - 1.5 * iqr) | vals[ok] > (q[2] + 1.5 * iqr)
    out
  }

  if (length(group_cols) == 0) {
    return(flag_group(seq_len(n)))
  }

  # Split row indices by group key and flag within each.
  result <- rep(FALSE, n)
  keys <- do.call(paste, c(data[group_cols], sep = "\r"))
  for (idx in split(seq_len(n), keys)) {
    result[idx] <- flag_group(idx)
  }
  result
}

#' Identify parameters that contain a Tukey/IQR outlier
#'
#' For each parameter (`PPTESTCD`), tests whether any non-excluded record is a
#' Tukey/IQR outlier within the supplied grouping. Excluded records (flag or
#' manual) are dropped before the calculation so they neither define the
#' quartiles nor count as outliers, matching the boxplot behaviour.
#'
#' @param res_nca A PKNCA results object (with `$result` data frame).
#' @param group_cols Character vector of columns defining comparison groups
#'   (typically the boxplot X and colour variables). Columns not present in the
#'   data are ignored.
#' @param value_col Name of the numeric value column. Default `"PPSTRES"`.
#'
#' @returns A character vector of `PPTESTCD` values that contain at least one
#'   outlier. Empty when none qualify.
#' @export
params_with_outliers <- function(res_nca, group_cols = character(0),
                                 value_col = "PPSTRES") {
  result <- res_nca$result
  if (is.null(result) || nrow(result) == 0) return(character(0))

  # Rename manual interval parameters (AUCINT -> AUCINT_0-12) so they are
  # treated as distinct parameters, when the required columns are present.
  if (all(c("type_interval", "start_dose", "end_dose") %in% names(result))) {
    result <- rename_interval_params(result)
  }

  # Drop excluded records (flag or manual) before computing outliers.
  is_flag <- !is.na(result[["exclude"]]) & result[["exclude"]] != ""
  if (".pp_excl" %in% names(result)) {
    pp_excl <- result[[".pp_excl"]]
    is_manual <- !is.na(pp_excl) & pp_excl
  } else {
    is_manual <- rep(FALSE, nrow(result))
  }
  included <- result[!(is_flag | is_manual), , drop = FALSE]
  if (nrow(included) == 0) return(character(0))

  # Outliers are computed per parameter, within the requested grouping.
  outlier_params <- character(0)
  for (param in unique(included$PPTESTCD)) {
    sub <- included[included$PPTESTCD == param, , drop = FALSE]
    if (any(is_iqr_outlier(sub, value_col, group_cols))) {
      outlier_params <- c(outlier_params, param)
    }
  }
  unique(outlier_params)
}
