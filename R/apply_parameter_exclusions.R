#' Apply parameter exclusions to NCA results for CDISC export
#'
#' Tags rows in the NCA result with `.pp_excl` and `.pp_excl_reason` columns,
#' which are consumed by [export_cdisc()] to populate `PPSUMFL`/`PPSUMRSN` in ADPP.
#'
#' @param res A PKNCA result object (list with `$result` data frame).
#' @param excl_info A list with `indices` (integer vector of zero-based row
#'   indices) and `reasons` (character vector of exclusion reasons).
#'   If `NULL` or empty, the result is returned unchanged.
#'
#' @returns The input `res` with `.pp_excl` and `.pp_excl_reason` columns
#'   added to `res$result`.
#'
#' @export
apply_parameter_exclusions <- function(res, excl_info) {
  if (is.null(excl_info) || length(excl_info$indices) == 0) {
    return(res)
  }
  n <- nrow(res$result)
  excl_indices <- excl_info$indices
  excl_reasons <- excl_info$reasons

  res$result$.pp_excl <- seq_len(n) %in% excl_indices
  reason_vec <- rep(NA_character_, n)
  if (length(excl_indices) > 0 && length(excl_indices) == length(excl_reasons)) {
    reason_vec[excl_indices] <- excl_reasons
  }
  res$result$.pp_excl_reason <- reason_vec
  res
}
