#' Units Table Utility Functions
#'
#' Helpers for building and filtering the parameter units table.
#' Used internally by the units table module.

#' Append ratio unit rows to a default units table
#'
#' Aligns columns between the default units and ratio rows, then binds them.
#' @param default_units Data frame of default PKNCA units (with `default` column).
#' @param ratio_rows Data frame from `derive_ratio_units()`, or NULL.
#' @return The combined data frame, or `default_units` unchanged if no ratio rows.
.append_ratio_rows <- function(default_units, ratio_rows) {
  if (is.null(ratio_rows) || nrow(ratio_rows) == 0) return(default_units)
  ratio_rows$default <- TRUE
  for (col in setdiff(names(default_units), names(ratio_rows))) {
    ratio_rows[[col]] <- NA
  }
  dplyr::bind_rows(default_units, ratio_rows[, names(default_units)])
}

#' Merge custom units into a default units table
#'
#' Applies user-edited unit overrides onto the default table via `rows_update`.
#' @param default_units Data frame of default units (with `default = TRUE`).
#' @param custom_units Data frame of user-edited units from `session$userData$units_table()`.
#' @return The merged data frame.
.merge_custom_units <- function(default_units, custom_units) {
  custom_units <- dplyr::mutate(custom_units, default = FALSE)
  by_cols <- intersect(names(default_units), names(custom_units))
  by_cols <- setdiff(by_cols, c("PPSTRESU", "conversion_factor", "default"))
  dplyr::rows_update(default_units, custom_units, by = by_cols, unmatched = "ignore")
}

#' Determine which row indices in the units table should be hidden
#'
#' Filters the units table to rows matching active PKNCA parameters and groups.
#' Ratio parameter rows are always kept visible.
#' @param tbl Data frame of the full modal units table.
#' @param mydata PKNCA data object (with `$conc`, `$units`, `$intervals`).
#' @param ratio_pptestcds Character vector of ratio parameter PPTESTCDs.
#' @return Integer vector of row indices to hide.
.compute_rows_to_hide <- function(tbl, mydata, ratio_pptestcds) {
  group_cols <- intersect(
    names(PKNCA::getGroups(mydata$conc)), names(mydata$units)
  )
  groups_to_keep <- dplyr::select(mydata$intervals, dplyr::any_of(group_cols))
  params_to_keep <- names(purrr::keep(mydata$intervals, ~ is.logical(.x) && any(.x)))

  rows_to_keep <- tbl %>%
    dplyr::mutate(nrow = dplyr::row_number()) %>%
    dplyr::filter(PPTESTCD %in% params_to_keep | PPTESTCD %in% ratio_pptestcds)

  if (ncol(groups_to_keep) > 0 && length(ratio_pptestcds) > 0) {
    pknca_rows <- dplyr::filter(rows_to_keep, !PPTESTCD %in% ratio_pptestcds)
    ratio_rows <- dplyr::filter(rows_to_keep, PPTESTCD %in% ratio_pptestcds)
    pknca_rows <- dplyr::inner_join(
      pknca_rows, unique(groups_to_keep),
      by = intersect(names(pknca_rows), names(groups_to_keep))
    )
    rows_to_keep <- dplyr::bind_rows(pknca_rows, ratio_rows)
  } else if (ncol(groups_to_keep) > 0) {
    rows_to_keep <- dplyr::inner_join(
      rows_to_keep, unique(groups_to_keep),
      by = intersect(names(rows_to_keep), names(groups_to_keep))
    )
  }

  setdiff(seq_len(nrow(tbl)), rows_to_keep$nrow)
}
