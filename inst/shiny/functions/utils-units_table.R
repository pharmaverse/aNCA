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
  bind_rows(default_units, ratio_rows[, names(default_units)])
}

#' Merge custom units into a default units table
#'
#' Applies user-edited unit overrides onto the default table via `rows_update`.
#' @param default_units Data frame of default units (with `default = TRUE`).
#' @param custom_units Data frame of user-edited units from `session$userData$units_table()`.
#' @return The merged data frame.
.merge_custom_units <- function(default_units, custom_units) {
  custom_units <- mutate(custom_units, default = FALSE)
  by_cols <- intersect(names(default_units), names(custom_units))
  by_cols <- setdiff(by_cols, c("PPSTRESU", "conversion_factor", "default"))
  rows_update(default_units, custom_units, by = by_cols, unmatched = "ignore")
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

  # Build a per-group set of active parameters by pivoting intervals so that
  # only (group, parameter) pairs where the parameter is TRUE are kept.
  param_cols <- names(purrr::keep(mydata$intervals, is.logical))
  if (length(param_cols) > 0 && length(group_cols) > 0) {
    active_params <- mydata$intervals %>%
      select(any_of(group_cols), any_of(param_cols)) %>%
      tidyr::pivot_longer(
        cols = any_of(param_cols), names_to = "PPTESTCD", values_to = ".active"
      ) %>%
      filter(.active) %>%
      select(-".active") %>%
      distinct()
  } else if (length(param_cols) > 0) {
    active_params <- data.frame(
      PPTESTCD = names(purrr::keep(mydata$intervals, ~ is.logical(.x) && any(.x))),
      stringsAsFactors = FALSE
    )
  } else {
    active_params <- data.frame(PPTESTCD = character(), stringsAsFactors = FALSE)
  }

  numbered_tbl <- tbl %>% mutate(nrow = row_number())

  # Match PKNCA rows against active (group, parameter) pairs
  join_cols <- intersect(names(numbered_tbl), names(active_params))
  pknca_rows <- inner_join(numbered_tbl, unique(active_params), by = join_cols)

  # Ratio rows are always visible
  ratio_rows <- numbered_tbl %>% filter(PPTESTCD %in% ratio_pptestcds)

  rows_to_keep <- bind_rows(pknca_rows, ratio_rows) %>% distinct(nrow)

  setdiff(seq_len(nrow(tbl)), rows_to_keep$nrow)
}
