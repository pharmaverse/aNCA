#' Apply custom unit conversions to NCA results
#'
#' Joins PKNCA parameter units by all shared columns (including groups),
#' then joins ratio parameter units by PPTESTCD only for unmatched rows.
#' Computes `PPSTRES` from `PPORRES * conversion_factor` where applicable.
#'
#' @param result Data frame of NCA results (with PPTESTCD, PPORRES, PPSTRES,
#'   PPSTRESU columns).
#' @param custom_units Data frame of custom units (with PPTESTCD, PPSTRESU,
#'   conversion_factor, and optional group columns). Rows with all-NA group
#'   columns are treated as ratio parameters and joined by PPTESTCD only.
#' @returns Data frame with PPSTRESU and PPSTRES applied, conversion_factor
#'   removed. PPSTRESU is "" (not NA) for rows without custom units.
#' @importFrom dplyr select mutate left_join any_of all_of distinct
#' @keywords internal
apply_custom_units <- function(result, custom_units) {
  group_cols <- setdiff(
    names(custom_units),
    c("PPTESTCD", "PPORRESU", "PPSTRESU", "conversion_factor")
  )

  # Deduplicate custom_units up front to prevent left_join from producing
  # multiple matches. Key = PPTESTCD + PPORRESU + group columns.
  dedup_cols <- c("PPTESTCD", "PPORRESU", intersect(group_cols, names(custom_units)))
  custom_units <- distinct(custom_units, across(all_of(dedup_cols)), .keep_all = TRUE)
  has_groups <- length(group_cols) > 0
  if (has_groups) {
    is_ratio_row <- rowSums(is.na(custom_units[, group_cols, drop = FALSE])) == length(group_cols)
  } else {
    is_ratio_row <- rep(FALSE, nrow(custom_units))
  }
  pknca_custom <- custom_units[!is_ratio_row, , drop = FALSE]
  ratio_custom <- custom_units[is_ratio_row, , drop = FALSE] %>%
    select("PPTESTCD", "PPSTRESU", "conversion_factor")

  result <- result %>%
    select(-any_of(c("PPSTRESU", "PPSTRES"))) %>%
    mutate(PPSTRESU = NA_character_, conversion_factor = NA_real_)

  if (nrow(pknca_custom) > 0) {
    pknca_join_cols <- intersect(names(result), names(pknca_custom))
    pknca_join_cols <- setdiff(pknca_join_cols, c("PPSTRESU", "conversion_factor"))
    matched <- result %>%
      select(-"PPSTRESU", -"conversion_factor") %>%
      left_join(pknca_custom, by = pknca_join_cols)
    result$PPSTRESU <- matched$PPSTRESU
    result$conversion_factor <- matched$conversion_factor
  }

  if (nrow(ratio_custom) > 0) {
    unmatched <- is.na(result$PPSTRESU)
    if (any(unmatched)) {
      ratio_matched <- result[unmatched, ] %>%
        select(-"PPSTRESU", -"conversion_factor") %>%
        left_join(ratio_custom, by = "PPTESTCD")
      result$PPSTRESU[unmatched] <- ratio_matched$PPSTRESU
      result$conversion_factor[unmatched] <- ratio_matched$conversion_factor
    }
  }

  result %>%
    mutate(
      PPSTRES = ifelse(!is.na(conversion_factor), PPORRES * conversion_factor, PPORRES),
      PPSTRESU = ifelse(is.na(PPSTRESU), "", PPSTRESU)
    ) %>%
    select(-"conversion_factor")
}
