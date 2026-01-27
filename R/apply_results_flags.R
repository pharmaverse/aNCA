#' Apply Flagging Logic to NCA Results
#'
#' @description
#' Evaluates NCA results against defined flag settings and intervals. It checks for missing
#' parameters that were requested (based on intervals), applies threshold rules to the `Exclude`
#' column, and determines the final `flagged` status (ACCEPTED, FLAGGED, or MISSING).
#'
#' @param data A data frame of pivoted NCA results.
#' with additional grouping variables merged.
#' @param nca_intervals A data frame. The intervals object from the
#'  PKNCA result (e.g., `res$data$intervals`).
#' @param group_cols A character vector. The column names used for grouping the data.
#' @param flag_settings A named list of flag settings. Each element must contain
#'   `is.checked` (logical) and `threshold` (numeric or character).
#'
#' @return A data frame with updated `Exclude` and `flagged` columns.
#'
#' @importFrom dplyr select mutate group_by summarise left_join rowwise ungroup
#'   case_when intersect any_of across
#' @importFrom purrr keep map_lgl map2_chr
#' @importFrom stringr str_remove str_detect fixed
#' @importFrom formatters var_labels
#' @export
apply_results_flags <- function(data, nca_intervals, group_cols, flag_settings) {

  #Initialise flagged column
  flagged_data <- data %>%
    mutate(
      flagged = "NOT DONE"
    )

  # Add flagging column in the pivoted results
  applied_flags <- purrr::keep(flag_settings, function(x) x$is.checked)
  flag_params <- names(applied_flags)
  flag_params_pknca <- translate_terms(flag_params, "PPTESTCD", "PKNCA")

  flag_thr <- sapply(flag_settings, FUN =  function(x) x$threshold)
  flag_rule_msgs <- c(paste0(names(flag_settings), c(" < ", " < ", " > ", " > ", " < "), flag_thr))

  valid_indices <- map_lgl(flag_params, function(p) {
    any(grepl(paste0("^", p, "(\\[|$)"), names(flagged_data)))
  })

  flag_cols <- names(flagged_data)[formatters::var_labels(flagged_data)
                                   %in% translate_terms(flag_params, "PPTESTCD", "PPTEST")]

  if (length(flag_cols) > 0) {
    requested_flags <- nca_intervals %>%
      select(any_of(c(
        group_cols,
        flag_params_pknca
      ))) %>%
      group_by(across(any_of(group_cols))) %>%
      # Collapse duplicates: if any row is TRUE, the result is TRUE
      summarise(across(any_of(flag_params_pknca), any), .groups = "drop")

    flagged_data <- flagged_data %>%
      left_join(requested_flags, by = intersect(names(.), names(requested_flags))) %>%
      rowwise() %>%
      mutate(
        na_msg_vec = list(
          purrr::map2_chr(flag_cols, flag_params_pknca[valid_indices],
                          function(d_col, f_col) {
                            if (!is.na(d_col) && is.na(get(d_col)) && isTRUE(get(f_col))) {
                              # Return "Param is NA"
                              paste(str_remove(d_col, "\\[.*\\]"), "is NA")
                            } else {
                              NA_character_
                            }
                          }) %>% na.omit()
        ),
        na_msg = paste(na_msg_vec, collapse = "; "),
        # Update Exclude using case_when
        Exclude = case_when(
          # Combine na message and existing Exclude text
          na_msg != "" & !is.na(Exclude) ~ paste(Exclude, na_msg, sep = "; "),
          na_msg != "" ~ na_msg,
          TRUE ~ Exclude
        ),
        flagged = case_when(
          is.na(Exclude) ~ "ACCEPTED",
          any(sapply(
            flag_rule_msgs, function(msg) str_detect(Exclude, fixed(msg))
          )) ~ "FLAGGED",
          TRUE ~ "MISSING"
        )
      ) %>%
      ungroup() %>%
      select(-all_of(c(flag_params_pknca, "na_msg_vec", "na_msg")))
  }

  flagged_data
}
