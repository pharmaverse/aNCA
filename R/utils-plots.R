#' Tooltip construction helper
#' @param data the dataframe
#' @param tooltip_vars character vector of tooltip variables to extract
#' @param labels_df data.frame used for label lookups in tooltips
#' @returns data with added tooltip_text column
.handle_tooltips <- function(data, tooltip_vars, labels_df) {

  data$tooltip_text <- NA_character_

  if (nrow(data) > 0) {
    # 1. Round numeric tooltip variables for cleaner display
    numeric_tt_vars <- intersect(tooltip_vars, names(data)[sapply(data, is.numeric)])
    if (length(numeric_tt_vars) > 0) {
      data <- data %>%
        mutate(across(all_of(numeric_tt_vars), ~ round(., digits = 2)))
    }

    # 2. Generate Tooltip Text
    if (!is.null(tooltip_vars)) {
      if (!is.null(labels_df)) {
        # Use the shared helper function if available and labels provided
        data$tooltip_text <- generate_tooltip_text(data, labels_df, tooltip_vars, "ADNCA")
      } else {
        # Fallback: Create simple "Var: Value" string
        valid_vars <- intersect(tooltip_vars, names(data))
        if (length(valid_vars) > 0) {
          parts <- lapply(valid_vars, \(v) paste0(v, ": ", data[[v]]))
          data$tooltip_text <- paste(parts, collapse = "<br>")
        }
      }
    }
  }

  data
}
