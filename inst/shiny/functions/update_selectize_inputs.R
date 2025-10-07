#' Update Selectize Inputs
#'
#' This function updates the selectize inputs for the Shiny module based
#'  on the provided column names
#' and manual units.
#'
#' @param session The Shiny session object.
#' @param input_ids A vector of input IDs to be updated.
#' @param column_names A vector of column names from the dataset.
#' @param manual_units A list containing manual units for concentration, dose, and time.
#' @details
#' This function updates the selectize inputs in the Shiny session based on the
#' provided column names and manual units. It handles special cases for
#' `Grouping_Variables`, `NCA_PROFILE`, and units columns. It also
#' ensures that the selectize inputs are populated with the correct choices and
#' selected values based on the current dataset.
#'
#' @returns None. This function updates the selectize inputs in the Shiny session.

update_selectize_inputs <- function(session, input_ids, column_names, manual_units) {

  # Define the desired columns for Grouping_Variables
  desired_grouping_columns <- c("TRTA", "TRTAN", "ACTARM", "TRT01A",
                                "TRT01P", "AGE", "RACE", "SEX", "GROUP", "NOMDOSE", "DOSEP")

  for (input_id in input_ids) {
    # Remove the "select_" prefix to get the actual column name
    column_name <- sub("select_", "", input_id)
    # Determine the selected value(s) based on the column name
    if (column_name == "Grouping_Variables") {
      # Find which desired grouping columns are present
      selected_values <- intersect(desired_grouping_columns, column_names)

      # If none are present, set to NULL
      if (length(selected_values) == 0) {
        selected_values <- NULL
      }
    } else if (column_name == "NCA_PROFILE") {
      # Find which desired grouping columns are present
      nca_profile_cols <- c("ATPTREF", "DOSNO", "DOSNOP")
      selected_values <- if (any(nca_profile_cols %in% column_names)) {
        nca_profile_cols[nca_profile_cols %in% column_names][1]
      } else {
        NULL
      }
    } else {
      # For other columns, use basic logic
      selected_values <- if (column_name %in% column_names) column_name else NULL
    }
    # Update the Selectize input with the new choices and selected values
    updateSelectizeInput(
      session,
      input_id,
      choices = c("Select Column" = "", column_names),
      selected = selected_values
    )
  }

  special_cases <- list(
    "select_AVALU" = list(
      "Dataset Columns" = column_names,
      "Manual Units" = manual_units$concentration
    ),
    "select_DOSEU" = list(
      "Dataset Columns" = column_names,
      "Manual Units" = manual_units$dose
    ),
    "select_RRLTU" = list(
      "Dataset Columns" = column_names,
      "Manual Units" = manual_units$time
    )
  )

  for (input_id in names(special_cases)) {
    # Custom logic for DOSEU fallback to DOSEAU
    selected <- {
      # Custom logic for DOSEU fallback to DOSEAU
      param_name <- sub("select_", "", input_id)
      if (input_id == "select_DOSEU" && "DOSEAU" %in% column_names) {
        "DOSEAU"
      } else if (param_name %in% column_names) {
        param_name
      } else {
        NULL
      }
    }
    updateSelectizeInput(
      session, input_id,
      choices = c("Select Column" = "", special_cases[[input_id]]),
      selected = selected
    )
  }
}
