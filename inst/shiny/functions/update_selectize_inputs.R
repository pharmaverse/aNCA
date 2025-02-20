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
#' @param desired_order A vector of desired column order.
#'
#' @return None. This function updates the selectize inputs in the Shiny session.

update_selectize_inputs <- function(session, input_ids, column_names, manual_units, desired_order) {

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
    "select_ADOSEDUR" = c("Select Column" = "", column_names, "NA"),
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
    updateSelectizeInput(
      session, input_id,
      choices = special_cases[[input_id]],
      selected =
        if (sub("select_", "", input_id) %in% column_names)
          sub("select_", "", input_id) else NULL
    )
  }
}
