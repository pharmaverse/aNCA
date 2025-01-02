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
  for (input_id in input_ids) {
    column_name <- sub("select_", "", input_id)
    selected_value <- if (column_name %in% column_names) column_name else NULL
    updateSelectizeInput(
      session, input_id, choices = c("Select Column" = "", column_names),
      selected = selected_value
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
      selected = if (sub("select_", "", input_id)
                     %in% column_names) sub("select_", "", input_id) else NULL
    )
  }

  updateSelectizeInput(
    session, "select_Grouping_Variables",
    choices = setdiff(column_names, desired_order)
  )
}
