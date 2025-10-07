#' Update Selectize Inputs
#'
#' This function updates the selectize inputs for the Shiny module based
#'  on the provided column names
#' and manual units.
#'
#' @param session The Shiny session object.
#' @param input_ids A vector of input IDs to be updated.
#' @param data_colnames A vector of column names from the dataset.
#' @param manual_units A list containing value choices to be mapped for each input_id.
#' @details
#' This function updates the selectize inputs in the Shiny session based on the
#' provided column names and manual units. It handles special cases for
#' `Grouping_Variables`, `NCA_PROFILE`, and units columns. It also
#' ensures that the selectize inputs are populated with the correct choices and
#' selected values based on the current dataset.
#'
#' @returns None. This function updates the selectize inputs in the Shiny session.

update_selectize_inputs <- function(session, input_ids, data_colnames, alternative_choices) {

  alternative_choices_list <- split(alternative_choices, alternative_choices$Variable)
  for (input_id in input_ids) {
    input_colname <- sub("select_", "", input_id)
    input_info <- alternative_choices_list[[input_colname]]

    # Define the expected choices for the mapping (columns and/or values)
    potential_mappings <- c(
      intersect(
        c(input_colname, strsplit(input_info$mapping_alternatives, ", ")[[1]]),
        data_colnames
      ),
      strsplit(input_info$Values, ", ")[[1]]
    )

    # Define the values to select by default (if possible)
    selected_vals <- if (length(potential_mappings) == 0) {
      NULL
    } else if (input_info$is_multiple_choice) {
      potential_mappings
    } else {
      potential_mappings[[1]]
    }

    # Update the input using the defined choices and default selections
    updateSelectizeInput(
      session,
      input_id,
      choices = list(
        "Select Column" = "",
        "Mapping Columns" = data_colnames,
        "Mapping Values" =  strsplit(alternative_choices_list[[input_colname]]$Values, ", ")[[1]]
      ),
      selected = selected_vals
    )
  }
}
