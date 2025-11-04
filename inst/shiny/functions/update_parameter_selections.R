#' Update selection state from UI checkbox inputs
#'
#' Pure function that takes the current state and a list of inputs,
#' figures out what changed, and returns the new state.
#'
#' @param current_selections The current selection data frame (from `isolate(selection_state())`).
#' @param module_inputs A named list of all inputs in the module (from `reactiveValuesToList(input)`).
#'
#' @returns A list containing `new_selections` (the updated data frame) and `is_dirty` (a boolean).
update_parameter_selections <- function(current_selections, module_inputs) {
  
  has_changes <- FALSE
  new_selections <- current_selections
  
  # Filter for just our dynamically-created checkboxes
  checkbox_input_names <- names(module_inputs)[
    startsWith(names(module_inputs), "chk_")
  ]
  
  if (length(checkbox_input_names) == 0) {
    return(list(new_selections = new_selections, has_changes = has_changes))
  }
  
  for (chk_name in checkbox_input_names) {
    # Parse the chk_id: "chk_cmax_sd-oral"
    parts <- stringr::str_split(chk_name, "_", n = 3)[[1]]
    param_name <- parts[2]
    study_type <- parts[3]
    
    new_value <- module_inputs[[chk_name]]
    row_idx <- which(new_selections$PKNCA == param_name)
    
    if (length(row_idx) > 0 && (study_type %in% names(new_selections))) {
      old_value <- new_selections[row_idx, study_type]
      
      # If UI value is different from state value, update the state
      if (!is.null(new_value) && !is.null(old_value) && new_value != old_value) {
        new_selections[row_idx, study_type] <- new_value
        has_changes <- TRUE
      }
    }
  }
  
  list(new_selections = new_selections, has_changes = has_changes)
}