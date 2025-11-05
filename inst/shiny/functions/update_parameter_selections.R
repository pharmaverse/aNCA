#' Update selection state from UI checkbox inputs
#'
#' @param current_selections The current selection data frame.
#' @param module_inputs A named list of all inputs.
#'
#' @returns A list containing `new_selections` (the updated data frame)
#'  and `has_changes` (a boolean).
update_parameter_selections <- function(current_selections, module_inputs) {

  has_changes <- FALSE
  new_selections <- current_selections

  # 1. Parse all checkbox inputs into a tidy data frame
  potential_updates <- tibble::enframe(module_inputs, name = "chk_name", value = "new_value") %>%
    filter(
      str_starts(chk_name, "chk_"),
      !sapply(new_value, is.null) # Ignore NULL inputs
    ) %>%
    tidyr::separate(
      col = chk_name,
      into = c(NA, "PKNCA", "study_type"), # "chk", "param", "study"
      sep = "_",
      remove = FALSE
    ) %>%
    tidyr::unnest(new_value) %>%
    filter(
      study_type %in% names(current_selections) # Ensure study_type is a valid column
    )

  # 2. Early exit if no valid updates are found
  if (nrow(potential_updates) == 0) {
    return(list(new_selections = new_selections, has_changes = has_changes))
  }

  # 3. Loop over the parsed updates and apply them
  for (i in seq_len(nrow(potential_updates))) {

    update_row <- potential_updates[i, ]
    #Find row index
    row_idx <- which(new_selections$PKNCA == update_row$PKNCA)
    old_value <- new_selections[row_idx, update_row$study_type]

    # Compare and update state
    if (!is.null(old_value) && old_value != update_row$new_value) {
      new_selections[row_idx, update_row$study_type] <- update_row$new_value
      has_changes <- TRUE
    }
  }

  list(new_selections = new_selections, has_changes = has_changes)
}
