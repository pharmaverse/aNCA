#' Helper to Apply Default or Override Parameter Selections
#'
#' Populates a selection data frame with boolean columns for each study type,
#' indicating which parameters are selected based on either default rules or a
#' provided override list.
#'
#' @param selection_df A data frame containing PK parameters and their metadata.
#'   Must include a 'PKNCA' column and logical columns for various attributes
#'   (e.g., 'can_excretion', 'can_single_dose').
#' @param study_type_names A character vector of study type names to generate
#'   selection columns for.
#' @param default_params A character vector of default PKNCA parameters to select.
#' @param selections_override An optional named list where names correspond to
#'   study types and values are character vectors of PKNCA parameters to select.
#'   If NULL, default logic is applied.
#'
#' @return The 'selection_df' data frame with added boolean columns for each
#'   study type.
#'
apply_parameter_selections <- function(selection_df,
                                       study_type_names,
                                       default_params,
                                       selections_override = NULL) {
  # Use override if available, otherwise use defaults
  if (is.null(selections_override) || length(selections_override) == 0) {
    # Default behavior
    for (st_name in study_type_names) {
      is_selected <- selection_df$PKNCA %in% default_params

      # Apply metadata rules
      is_selected <- is_selected & if (st_name == "Excretion Data") {
        selection_df$can_excretion
      } else {
        selection_df$can_non_excretion
      }

      if (stringr::str_detect(st_name, "Single")) {
        is_selected <- is_selected & selection_df$can_single_dose
      }
      if (stringr::str_detect(st_name, "Multiple")) {
        is_selected <- is_selected & selection_df$can_multiple_dose
      }
      if (stringr::str_detect(st_name, "Extravascular")) {
        is_selected <- is_selected & selection_df$can_extravascular
      }
      if (stringr::str_detect(st_name, "Metabolite")) {
        is_selected <- is_selected & selection_df$can_metabolite
      }

      selection_df[[st_name]] <- is_selected
    }
  } else {
    # Override behavior
    for (st_name in study_type_names) {
      override_params <- selections_override[[st_name]]
      if (!is.null(override_params)) {
        selection_df[[st_name]] <- selection_df$PKNCA %in% override_params
      } else {
        selection_df[[st_name]] <- FALSE
      }
    }
  }
  return(selection_df)
}
