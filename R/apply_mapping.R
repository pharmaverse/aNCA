#' Apply UI-Based Column Mapping to a Dataset
#'
#' This function takes a dataset and applies user-specified column mappings
#'  provided through a Shiny `input` object. It renames columns based
#'  on the selected mappings, handles special cases such as `ADOSEDUR`,
#'  updates units for key variables, applies labels,
#'  and removes detected duplicate concentration records.
#'
#' @param dataset A data frame containing the raw data to be transformed.
#' @param mapping A named list of column mappings.
#' @param manual_units A named list containing valid unit options for different
#'                     variable types (e.g., `concentration`, `dose`, `time`).
#' @param column_groups A named list where names are group categories
#'                      (e.g., "Group Identifiers", "Sample Variables") and values are vectors
#'                      of column names belonging to each group.
#' @param desired_order A character vector specifying the desired column order
#'                      in the output dataset.
#' @param silent Boolean, whether to print message with applied mapping.
#'               Defaults to `FALSE`.
#'
#' @returns A transformed data frame with:
#'   - Renamed columns according to user mappings
#'   - Unit columns updated (if user-specified units are in `manual_units`)
#'   - Concentration duplicates removed
#'   - Labels applied to columns (via `apply_labels()`)
#'
#' @details
#' - Validates that all required columns are mapped and no duplicates exist.
#' - If `ADOSEDUR` is not mapped, it is assigned a value of `0`.
#' - Removes concentration data duplicates using all columns except `ARRLT`, `NRRLT`,
#'  and `NCA_PROFILE`.
#' - Uses global objects like `MAPPING_COLUMN_GROUPS`, `MAPPING_DESIRED_ORDER`, and `LABELS`.
#' @export
apply_mapping <- function(
  dataset, mapping,
  manual_units = NULL, column_groups = NULL, # Not used anymore
  desired_order, silent = FALSE
) {

  if (!silent) {
    paste0(paste0("* ", names(mapping), " -> ", unname(mapping)), collapse = "\n") |>
      message()
  }
  if (!.validate_column_mapping(mapping)) return(NULL)

  ####### TODO (Gerardo): This would be better to be explicit outside the function
  # Grouping_Variables should not be renamed
  mapping <- mapping[names(mapping) != "Grouping_Variables"]   # TODO: Don't include in mapping arg
  # Special case: If ADOSEDUR is not mapped, we assume is 0
  if (is.null(mapping$ADOSEDUR)) dataset$ADOSEDUR <- 0         # TODO: Make it default in select Input
  ################################################################################

  # If a variable to map is not a column in the data.frame, assume is the value to use for its creation
  mapping_values <- mapping[!unname(mapping) %in% names(dataset)]
  for (col in names(mapping_values)) {
    dataset[[col]] <- mapping_values[[col]]
  }
  mapping <- mapping[unname(mapping) %in% names(dataset)]
  
  # Apply the changes to the data
  dataset <- dataset %>%
    # Rename variables based on mapping instructions
    dplyr::rename(!!!mapping) %>%
    # Order the renamed variables based on desired_order
    select(any_of(desired_order), everything()) %>%
    # Apply the default ADPC labels
    apply_labels()

  # Check there are no duplicates
  # TODO(Gerardo): This perhaps should be apart and outside of the mapping function
  conc_duplicates <- dataset %>%
    group_by(across(any_of(setdiff(desired_order, c("ARRLT", "NRRLT", "NCA_PROFILE"))))) %>%
    filter(n() > 1) %>%
    slice(1) %>%
    ungroup() %>%
    select(names(dataset))
  if (nrow(conc_duplicates) > 0) {
    warning("Duplicate concentration data detected and filtered.")
  }
  dataset %>% anti_join(conc_duplicates, by = colnames(conc_duplicates))
}

#' Internal helper to check for missing or duplicate column mappings.
#'
#' @param mapping A named list of vectors representing mapped columns
#'                      grouped by category (e.g., identifiers, dosing, etc.).
#'
#' @returns `TRUE` if valid; otherwise triggers notifications and returns `FALSE`.
#'
#' @noRd
#' @keywords internal
.validate_column_mapping <- function(mapping) {
  if (any(mapping == "")) stop("Unmapped columns detected.")
  if (any(duplicated(mapping))) stop("Duplicate column selection detected.")
  TRUE
}
