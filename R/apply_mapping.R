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
#' @param desired_order A character vector specifying the desired column order
#'                      in the output dataset.
#' @param silent Boolean, whether to print message with applied mapping.
#'               Defaults to `TRUE`.
#'
#' @returns A transformed data frame with:
#'   - Renamed columns according to user mappings
#'   - Created columns for mapped non-existent variables (e.g., `mg/L` in `AVALU`)
#'   - Created columns for unmapped required variables (e.g., `ADOSEDUR`)
#'   - Ordered columns as specified
#'   - Labels applied to columns (via `apply_labels()`)
#'   - Concentration duplicates removed based on key identifiers:
#'   `AFRLT`, `STUDYID`, `PCSPEC`, `DRUG`, `USUBJID`, and `PARAM`
#'
#' @details
#' - Validates that all required columns are mapped and no duplicates exist.
#' - If `ADOSEDUR` is not mapped, it is assigned a value of `0`.
#' - Removes concentration data duplicates using all columns except `ARRLT`, `NRRLT`,
#'  and `NCA_PROFILE`.
#'  @importFrom dplyr rename select any_of everything group_by slice ungroup
#' @export
apply_mapping <- function(dataset, mapping, desired_order, silent = TRUE
) {

  if (!silent) {
    paste0(paste0("* ", names(mapping), " -> ", unname(mapping)), collapse = "\n") |>
      message()
  }
  .validate_column_mapping(mapping)

  ####### TODO (Gerardo): This would be better to be explicit outside the function
  # Grouping_Variables should not be renamed
  mapping <- mapping[names(mapping) != "Grouping_Variables"] # TODO: Don't include in mapping arg
  # Special case: If ADOSEDUR is not mapped, we assume is 0
  if (is.null(mapping$ADOSEDUR)) dataset$ADOSEDUR <- 0       # TODO: Make it default in select
  ################################################################################

  # If a variable to map is not a column in the data, assume is the value to use for its creation
  mapping_values <- mapping[!unname(mapping) %in% names(dataset)]
  for (col in names(mapping_values)) {
    dataset[[col]] <- mapping_values[[col]]
  }
  mapping <- mapping[unname(mapping) %in% names(dataset)]

  # Apply the changes to the data
  dataset <- dataset %>%
    # Rename variables based on mapping instructions
    rename(!!!mapping) %>%
    # Order the renamed variables based on desired_order
    select(any_of(desired_order), everything()) %>%
    # Apply the default ADPC labels
    apply_labels()

  # Special case: make NCA_PROFILE a factor. TODO: Try make it in the mapping obj
  if (!is.null(dataset$NCA_PROFILE)) dataset[["NCA_PROFILE"]] <- as.factor(dataset[["NCA_PROFILE"]])

  # Check there are no duplicates
  # TODO(Gerardo): This perhaps should be apart and outside of the mapping function (PKNCA obj)
  filt_duplicates_dataset <- dataset %>%
    group_by(across(any_of(c("AFRLT", "STUDYID", "PCSPEC", "DRUG", "USUBJID", "PARAM")))) %>%
    slice(1) %>%
    ungroup() %>%
    as.data.frame()

  if (nrow(filt_duplicates_dataset) < nrow(dataset)) {
    warning("Duplicate concentration data detected and filtered.")
  }
  filt_duplicates_dataset
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
