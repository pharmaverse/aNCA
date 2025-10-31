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
#' @param req_mappings A character vector indicating the names of the mapping object
#'                     that must always be populated
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
#'   `AFRLT`, `STUDYID`, `PCSPEC`, `DOSETRT`, `USUBJID`, and `PARAM`
#'
#' @details
#' - Validates that all required columns are mapped and no duplicates exist.
#' - If `ADOSEDUR` is not mapped, it is assigned a value of `0`.
#' - Removes concentration data duplicates using all columns except `ARRLT`, `NRRLT`,
#'  and `ATPTREF`.
#' @importFrom dplyr rename select any_of everything group_by slice ungroup
#' @export
apply_mapping <- function(
  dataset, mapping, desired_order, silent = TRUE,
  req_mappings = c(
    "USUBJID", "AFRLT", "NFRLT", "ARRLT", "NRRLT",
    "PCSPEC", "ROUTE", "AVAL", "STUDYID", "ATPTREF",
    "AVALU", "RRLTU", "DOSEU", "PARAM"
  )
) {

  if (!silent) {
    paste0(paste0("* ", names(mapping), " -> ", unname(mapping)), collapse = "\n") |>
      message()
  }
  .validate_column_mapping(mapping, req_mappings)

  new_dataset <- dataset

  ####### TODO (Gerardo): This would be better to be explicit outside the function
  # Grouping_Variables should not be renamed
  mapping <- mapping[!names(mapping) %in% c("Grouping_Variables", "Metabolites")]

  # Special case: If ADOSEDUR is not mapped, we assume is 0
  if (is.null(mapping$ADOSEDUR)) {
    new_dataset$ADOSEDUR <- 0       # TODO: Make it default in select
    warning("Dose duration is assumed to be 0  for all records (ADOSEDUR = 0)")
  }
  # Special case: If DOSETRT is not mapped, we assume is equal to PARAM
  if (mapping$DOSETRT == "") {
    new_dataset$DOSETRT <- dataset[[mapping$PARAM]]
    warning("Treatment is assumed to be the same as the analyte for all records (DOSETRT = PARAM)")
  }

  # Conflictive original columns with the mapping will be removed. Warn the user
  is_col_rep_in_map_and_df <- names(mapping) %in% names(dataset)
  is_col_not_used_in_map <- sapply(names(mapping), \(n) !any(n %in% unname(mapping)))
  conflictive_cols <- is_col_rep_in_map_and_df & is_col_not_used_in_map
  if (any(conflictive_cols)) {
    conflictive_colnames <- names(conflictive_cols)[unname(conflictive_cols)]
    warning(
      paste0(
        "Conflictive column names between input and mapping names are removed: ",
        paste0(conflictive_colnames, collapse = ", ")
      )
    )
  }
  ################################################################################

  # If a variable to map is not a column in the data, assume is the value to use for its creation
  mappings_not_in_data <- mapping[!unname(mapping) %in% names(dataset) & mapping != ""]
  for (col in names(mappings_not_in_data)) {
    new_dataset[[col]] <- mappings_not_in_data[[col]]
  }
  mapping <- mapping[unname(mapping) %in% names(dataset)]

  # Apply the changes to the data
  new_dataset <- new_dataset %>%
    # Rename variables based on mapping instructions (allowing duplicate uses)
    mutate(!!!lapply(mapping, function(col) dataset[[col]])) %>%
    # Order the renamed variables based on desired_order
    select(any_of(desired_order), everything()) %>%
    # Apply the default ADPC labels
    apply_labels()

  # Remove variables that are now redundant due to the renaming
  redundant_cols <- unname(mapping)[!unname(mapping) %in% names(mapping)]
  new_dataset <- new_dataset[!names(new_dataset) %in% redundant_cols]

  # Special case: make ATPTREF a factor. TODO: Try make it in the mapping obj
  if (!is.null(new_dataset$ATPTREF)) {
    new_dataset[["ATPTREF"]] <- as.factor(new_dataset[["ATPTREF"]])
  }

  # Check there are no duplicates
  # TODO(Gerardo): This perhaps should be apart and outside of the mapping function (PKNCA obj)
  filt_duplicates_dataset <- new_dataset %>%
    group_by(across(any_of(setdiff(desired_order, c("ARRLT", "NRRLT", "NCA_PROFILE"))))) %>%
    slice(1) %>%
    ungroup() %>%
    as.data.frame()

  if (nrow(filt_duplicates_dataset) < nrow(new_dataset)) {
    warning("Duplicate concentration data detected and filtered.")
  }
  filt_duplicates_dataset
}

#' Internal helper to check for missing required columns to map
#'
#' @param mapping A named list of vectors representing mapped columns
#'                      grouped by category (e.g., identifiers, dosing, etc.).
#' @param req_mappings A character vector indicating the names of the mapping object
#'                     that must always be populated
#'
#' @returns Error if the checking does not pass
#'
#' @noRd
#' @keywords internal
.validate_column_mapping <- function(mapping, req_mappings) {
  missing_maps <- names(mapping[mapping == ""])
  if (any(missing_maps %in% req_mappings)) {
    stop("Unmapped required columns detected: ", paste0(missing_maps, collapse = ", "))
  }
}

#' Create METABFL Column Based on Selected Metabolites
#' @param dataset A data frame containing a dataset with a PARAM (parameter) column.
#' @param metabolites A character vector of parameter names representing metabolites.
#' @importFrom dplyr mutate
#' @returns The input dataset with an additional METABFL column indicating metabolite records ("Y")
#' or non-metabolite records ("").
create_metabfl <- function(dataset, metabolites) {
  mutate(dataset, METABFL = ifelse(PARAM %in% metabolites, "Y", ""))
}
