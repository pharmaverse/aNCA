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
  dataset, mapping, manual_units, column_groups, desired_order, silent = FALSE
) {
  selected_cols <- sapply(names(column_groups), function(group) {
    sapply(column_groups[[group]], function(column) {
      mapping[[column]]
    })
  }, simplify = FALSE)

  if (!silent) {
    .concatenate_list("The following mapping was applied:", purrr::flatten(selected_cols)) |>
      message()
  }

  if (!.validate_column_mapping(selected_cols)) return(NULL)

  selected_cols[["Group Identifiers"]] <- selected_cols[["Group Identifiers"]][
    names(selected_cols[["Group Identifiers"]]) != "NCA_PROFILE"
  ]

  selected_cols[["Supplemental Variables"]] <- selected_cols[["Supplemental Variables"]][
    names(selected_cols[["Supplemental Variables"]]) != "Grouping_Variables"
  ]

  # Rename necessary columns
  dataset <- .rename_columns(dataset, selected_cols)

  # Handle special case for NCA_PROFILE
  nca_profile_col <- mapping$NCA_PROFILE

  if (!is.null(nca_profile_col) && nca_profile_col != "" && nca_profile_col != "NA") {
    dataset <- dataset %>% mutate(NCA_PROFILE = as.factor(.data[[nca_profile_col]]))
  }

  if (is.null(mapping$ADOSEDUR)) dataset$ADOSEDUR <- 0

  dataset <- dataset %>%
    .apply_manual_units(mapping, manual_units) %>%
    relocate(any_of(desired_order)) %>%
    apply_labels(type = "ADPC")

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

#' Internal helper to rename dataset columns based on selected column mappings.
#'
#' @param dataset A data frame whose columns are to be renamed.
#' @param selected_cols A named list of vectors representing mapped columns
#'
#' @returns A data frame with renamed columns based on the provided mappings.
#'
#' @noRd
#' @keywords internal
.rename_columns <- function(dataset, selected_cols) {
  colnames(dataset) <- sapply(colnames(dataset), function(col) {
    for (group in names(selected_cols)) {
      mapped_values <- selected_cols[[group]]
      if (col %in% mapped_values) {
        return(names(mapped_values)[which(mapped_values == col)])
      }
    }
    col
  })
  dataset
}

#' Internal helper to check for missing or duplicate column mappings.
#'
#' @param selected_cols A named list of vectors representing mapped columns
#'                      grouped by category (e.g., identifiers, dosing, etc.).
#'
#' @returns `TRUE` if valid; otherwise triggers notifications and returns `FALSE`.
#'
#' @noRd
#' @keywords internal
.validate_column_mapping <- function(selected_cols) {
  flat_cols <- unlist(selected_cols)
  if (any(flat_cols == "")) {
    stop("Unmapped columns detected.")
  }

  if (any(duplicated(flat_cols))) {
    stop("Duplicate column selection detected.")
  }

  TRUE
}


#' Internal helper that adds unit metadata (AVALU, DOSEU, RRLTU) to the dataset
#' if defined in the `manual_units` list.
#'
#' @param dataset A `data.frame` or `tibble` representing the input data.
#' @param mapping A list of selected values from the UI mapping input.
#' @param manual_units A named list containing valid unit options.
#'
#' @returns A modified `dataset` with updated unit columns if applicable.
#'
#' @noRd
#' @keywords internal
.apply_manual_units <- function(dataset, mapping, manual_units) {
  if (mapping$AVALU %in% manual_units$concentration) dataset$AVALU <- mapping$AVALU
  if (mapping$DOSEU %in% manual_units$dose) dataset$DOSEU <- mapping$DOSEU
  if (mapping$RRLTU %in% manual_units$time) dataset$RRLTU <- mapping$RRLTU
  dataset
}
