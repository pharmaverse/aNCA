#' Apply UI-Based Column Mapping to a Dataset
#'
#' This function takes a dataset and applies user-specified column mappings
#'  provided through a Shiny `input` object. It renames columns based
#'  on the selected mappings, handles special cases such as `ADOSEDUR`,
#'  updates units for key variables, applies labels,
#'  and removes detected duplicate concentration records.
#'
#' @param dataset A data frame containing the raw input data.
#' @param input A Shiny input object containing the selected column mappings
#'  from the user interface. Expected to include input IDs like `select_<column_name>`.
#' @param manual_units A list with named elements (`concentration`, `dose`, and `time`)
#'  specifying the manually selectable unit options for relevant columns.
#'
#' @returns A transformed data frame with:
#'   - Renamed columns according to user mappings
#'   - Unit columns updated (if user-specified units are in `manual_units`)
#'   - Concentration duplicates removed
#'   - Labels applied to columns (via `apply_labels()`)
#'
#' @details
#' - Logs the mapping selections for debugging.
#' - Validates that all required columns are mapped and no duplicates exist.
#' - If `ADOSEDUR` is not mapped, it is assigned a value of `0`.
#' - Removes concentration data duplicates using all columns except `ARRLT`, `NRRLT`,
#'  and `NCA_PROFILE`.
#' - Uses global objects like `MAPPING_COLUMN_GROUPS`, `MAPPING_DESIRED_ORDER`, and `LABELS`.
#' @export
apply_column_mapping <- function(dataset, mapping, manual_units, column_groups, desired_order) {

  selected_cols <- sapply(names(column_groups), function(group) {
    sapply(column_groups[[group]], function(column) {
      mapping[[paste0("select_", column)]]
    })
  }, simplify = FALSE)

  #log_debug_list("The following mapping was applied:", purrr::flatten(selected_cols))

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
  nca_profile_col <- mapping$select_NCA_PROFILE

  if (!is.null(nca_profile_col) && nca_profile_col != "" && nca_profile_col != "NA") {
    dataset <- dataset %>% mutate(NCA_PROFILE = as.factor(.data[[nca_profile_col]]))
  }

  if (is.null(mapping$select_ADOSEDUR)) dataset$ADOSEDUR <- 0

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
    # showNotification("Duplicate concentration data detected and removed.
    #                  Please check the data", type = "warning", duration = 5)
  }

  dataset %>% anti_join(conc_duplicates, by = colnames(conc_duplicates))
}

#' Internal helper to rename dataset columns based on selected column mappings.
#'
#' @param dataset A data frame whose columns are to be renamed.
#' @param selected_cols A named list of vectors representing mapped columns
#'
#' @returns A data frame with renamed columns based on the provided mappings.
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
.validate_column_mapping <- function(selected_cols) {
  flat_cols <- unlist(selected_cols)
  if (any(flat_cols == "")) {
    stop("Unmapped columns detected.")
    # showNotification("Some required columns are not mapped. Please complete all selections.",
    #                  type = "error", duration = 5)
    return(FALSE)
  }

  if (any(duplicated(flat_cols))) {
    stop("Duplicate column selection detected.")
    # showNotification("Duplicate column selection detected. Please ensure each selection is unique.",
    #                  type = "error", duration = 5)
    return(FALSE)
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
.apply_manual_units <- function(dataset, mapping, manual_units) {
  if (mapping$select_AVALU %in% manual_units$concentration) dataset$AVALU <- mapping$select_AVALU
  if (mapping$select_DOSEU %in% manual_units$dose) dataset$DOSEU <- mapping$select_DOSEU
  if (mapping$select_RRLTU %in% manual_units$time) dataset$RRLTU <- mapping$select_RRLTU
  dataset
}
