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
#' - If `ADOSEDUR` is mapped to `"NA"`, it is assigned a value of `0`.
#' - Removes concentration data duplicates using all columns except `ARRLT`, `NRRLT`, and `DOSNO`.
#' - Uses global objects like `MAPPING_COLUMN_GROUPS`, `MAPPING_DESIRED_ORDER`, and `LABELS`.

apply_column_mapping <- function(dataset, mapping, manual_units, column_groups, desired_order) {
  selected_cols <- sapply(names(column_groups), function(group) {
    sapply(column_groups[[group]], function(column) {
      mapping[[paste0("select_", column)]]
    })
  }, simplify = FALSE)

  mapping_log <- lapply(selected_cols, function(group) {
    purrr::imap(group, function(v, n) paste0("* ", n, " -> ", paste0(v, collapse = ", ")))
  }) %>%
    unlist() %>%
    paste0(collapse = "\n")
  log_debug(paste0("The following mapping was applied:\n", mapping_log))

  if (any(unlist(selected_cols) == "")) {
    log_error("Unmapped columns detected.")
    showNotification("Some required columns are not mapped.
                     Please complete all selections.", type = "error", duration = 5)
    return(NULL)
  }

  if (any(duplicated(unlist(selected_cols)))) {
    log_error("Duplicate column selection detected.")
    showNotification("Duplicate column selection detected.
                     Please ensure each selection is unique.", type = "error", duration = 5)
    return(NULL)
  }

  selected_cols[["Group Identifiers"]] <- selected_cols[["Group Identifiers"]][
    names(selected_cols[["Group Identifiers"]]) != "Grouping_Variables"
  ]

  # Rename necessary columns
  colnames(dataset) <- sapply(colnames(dataset), function(col) {
    for (group in names(selected_cols)) {
      column_names <- names(selected_cols[[group]])
      mapped_values <- selected_cols[[group]]
      # Skip renaming if this is select_NCA_PROFILE
      for (i in seq_along(mapped_values)) {
        if (mapped_values[i] == col && !(group == "Group Identifiers" && column_names[i] == "NCA_PROFILE")) {
          return(column_names[i])
        }
      }
    }
    col
  })
  
  # Handle special case for NCA_PROFILE
  nca_profile_col <- mapping$select_NCA_PROFILE
  
  if (!is.null(nca_profile_col) && nca_profile_col != "" && nca_profile_col != "NA") {
    dataset <- dataset %>% mutate(NCA_PROFILE = .data[[nca_profile_col]])
  }

  if (mapping$select_ADOSEDUR == "NA") dataset$ADOSEDUR <- 0

  if (mapping$select_AVALU %in% manual_units$concentration) dataset$AVALU <- mapping$select_AVALU
  if (mapping$select_DOSEU %in% manual_units$dose) dataset$DOSEU <- mapping$select_DOSEU
  if (mapping$select_RRLTU %in% manual_units$time) dataset$RRLTU <- mapping$select_RRLTU

  dataset <- dataset %>%
    relocate(all_of(desired_order)) %>%
    apply_labels(LABELS, "ADPC")

  conc_duplicates <- dataset %>%
    group_by(across(all_of(setdiff(desired_order, c("ARRLT", "NRRLT", "DOSNO"))))) %>%
    filter(n() > 1) %>%
    slice(1) %>%
    ungroup() %>%
    select(names(dataset))

  if (nrow(conc_duplicates) > 0) {
    log_warn("Duplicate concentration data detected and filtered.")
    showNotification("Duplicate concentration data detected and removed.
                     Please check the data", type = "warning", duration = 5)
  }

  dataset %>% anti_join(conc_duplicates, by = colnames(conc_duplicates))
}
