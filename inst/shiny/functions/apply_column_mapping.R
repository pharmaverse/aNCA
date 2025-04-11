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

apply_column_mapping <- function(dataset, input, manual_units) {

  # Get the selected columns for each group
  selected_cols <- sapply(names(MAPPING_COLUMN_GROUPS), function(group) {
    sapply(MAPPING_COLUMN_GROUPS[[group]], function(column) {
      input[[paste0("select_", column)]]
    })
  }, simplify = FALSE)

  # Log the mapping for debugging
  mapping_log <- lapply(selected_cols, function(group) {
    purrr::imap(group, function(v, n) paste0("* ", n, " -> ", paste0(v, collapse = ", "))
    )
  }) %>%
    unlist() %>%
    paste0(collapse = "\n")
  log_debug(paste0("The following mapping was applied:\n", mapping_log))

  # Validation: Ensure no unmapped or duplicate selections
  if (any(unlist(selected_cols) == "")) {
    log_error("Unmapped columns detected.")
    showNotification(
      ui = "Some required columns are not mapped. Please complete all selections.",
      type = "error",
      duration = 5
    )
    return()
  }

  if (any(duplicated(unlist(selected_cols)))) {
    log_error("Duplicate column selection detected.")
    showNotification(
      ui = "Duplicate column selection detected. Please ensure each selection is unique.",
      type = "error",
      duration = 5
    )
    return()
  }

  # Remove "Grouping_Variables" from the "Group Identifiers" mapping
  selected_cols[["Group Identifiers"]][["Grouping_Variables"]] <- NULL

  # Rename dataset columns based on the mapping selections
  colnames(dataset) <- sapply(colnames(dataset), function(col) {
    for (group in names(selected_cols)) {
      if (col %in% selected_cols[[group]]) {
        return(names(selected_cols[[group]])[which(selected_cols[[group]] == col)])
      }
    }
    col
  })

  # Special handling: set ADOSEDUR to 0 if mapped to "NA"
  if (input$select_ADOSEDUR == "NA") {
    dataset$ADOSEDUR <- 0
  }

  # Update manual unit columns if selected
  if (input$select_AVALU %in% manual_units$concentration) {
    dataset$AVALU <- input$select_AVALU
  }
  if (input$select_DOSEU %in% manual_units$dose) {
    dataset$DOSEU <- input$select_DOSEU
  }
  if (input$select_RRLTU %in% manual_units$time) {
    dataset$RRLTU <- input$select_RRLTU
  }

  # Reorder columns per the desired order and apply labels
  dataset <- dataset %>% relocate(all_of(MAPPING_DESIRED_ORDER))
  dataset <- apply_labels(dataset, LABELS, "ADPC")

  # Detect and remove concentration duplicates (using all columns except a few)
  conc_duplicates <- dataset %>%
    group_by(across(all_of(setdiff(MAPPING_DESIRED_ORDER, c("ARRLT", "NRRLT", "DOSNO"))))) %>%
    filter(n() > 1) %>%
    slice(1) %>%
    ungroup() %>%
    select(names(dataset))

  if (nrow(conc_duplicates) > 0) {
    log_warn("Duplicate concentration data detected and filtered.")
    showNotification(
      ui = "Duplicate concentration data detected and removed. Please check the data",
      type = "warning",
      duration = 5
    )
  }

  dataset <- dataset %>% anti_join(conc_duplicates, by = colnames(conc_duplicates))
  dataset
}
