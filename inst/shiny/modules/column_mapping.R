#' Column Mapping Widget
#'
#' A reusable UI component for mapping dataset columns to specific identifiers or roles.
#'
#' @param ns A namespace function to generate IDs for Shiny inputs.
#' @param id A string representing the unique identifier for the widget.
#' @param tooltip_text A string containing the tooltip text to guide users.
#'
#' @return A Shiny `div` containing a `selectizeInput` with associated labels and tooltip.
#'
#' @examples
#' column_mapping_widget(ns = NS("example"), id = "STUDYID",
#' tooltip_text = "Select the study identifier column.")
.column_mapping_widget <- function(ns, id, tooltip_text) {
  div(
    class = "column-mapping-row",
    tooltip(
      selectizeInput(
        ns(paste0("select_", id)),
        "",
        choices = NULL,
        options = list(placeholder = "Select Column"),
        width = "40%"
      ),
      tooltip_text
    ),
    div(
      class = "column-mapping-output",
      span(paste0(id, ":"))
    ),
    div(
      class = "column-mapping-label",
      span(textOutput(ns(paste0("label_", id))))
    )
  )
}

#' Column Mapping Module
#' #' This module provides implementation for mapping columns from a dataset to specific
#' roles required for analysis. It allows users to select columns for various categories such as
#' group identifiers, sample variables, dose variables, time variables, and unit variables.
#'
#' @param id The module ID.
#' @param data A reactive expression that returns the dataset to be processed.
#' @param manual_units A list containing manual units for concentration, dose, and time.
#' @param on_submit A callback function to be executed when the submit button is clicked.
#'
#' @return A list containing:
#' \item{processed_data}{A reactive expression that returns the processed dataset.}
#' \item{grouping_variables}{A reactive expression that returns the selected grouping variables.}
#'
#' @details
#' The column_mapping_server module allows users to map columns from their dataset to specific
#' roles required for analysis. Users can select columns for the following categories:
#' \itemize{
#' \item \strong{Group Identifiers}:
#' Columns that identify groups, such as study ID and subject ID.
#' \item \strong{Sample Variables}:
#' Columns that describe sample characteristics, such as analyte and specimen.
#' \item \strong{Dose Variables}:
#' Columns that describe dosing information, such as dose number and dose amount.
#' \item \strong{Time Variables}:
#' Columns that describe time-related information, such as relative time.
#' \item \strong{Unit Variables}:
#' Columns that describe units for various measurements, such as concentration and dose units.
#' }
#'
#' The module also handles special cases, such as allowing users
#' to select "NA" for the dose duration
#' column and providing options for manual units.
#'
#' When the submit button is clicked, the module processes the dataset by:
#' \itemize{
#' \item Renaming columns based on user selections.
#' \item Handling manual units.
#' \item Reordering columns according to a desired order.
#' }
#' The processed dataset and selected grouping variables are returned as reactive expressions.
column_mapping_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Data Mapping"),
    p(
      "The following columns are required for data analysis. Please ensure each of these columns",
      " has been assigned a corresponding column from your dataset"
    ),
    # Adjusted layout using CSS flexbox
    tags$section(
      class = "column-mapping-section",
      h5("Group Identifiers"),
      .column_mapping_widget(ns, "STUDYID", "Select Corresponding Column, in character format."),
      .column_mapping_widget(ns, "USUBJID", "Character or Numeric format"),
      div(
        class = "column-mapping-row",
        tooltip(
          selectizeInput(
            ns("select_Grouping_Variables"),
            "",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "Select Column(s)"),
            width = "40%"
          ),
          "Select the additional column(s) that will be used to group the data 
          for tables, listings, and graphs. E.g. Treatment Arm, Age, Sex, Race"
        ),
        div(
          class = "column-mapping-output",
          span("Additional Grouping Variables")
        )
      )
    ),
    tags$section(
      class = "column-mapping-section",
      h5("Sample Variables"),
      .column_mapping_widget(ns, "ANALYTE", "Character format"),
      .column_mapping_widget(ns, "PCSPEC", "Character format"),
      .column_mapping_widget(ns, "AVAL", "Numeric format.")
    ),
    tags$section(
      class = "column-mapping-section",
      h5("Dose Variables"),
      .column_mapping_widget(ns, "DOSNO", "Numeric format."),
      .column_mapping_widget(
        ns, "ROUTE",
        "Character format, stating either 'intravascular' or 'extravascular'."
      ),
      .column_mapping_widget(ns, "DOSEA", "Numeric format."),
      .column_mapping_widget(
        ns, "ADOSEDUR",
        "Numeric format. Only required for infusion studies,
        otherwise select NA"
      )
    ),
    tags$section(
      class = "column-mapping-section",
      h5("Time Variables"),
      .column_mapping_widget(ns, "AFRLT", "Numeric format"),
      .column_mapping_widget(ns, "ARRLT", "Numeric format"),
      .column_mapping_widget(ns, "NFRLT", "Numeric format"),
      .column_mapping_widget(ns, "NRRLT", "Numeric format")
    ),
    tags$section(
      class = "column-mapping-section",
      h5("Unit Variables"),
      .column_mapping_widget(ns, "AVALU", "Character format."),
      .column_mapping_widget(ns, "DOSEU", "Character format."),
      .column_mapping_widget(ns, "RRLTU", "Character format.")
    ),
    input_task_button(ns("submit_columns"), "Submit Mapping")
  )
}

column_mapping_server <- function(id, data, manual_units, on_submit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define the required columns and group them into categories
    column_groups <- list(
      "Group Identifiers" = c("STUDYID", "USUBJID", "Grouping_Variables"),
      "Sample Variables" = c("ANALYTE", "PCSPEC", "ROUTE", "AVAL"),
      "Dose Variables" = c("DOSNO", "DOSEA", "ADOSEDUR"),
      "Time Variables" = c("AFRLT", "ARRLT", "NFRLT", "NRRLT"),
      "Unit Variables" = c("AVALU", "DOSEU", "RRLTU")
    )


    # Derive input IDs from column_groups
    input_ids <- unlist(lapply(column_groups, function(cols) {
      paste0("select_", cols)
    }))

    # Define the desired column order
    desired_order <- c("STUDYID", "USUBJID", "ANALYTE",
                       "PCSPEC", "AVAL", "AVALU", "AFRLT",
                       "ARRLT", "NRRLT", "NFRLT", "RRLTU",
                       "ROUTE", "DOSEA", "DOSEU", "DOSNO",
                       "ADOSEDUR")

    # Loop through each label and create the renderText outputs
    purrr::walk(desired_order, \(label) {
      output[[paste0("label_", label)]] <- renderText(get_label(LABELS, label, "ADPC"))
    })

    # Populate the static inputs with column names
    observeEvent(data(), {
      column_names <- names(data())
      update_selectize_inputs(session, input_ids, column_names, manual_units, desired_order)
    })

    # Global variable to store grouping variables
    grouping_variables <- reactiveVal(NULL)

    # Reactive value for the processed dataset
    processed_data <- reactiveVal(NULL)

    # Observe submit button click and update processed_data
    observeEvent(input$submit_columns, {
      Sys.sleep(1) # Make this artificially slow to show the loading spinner

      req(data())
      dataset <- data()

      # Get the selected columns
      selected_cols <- sapply(names(column_groups), function(group) {
        sapply(column_groups[[group]], function(column) {
          input[[(paste0("select_", column))]]
        })
      }, simplify = FALSE)

      # Check for duplicate column selections
      all_selected_columns <- unlist(selected_cols)
      if (any(duplicated(all_selected_columns))) {
        showModal(modalDialog(
          title = "Duplicate Column Selections",
          "Please ensure that each column selection is unique.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }

      # Extract and store the "Grouping_Variables" column
      grouping_variables(input$select_Grouping_Variables)

      # Remove "Grouping_Variables" from selected columns to prevent renaming
      selected_cols[["Group Identifiers"]] <- selected_cols[["Group Identifiers"]][
        names(selected_cols[["Group Identifiers"]]) != "Grouping_Variables"
      ]

      # Rename columns
      colnames(dataset) <- sapply(colnames(dataset), function(col) {
        for (group in names(selected_cols)) {
          if (col %in% selected_cols[[group]]) {
            return(names(selected_cols[[group]])[which(selected_cols[[group]] == col)])
          }
        }
        return(col)
      })

      # Handle ADOSEDUR == NA case
      if (input$select_ADOSEDUR == "NA") {
        dataset$ADOSEDUR <- 0
      }

      # Update dataset columns if manual units are selected
      if (input$select_AVALU %in% manual_units$concentration) {
        dataset$AVALU <- input$select_AVALU
      }
      if (input$select_DOSEU %in% manual_units$dose) {
        dataset$DOSEU <- input$select_DOSEU
      }
      if (input$select_RRLTU %in% manual_units$time) {
        dataset$RRLTU <- input$select_RRLTU
      }

      # Reorder columns based on the desired order
      dataset <- dataset %>%
        relocate(all_of(desired_order)) %>%
        mutate(TIME = ifelse(DOSNO == 1, AFRLT, ARRLT))#TODO: Remove this after AUC0 merged

      # Apply labels to the dataset
      dataset <- apply_labels(dataset, LABELS, "ADPC")

      # Update the processed data
      processed_data(dataset)

      # Execute the callback function to change the tab
      on_submit()
    })

    list(
      processed_data = processed_data,
      grouping_variables = grouping_variables
    )
  })
}
