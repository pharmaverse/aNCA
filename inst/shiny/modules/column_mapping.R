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
    tagList(
      tags$style(
        type = "text/css",
        "
        h4 {
            margin-bottom: -40px; /* Reduce space below h4 headings */
        }
        .fluidRow {
            margin-top: -5px; /* Reduce space above rows */
        }
        .shiny-input-container {margin-bottom: 0px; }"
      ),
      h4("Group Identifiers"),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -45px;",
            tooltip(
              selectizeInput(
                ns("select_STUDYID"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Select Corresponding Column, in character format."
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("STUDYID:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_STUDYID")),
                   style = "color: grey;")
            )
          )
        )
      ),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -45px;",
            tooltip(
              selectizeInput(
                ns("select_USUBJID"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Character or Numeric format"
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("USUBJID:", style = "color: black; font-size: normal;"),
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_USUBJID")),
                   style = "color: grey;")
            )
          )
        )
      ),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -15px;",
            tooltip(
              selectizeInput(
                ns("select_Grouping_Variables"),
                "",
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Select Column(s)"),
                width = "25%"
              ),
              "Select the additional column(s) that will be used to group the data\n
              for tables, listings and graphs. E.g. Treatment Arm, Age, Sex, Race"
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("Additional Grouping Variables", style = "color: black; font-size: normal;")
            ),
          )
        )
      ),
      h4("Sample Variables"),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -45px;",
            tooltip(
              selectizeInput(
                ns("select_ANALYTE"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Analyte"
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("ANALYTE:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_ANALYTE")),
                   style = "color: grey;")
            )
          )
        )
      ),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -45px;",
            tooltip(
              selectizeInput(
                ns("select_PCSPEC"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Matrix"
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("PCSPEC:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_PCSPEC")),
                   style = "color: grey;")
            )
          )
        )
      ),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -15px;",
            tooltip(
              selectizeInput(
                ns("select_AVAL"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Analysis value in numeric format."
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("AVAL:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_AVAL")),
                   style = "color: grey;")
            )
          )
        )
      ),
      h4("Dose Variables"),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -45px;",
            tooltip(
              selectizeInput(
                ns("select_DOSNO"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Numeric format."
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("DOSNO:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_DOSNO")),
                   style = "color: grey;")
            )
          )
        )
      ),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -45px;",
            tooltip(
              selectizeInput(
                ns("select_ROUTE"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Route of administration, stating either 'intravascular' or 'extravascular'."
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("ROUTE:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_ROUTE")),
                   style = "color: grey;")
            )
          )
        )
      ),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -45px;",
            tooltip(
              selectizeInput(
                ns("select_DOSEA"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Actual Dose amount in numeric format."
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("DOSEA:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_DOSEA")),
                   style = "color: grey;")
            )
          )
        )
      ),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -15px;",
            tooltip(
              selectizeInput(
                ns("select_ADOSEDUR"),
                "",
                choices = c("Select Column" = "", "NA"),
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Duration of dose administration. 
              Only required for infusion studies,\notherwise select NA"
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("ADOSEDUR:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_ADOSEDUR")),
                   style = "color: grey;")
            )
          )
        )
      ),
      h4("Time Variables"),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -45px;",
            tooltip(
              selectizeInput(
                ns("select_AFRLT"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Numeric format"
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("AFRLT:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_AFRLT")),
                   style = "color: grey;")
            )
          )
        )
      ),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -45px;",
            tooltip(
              selectizeInput(
                ns("select_ARRLT"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Numeric format"
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("ARRLT:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_ARRLT")),
                   style = "color: grey;")
            )
          )
        )
      ),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -45px;",
            tooltip(
              selectizeInput(
                ns("select_NFRLT"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Numeric format"
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("NFRLT:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_NFRLT")),
                   style = "color: grey;")
            )
          )
        )
      ),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -15px;",
            tooltip(
              selectizeInput(
                ns("select_NRRLT"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Numeric format"
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("NRRLT:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_NRRLT")),
                   style = "color: grey;")
            )
          )
        )
      ),
      h4("Unit Variables"),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -45px;",
            tooltip(
              selectizeInput(
                ns("select_AVALU"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Unit of analysis value."
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("AVALU:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_AVALU")),
                   style = "color: grey;")
            )
          )
        )
      ),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -45px;",
            tooltip(
              selectizeInput(
                ns("select_DOSEU"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Unit of dose amount."
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("DOSEU:", style = "color: black; font-size: normal;")
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_DOSEU")),
                   style = "color: grey;")
            )
          )
        )
      ),
      fluidRow(
        column(
          8,
          div(
            style = "display: flex; align-items: center; margin-bottom: -15px;",
            tooltip(
              selectizeInput(
                ns("select_RRLTU"),
                "",
                choices = NULL,
                options = list(placeholder = "Select Column"),
                width = "25%"
              ),
              "Unit of time."
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span("RRLTU:", style = "color: black; font-size: normal;"),
            ),
            div(
              style = "margin-left: 10px; margin-top: 20px; margin-bottom: 0px;",
              span(textOutput(ns("label_RRLTU")),
                   style = "color: grey;")
            )
          )
        )
      )
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
    for (label in desired_order) {
      local({
        current_label <- label
        output_name <- paste0("label_", current_label)
        output[[output_name]] <- renderText({
          get_label(current_label)
        })
      })
    }

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
      dataset <- apply_labels(dataset, labels_file)

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
