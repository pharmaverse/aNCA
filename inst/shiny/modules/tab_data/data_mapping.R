#' Define the manual units for concentration, dose, and time in a format recognized by PKNCA
MANUAL_UNITS <- list(
  concentration = c(
    "mg/L", "µg/mL", "ng/mL", "pg/mL", "mol/L", "mmol/L", "µmol/L", "nmol/L", "pmol/L", "mg/dL",
    "µg/dL", "ng/dL"
  ),
  dose = c(
    "mg", "g", "µg", "ng", "pg", "mol", "mmol", "µmol", "nmol", "pmol", "mg/kg", "g/kg", "µg/kg",
    "ng/kg", "pg/kg", "mol/kg", "mmol/kg", "µmol/kg", "nmol/kg", "pmol/kg"
  ),
  time = c("sec", "min", "hr", "day", "week", "month", "year")
)

# Define the required columns and group them into categories
MAPPING_COLUMN_GROUPS <- list(
  "Group Identifiers" = c("STUDYID", "USUBJID", "Grouping_Variables"),
  "Sample Variables" = c("PARAM", "PCSPEC", "ROUTE", "AVAL"),
  "Dose Variables" = c("DRUG", "DOSNO", "DOSEA", "ADOSEDUR"),
  "Time Variables" = c("AFRLT", "ARRLT", "NFRLT", "NRRLT"),
  "Unit Variables" = c("AVALU", "DOSEU", "RRLTU")
)

# Define the desired column order
MAPPING_DESIRED_ORDER <- c(
  "STUDYID", "USUBJID", "PARAM", "PCSPEC", "AVAL", "AVALU", "AFRLT", "ARRLT", "NRRLT", "NFRLT",
  "RRLTU", "ROUTE", "DRUG", "DOSEA", "DOSEU", "DOSNO", "ADOSEDUR"
)

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
#' This module provides implementation for mapping columns from a dataset to specific
#' roles required for analysis. It allows users to select columns for various categories such as
#' group identifiers, sample variables, dose variables, time variables, and unit variables.
#'
#' @param id The module ID.
#' @param adnca_data A reactive expression that returns the dataset to be processed.
#' @param on_submit A callback function to be executed when the submit button is clicked.
#'
#' @returns A list containing:
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
data_mapping_ui <- function(id) {
  ns <- NS(id)

  card(
    div(
      class = "data-mapping-container",
      h3("Data Mapping"),
      p(
        "The following columns are required for data analysis. Please ensure each of these columns",
        " has been assigned a corresponding column from your dataset"
      ),
      # Adjusted layout using CSS flexbox
      tags$section(
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
            in the outputs. E.g. Treatment Arm, Age, Sex, Race"
          ),
          div(
            class = "column-mapping-output",
            span("Additional Grouping Variables")
          )
        )
      ),
      tags$section(
        h5("Sample Variables"),
        .column_mapping_widget(ns, "PARAM", "Analyte in character format."),
        .column_mapping_widget(ns, "PCSPEC", "Character format"),
        .column_mapping_widget(ns, "AVAL", "Numeric format.")
      ),
      tags$section(
        h5("Dose Variables"),
        .column_mapping_widget(ns, "DRUG", "Character format."),
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
        h5("Time Variables"),
        .column_mapping_widget(ns, "AFRLT", "Numeric format"),
        .column_mapping_widget(ns, "ARRLT", "Numeric format"),
        .column_mapping_widget(ns, "NFRLT", "Numeric format"),
        .column_mapping_widget(ns, "NRRLT", "Numeric format")
      ),
      tags$section(
        h5("Unit Variables"),
        .column_mapping_widget(ns, "AVALU", "Character format."),
        .column_mapping_widget(ns, "DOSEU", "Character format."),
        .column_mapping_widget(ns, "RRLTU", "Character format.")
      ),
      input_task_button(ns("submit_columns"), "Submit Mapping")
    )
  )
}

data_mapping_server <- function(id, adnca_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rerun_trigger <- reactiveVal(0)
    # Derive input IDs from column_groups
    input_ids <- lapply(MAPPING_COLUMN_GROUPS, \(cols) paste0("select_", cols)) |>
      unlist()

    # Loop through each label and create the renderText outputs
    purrr::walk(MAPPING_DESIRED_ORDER, \(label) {
      output[[paste0("label_", label)]] <- renderText(get_label(LABELS, label, "ADPC"))
    })

    # Populate the static inputs with column names
    observeEvent(adnca_data(), {
      column_names <- names(adnca_data())
      update_selectize_inputs(session, input_ids, column_names, MANUAL_UNITS, MAPPING_DESIRED_ORDER)
    })
    
    observeEvent(input$keep_selected_btn, {

      rerun_trigger(rerun_trigger() + 1)
      removeModal()
    })

    # Observe submit button click and update processed_data
    processed_data <- reactive({
      req(adnca_data())
      req(input$submit_columns || rerun_trigger() > 0)
      
      log_info("Processing data mapping...")
      Sys.sleep(1) # Make this artificially slow to show the loading spinner

      dataset <- adnca_data()
      
      # Get the selected columns
      selected_cols <- sapply(names(MAPPING_COLUMN_GROUPS), function(group) {
        sapply(MAPPING_COLUMN_GROUPS[[group]], function(column) {
          input[[(paste0("select_", column))]]
        })
      }, simplify = FALSE)

      lapply(selected_cols, \(group) {
        purrr::imap(group, \(v, n) paste0("* ", n, " -> ", paste0(v, collapse = ", ")))
      }) %>%
        unlist(use.names = FALSE) %>%
        paste0(collapse = "\n") %>%
        paste0("The following mapping was applied:\n", .) %>%
        log_info()

      # Check for duplicate column selections
      all_selected_columns <- unlist(selected_cols)
      if (any(duplicated(all_selected_columns))) {
        log_error("Duplicate column selection detected.")
        showNotification(
          ui = "Duplicate column selection detected. 
          Please ensure each selection is unique.",
          type = "error",
          duration = 5
        )
        return()
      }

      # Remove "Grouping_Variables" from selected columns to prevent renaming
      selected_cols[["Group Identifiers"]] <- selected_cols[["Group Identifiers"]][
        names(selected_cols[["Group Identifiers"]]) != "Grouping_Variables"
      ]

      # Check for unmapped columns
      if (any(unlist(selected_cols) == "")) {
        log_error("Unmapped columns detected.")
        showNotification(
          ui = "Some required columns are not mapped. Please complete all selections.",
          type = "error",
          duration = 5
        )
        return()
      }

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
      if (input$select_AVALU %in% MANUAL_UNITS$concentration) {
        dataset$AVALU <- input$select_AVALU
      }
      if (input$select_DOSEU %in% MANUAL_UNITS$dose) {
        dataset$DOSEU <- input$select_DOSEU
      }
      if (input$select_RRLTU %in% MANUAL_UNITS$time) {
        dataset$RRLTU <- input$select_RRLTU
      }

      # Reorder columns based on the desired order
      dataset <- dataset %>%
        relocate(all_of(MAPPING_DESIRED_ORDER))

      # Apply labels to the dataset
      dataset <- apply_labels(dataset, LABELS, "ADPC")
      
      # Handle duplicates in the data
      #Check for duplicates
      group_columns <- intersect(colnames(dataset), c("STUDYID", "PCSPEC", "ROUTE", "DRUG"))
      
      df_conc <- format_pkncaconc_data(
        ADNCA = dataset,
        group_columns = c(group_columns, "USUBJID", "PARAM"),
        time_column = "AFRLT",
        route_column = "ROUTE",
        dosno_column = "DOSNO"
      )
      
      df_duplicates <<- df_conc %>%
        group_by(TIME, STUDYID, PCSPEC, DRUG, USUBJID, PARAM) %>%
        filter(n() > 1) %>%
        ungroup() %>%
        mutate(.dup_group = paste(TIME, STUDYID, PCSPEC, DRUG, USUBJID, PARAM, sep = "_"))
      
      if (nrow(df_duplicates) == 0) {
        dataset$DFLAG <- FALSE
        return(dataset)
      }

      # CASE: user resolved duplicates, apply DFLAG
      if (nrow(df_duplicates) > 0) {
  
        if(!is.null(input$keep_selected_btn) && input$keep_selected_btn > 0) {
            # Get selected rows from the reactable
          selected <- getReactableState("duplicate_modal_table", "selected")
          req(length(selected) > 0)
          
          kept <- df_duplicates[selected, , drop = FALSE]
          removed <- anti_join(df_duplicates, kept, by = colnames(kept))
          dataset <- dataset %>%
            mutate(DFLAG = FALSE)
        
          dataset <- dataset %>%
            rows_update(
              removed %>% mutate(DFLAG = TRUE) %>%
                select(names(dataset)),
              by = intersect(names(dataset), names(removed))
            )
            
          return(dataset)
        }
      
      showModal(
        modalDialog(
          title = "Duplicate Rows Detected",
          tagList(
            tags$style(HTML("
        .modal-dialog { width: 90vw !important; max-width: 95vw !important; }
        .modal-body { max-height: 90vh; overflow-y: auto; }
      ")),
            p("The following rows are duplicates based on TIME, STUDYID, PCSPEC, DRUG, USUBJID, PARAM."),
            p("Please select the rows you would like to KEEP. Rows not selected will be removed before continuing."),
            div(
              style = "overflow-x: auto; white-space: nowrap; border: 1px solid #ddd; padding: 1em;",
              reactable::reactableOutput(ns("duplicate_modal_table"))
            )
          ),
          easyClose = FALSE,
          footer = tagList(
            actionButton(ns("keep_selected_btn"), "Keep Selected", class = "btn-primary"),
            modalButton("Cancel")
          ),
          size = "l"
        )
      )

      output$duplicate_modal_table <- reactable::renderReactable({
        group_ids <- unique(df_duplicates$.dup_group)
        color_map <- setNames(rep(c("white", "#e6f2ff"), length.out = length(group_ids)), group_ids)
        
        reactable::reactable(
          df_duplicates,
          columns = list(.dup_group = colDef(show = FALSE)),
          rowStyle = function(index) {
            group <- df_duplicates$.dup_group[index]
            list(background = color_map[[group]])
          },
          selection = "multiple",
          onClick = "select",
          compact = TRUE,
          wrap = FALSE,
          resizable = TRUE
        )
      })
      
      }
      
      # Don't return anything until duplicates are resolved
      return(NULL)
      
    })

    list(
      processed_data = processed_data,
      grouping_variables = reactive(input$select_Grouping_Variables)
    )
  })
}
