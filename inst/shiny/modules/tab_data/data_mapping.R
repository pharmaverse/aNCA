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
  "Group Identifiers" = c("STUDYID", "USUBJID", "NCA_PROFILE", "Grouping_Variables"),
  "Sample Variables" = c("PARAM", "PCSPEC", "ROUTE", "AVAL"),
  "Dose Variables" = c("DRUG", "DOSEA", "ADOSEDUR"),
  "Time Variables" = c("AFRLT", "ARRLT", "NFRLT", "NRRLT"),
  "Unit Variables" = c("AVALU", "DOSEU", "RRLTU")
)

# Define the desired column order
MAPPING_DESIRED_ORDER <- c(
  "STUDYID", "USUBJID", "PARAM", "PCSPEC", "NCA_PROFILE",
  "AVAL", "AVALU", "AFRLT", "ARRLT", "NRRLT", "NFRLT",
  "RRLTU", "ROUTE", "DRUG", "DOSEA", "DOSEU", "ADOSEDUR"
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
#' It also checks for duplicates in the data that will cause problems later, and allows the user
#' to resolve them.
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
#' \item Filtering out concentration duplicates
#' \item Setting a flag for duplicate rows, as selected by the user.
#' }
#' The processed dataset and selected grouping variables are returned as reactive expressions.
data_mapping_ui <- function(id) {
  ns <- NS(id)

  div(
    stepper_ui("Mapping"),
    card(
      div(
        class = "data-mapping-container",
        h3("Data Mapping"),
        p(
          "The following columns are required for data analysis.",
          " Please ensure each of these columns",
          " has been assigned a corresponding column from your dataset"
        ),
        # Adjusted layout using CSS flexbox
        tags$section(
          h5("Group Identifiers"),
          .column_mapping_widget(
            ns, "STUDYID", "Select Corresponding Column, in character format."
          ),
          .column_mapping_widget(
            ns, "USUBJID", "Character or Numeric format"
          ),
          .column_mapping_widget(
            ns,
            "NCA_PROFILE",
            "Select the column you want to use for selecting the NCA profiles."
          ),
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
  )
}

data_mapping_server <- function(id, adnca_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    duplicates <- reactiveVal(NULL)
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

    # Observe submit button click and update processed_data
    mapping <- reactive({
      input_ids <- unlist(lapply(MAPPING_COLUMN_GROUPS, \(cols) paste0("select_", cols)))
      input_ids <- c(input_ids, "select_Grouping_Variables") # Include manually added input
      mapping_list <- setNames(lapply(input_ids, \(id) input[[id]]), input_ids)
      mapping_list
    })

    mapped_data <- reactive({
      req(adnca_data())
      log_info("Processing data mapping...")
      apply_column_mapping(adnca_data(), mapping(),
                           MANUAL_UNITS, MAPPING_COLUMN_GROUPS,
                           MAPPING_DESIRED_ORDER)
    }) |>
      bindEvent(input$submit_columns)

    #Check for blocking duplicates
    # groups based on PKNCAconc formula
    df_duplicates <- reactive({
      req(mapped_data)
      mapped_data() %>%
        group_by(AFRLT, STUDYID, PCSPEC, DRUG, USUBJID, PARAM) %>%
        filter(n() > 1) %>%
        ungroup() %>%
        mutate(.dup_group = paste(AFRLT, STUDYID, PCSPEC, DRUG, USUBJID, PARAM, sep = "_"))
    })

    processed_data <- reactive({
      req(mapped_data())
      dataset <- mapped_data()

      if (nrow(df_duplicates()) == 0) {
        return(mutate(dataset, DFLAG = FALSE))
      }

      # User resolves duplicates, apply DFLAG
      duplicates(df_duplicates())

      if (!is.null(input$keep_selected_btn) && input$keep_selected_btn > 0) {
        # Get selected rows from the reactable
        selected <- getReactableState("duplicate_modal_table", "selected")
        req(length(selected) > 0)

        kept <- df_duplicates()[selected, , drop = FALSE]
        removed <- anti_join(df_duplicates(), kept, by = colnames(kept))
        dataset <- dataset %>%
          mutate(DFLAG = FALSE)

        # Set DFLAG to TRUE for the removed rows
        dataset <- dataset %>%
          rows_update(
            removed %>%
              mutate(DFLAG = TRUE) %>%
              select(names(dataset)),
            by = intersect(names(dataset), names(removed))
          )

        return(dataset)
      }
      # Don't return anything until duplicates are resolved
      return(NULL)

    })

    observeEvent(duplicates(), {
      showModal(
        modalDialog(
          title = "Duplicate Rows Detected",
          class = "modal-duplicates",
          tagList(
            p("The following rows are duplicates based on TIME, STUDYID,
              PCSPEC, DRUG, USUBJID, PARAM."),
            p("Please select the rows you would like to KEEP.
              Rows not selected will be flagged and filtered."),
            p("Alternatively, click 'Cancel' to discard the changes,
              and clean the dataset yourself."),
            div(reactableOutput(ns("duplicate_modal_table")))
          ),
          easyClose = FALSE,
          footer = tagList(
            actionButton(ns("keep_selected_btn"), "Keep Selected", class = "btn-primary"),
            modalButton("Cancel")
          ),
          size = "l"
        )
      )
    })

    observeEvent(input$keep_selected_btn, {
      duplicates(NULL)
      removeModal()
    })

    output$duplicate_modal_table <- renderReactable({
      group_ids <- unique(df_duplicates()$.dup_group)
      color_map <- setNames(rep(c("white", "#e6f2ff"), length.out = length(group_ids)), group_ids)

      reactable(
        df_duplicates(),
        columns = list(.dup_group = colDef(show = FALSE)),
        rowStyle = function(index) {
          group <- df_duplicates()$.dup_group[index]
          list(background = color_map[[group]])
        },
        selection = "multiple",
        onClick = "select",
        compact = TRUE,
        wrap = FALSE,
        resizable = TRUE
      )
    })

    list(
      processed_data = processed_data,
      grouping_variables = reactive(input$select_Grouping_Variables)
    )
  })
}
