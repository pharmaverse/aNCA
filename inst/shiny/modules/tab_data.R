#' The user can upload a new data set by clicking the "Browse" button.
#' The data set must be in CSV format and contain the following columns:
#' STUDYID, USUBJID, ANALYTE, PCSPEC, DOSEFRQ, DOSNO, AFRLT, ARRLT, NRRLT, NFRLT,
#' AVAL, AVALU, ROUTE, DOSEA, AGE

source(system.file("/shiny/modules/input_filter.R", package = "aNCA"))

tab_data_ui <- function(id) {
  ns <- NS(id)

  navset_pill(
    id = ns("data_navset"),
    nav_panel("Raw Data Upload",
      card(
        "Upload your PK dataset in .csv format",
        # Local upload option
        fileInput(
                  ns("local_upload"),
                  width = "60%",
                  label = NULL,
                  placeholder = ".csv",
                  buttonLabel = list(icon("folder"), "Upload File..."),
                  accept = c(".csv", ".rds"))
      ),
      reactableOutput(ns("filecontents"))
    ),
    nav_panel("Mapping and Filters",
              card(
                h3("Data Mapping"),
                br(),
                "The following columns are required for data analysis.
    Please ensure each of these columns has been assigned
    a corresponding column from your dataset",
                br(),
                fluidRow(
                  h4("Group Identifiers"),
                  column(12,
                  tooltip(
                    selectizeInput(ns("select_STUDYID"), "STUDYID",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Select Study ID Column"
                  ),
                  tooltip(
                    selectizeInput(ns("select_USUBJID"), "USUBJID",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Unique subject identifier."
                  ),
                  tooltip(
                    selectizeInput(ns("select_Grouping_Variables"), "Grouping Variables",
                                   choices = NULL, multiple = TRUE, options = list(placeholder = "Select Column(s)")),
                    "Select the additional column(s) that will be used to group the data 
                    for tables, listings and graphs. E.g. Treatment Arm, Age, Sex, Race"
                  )
                  )
                ),
                fluidRow(
                  h4("Sample Variables"),
                  column(12,
                  tooltip(
                    selectizeInput(ns("select_ANALYTE"), "ANALYTE",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Analyte"
                  ),
                  tooltip(
                    selectizeInput(ns("select_PCSPEC"), "PCSPEC",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Matrix"
                  ),
                  tooltip(
                    selectizeInput(ns("select_ROUTE"), "ROUTE",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Route of administration, stating either 'intravascular' or 'extravascular'."
                  ),
                  tooltip(
                    selectizeInput(ns("select_AVAL"), "AVAL",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Analysis value in numeric format."
                  )
                  )
                ),
                fluidRow(
                  h4("Dose Variables"),
                  column(12,
                  tooltip(
                    selectizeInput(ns("select_DOSNO"), "DOSNO",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Numeric format."
                  ),
                  tooltip(
                    selectizeInput(ns("select_DOSEA"), "DOSEA",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Actual Dose amount in numeric format."
                  ),
                  tooltip(
                    selectizeInput(ns("select_ADOSEDUR"), "ADOSEDUR",
                                   choices = c("Select Column" = "", "NA"),
                                   options = list(placeholder = "Select Column")),
                    "Duration of dose administration. Only required for infusion studies, otherwise select NA"
                  )
                  )
                ),
                fluidRow(
                  h4("Time Variables"),
                  column(12,
                  tooltip(
                    selectizeInput(ns("select_AFRLT"), "AFRLT",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Numeric format"
                  ),
                  tooltip(
                    selectizeInput(ns("select_ARRLT"), "ARRLT",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Numeric format"
                  ),
                  tooltip(
                    selectizeInput(ns("select_NFRLT"), "NFRLT",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Numeric format"
                  ),
                  tooltip(
                    selectizeInput(ns("select_NRRLT"), "NRRLT",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Numeric format"
                  )
                  )
                ),
                fluidRow(
                  h4("Unit Variables"),
                  column(12,
                  tooltip(
                    selectizeInput(ns("select_AVALU"), "AVALU",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Unit of analysis value."
                  ),
                  tooltip(
                    selectizeInput(ns("select_DOSEU"), "DOSEU",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Unit of dose amount."
                  ),
                  tooltip(
                    selectizeInput(ns("select_RRLTU"), "RRLTU",
                                   choices = NULL, options = list(placeholder = "Select Column")),
                    "Unit of time."
                  )
                  )
                ),
                input_task_button(ns("submit_columns"), "Submit Mapping")
              ),
              card(
                # Add filter UI elements
                actionButton(ns("add_filter"), "Add filter"),
                tags$div(id = ns("filters")),
                actionButton(ns("submit_filters"), "Submit filters"),
              )
    ),
    nav_panel("Review Data",
      "This is the data set that will be used for the analysis.
      If you want to make any changes, please do so in the Mapping and Filters tab.",
      reactableOutput(ns("data_processed"))
    )
  )

}

tab_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # DATA LOADING -----------------------------------------------------------------
    # Load the dummy ADNCA example for the user as default
    ADNCA <- reactiveVal(
      read.csv(
        system.file("shiny/data/DummyRO_ADNCA.csv", package = "aNCA"),
        na.strings = c("", "NA")
      )
    )

    # Load data provided by user
    observeEvent(input$local_upload, {
      new_adnca <- switch(
        file_ext(input$local_upload$name),
        csv = read.csv(input$local_upload$datapath, na = c("", "NA")),
        rds = readRDS(input$local_upload$datapath),
        validate("Invalid file type. Only accepted are .csv and .rds")
      )

      ADNCA(new_adnca)
    })

    output$filecontents <- renderReactable({
      req(ADNCA())
      reactable(
        ADNCA(),
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        bordered = TRUE,
        height = "98vh"
      )
    })
    
    # Define the required columns and group them into categories
    column_groups <- list(
      "Group Identifiers" = c("STUDYID", "USUBJID", "Grouping Variables"),
      "Sample Variables" = c("ANALYTE", "PCSPEC", "ROUTE", "AVAL"),
      "Dose Variables" = c("DOSNO", "DOSEA", "ADOSEDUR"),
      "Time Variables" = c("AFRLT", "ARRLT", "NFRLT", "NRRLT"),
      "Unit Variables" = c("AVALU", "DOSEU", "RRLTU")
    )
    
    # Define the desired column order
    desired_order <- c("STUDYID", "USUBJID", "ANALYTE",
                       "PCSPEC", "AVAL", "AVALU", "AFRLT",
                       "ARRLT", "NRRLT", "NFRLT", "RRLTU",
                       "ROUTE", "DOSEA", "DOSEU", "DOSNO")
    
    # Populate the static inputs with column names
    observe({
      data <- ADNCA()
      column_names <- names(data)
      
      input_ids <- c("select_STUDYID", "select_USUBJID", "select_Grouping_Variables",
                     "select_ANALYTE", "select_PCSPEC", "select_ROUTE", "select_AVAL",
                     "select_DOSNO", "select_DOSEA", "select_ADOSEDUR", "select_AFRLT",
                     "select_ARRLT", "select_NFRLT", "select_NRRLT", "select_AVALU",
                     "select_DOSEU", "select_RRLTU")
      
      # Exclude columns specified in desired_order from the choices for "Grouping Variables"
      grouping_variable_choices <- setdiff(column_names, desired_order)
      
      for (input_id in input_ids) {
        column_name <- sub("select_", "", input_id)
        selected_value <- if (column_name %in% column_names) column_name else NULL
        updateSelectizeInput(session, input_id, choices = c("Select Column" = "", column_names), selected = selected_value)
      }
      
      # Special case for ADOSEDUR to include "NA"
      updateSelectizeInput(session, "select_ADOSEDUR", choices = c("Select Column" = "", column_names, "NA"))
      
      # Special case for Grouping Variables
      updateSelectizeInput(session, "select_Grouping_Variables", choices = grouping_variable_choices)
    })

    # Global variable to store grouping variables
    grouping_variables <- reactiveVal(NULL)

    # Reactive value for the processed dataset
    processed_data <- reactiveVal(NULL)

    # Observe submit button click and update processed_data
    observeEvent(input$submit_columns, {
      Sys.sleep(1)  # Make this artificially slow
      req(ADNCA())
      data <- ADNCA()

      # Get the selected columns
      selected_cols <- sapply(names(column_groups), function(group) {
        sapply(column_groups[[group]], function(column) {
          input[[(paste0("select_", column))]]
        })
      }, simplify = FALSE)

      # Extract and store the "Grouping Variables" column
      grouping_variables(selected_cols[["Group Identifiers"]][["Grouping Variables"]])

      # Remove "Grouping Variables" from selected columns to prevent renaming
      selected_cols[["Group Identifiers"]] <- selected_cols[["Group Identifiers"]][
        names(selected_cols[["Group Identifiers"]]) != "Grouping Variables"
      ]

      # Rename columns
      colnames(data) <- sapply(colnames(data), function(col) {
        for (group in names(selected_cols)) {
          if (col %in% selected_cols[[group]]) {
            return(names(selected_cols[[group]])[which(selected_cols[[group]] == col)])
          }
        }
        return(col)
      })

      # Reorder columns based on the desired order
      ordered_data <- data[, c(desired_order, setdiff(names(data), desired_order))] %>%
        mutate(TIME = ifelse(DOSNO == 1, AFRLT, ARRLT))#TODO: Remove this after auc0 merged
      processed_data(ordered_data)
      
      # Navigate to the "Review Data" tab
      nav_select("data_navset", "Review Data")
    })

    # Handle user-provided filters
    filters <- reactiveValues()

    observeEvent(input$add_filter, {
      # Create a unique ID for each filter
      filter_id <- paste0("filter_", input$add_filter)

      # Insert a new filter UI
      insertUI(
        selector = paste0("#", session$ns("filters")),
        ui = input_filter_ui(session$ns(filter_id), colnames(ADNCA()))
      )

      filters[[filter_id]] <- input_filter_server(filter_id)
    })

    # create reactive value with applied filters
    data <- reactiveVal(NULL)
    observeEvent(list(input$submit_filters, processed_data()), {
      # extract filters from reactive #
      applied_filters <- lapply(reactiveValuesToList(filters), \(x) x())

      # filter and overwrite data #
      filtered_data <- apply_filters(
        processed_data(), applied_filters
      )
      data(filtered_data)
    }, ignoreInit = FALSE)

    # update the data table object with the filtered data #
    output$data_processed <- renderReactable({
      req(data())
      reactable(
        data(),
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        bordered = TRUE,
        height = "98vh"
      )
    })

    return(list(
      data = data,
      grouping_variables = grouping_variables
    ))
  })
}