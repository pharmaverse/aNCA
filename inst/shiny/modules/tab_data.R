#' The user can upload a new data set by clicking the "Browse" button.
#' The data set must be in CSV format and contain the following columns:
#' STUDYID, USUBJID, ANALYTE, PCSPEC, DOSEFRQ, DOSNO, AFRLT, ARRLT, NRRLT, NFRLT,
#' AVAL, AVALU, ROUTE, DOSEA, AGE

source(system.file("/shiny/modules/input_filter.R", package = "aNCA"))
source(system.file("/shiny/modules/column_mapping.R", package = "aNCA"))

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
        column_mapping_ui(ns("column_mapping"))
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

    # Column Mapping ----

    # Define the manual units for concentration, dose, and time in a format recognized by PKNCA
    manual_units <- list(
      concentration = c("mg/L", "µg/mL", "ng/mL", "pg/mL",
                        "mol/L", "mmol/L", "µmol/L", "nmol/L",
                        "pmol/L", "mg/dL", "µg/dL", "ng/dL"),
      dose = c("mg", "g", "µg", "ng", "pg", "mol", "mmol",
               "µmol", "nmol", "pmol", "mg/kg", "g/kg",
               "µg/kg", "ng/kg", "pg/kg", "mol/kg", "mmol/kg",
               "µmol/kg", "nmol/kg", "pmol/kg"),
      time = c("sec", "min", "hr", "day", "week", "month", "year")
    )

    # Define the callback function to change the tab
    change_to_review_tab <- function() {
      updateTabsetPanel(session, "data_navset", selected = "Review Data")
    }

    # Call the column mapping module
    column_mapping <- column_mapping_server(
      id = "column_mapping",
      data = ADNCA,
      manual_units = manual_units,
      on_submit = change_to_review_tab
    )

    # Reactive value for the processed dataset
    processed_data <- column_mapping$processed_data

    # Global variable to store grouping variables
    grouping_variables <- column_mapping$grouping_variables

    # Handle user-provided filters
    filters <- reactiveValues()

    observeEvent(input$add_filter, {
      # Create a unique ID for each filter
      filter_id <- paste0("filter_", input$add_filter)

      # Insert a new filter UI
      insertUI(
        selector = paste0("#", session$ns("filters")),
        ui = input_filter_ui(session$ns(filter_id), colnames(processed_data()))
      )

      filters[[filter_id]] <- input_filter_server(filter_id)
    })

    # Create reactive value with applied filters
    data <- reactiveVal(NULL)
    observeEvent(list(input$submit_filters, processed_data()), {
      # Extract filters from reactive values
      applied_filters <- lapply(reactiveValuesToList(filters), \(x) x())

      # Filter and overwrite data
      filtered_data <- apply_filters(
        processed_data(), applied_filters
      )
      data(filtered_data)
    }, ignoreInit = FALSE)

    # Update the data table object with the filtered data
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