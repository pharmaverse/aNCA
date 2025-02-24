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
                "Upload your PK dataset in .csv or .rds format",
                # Local upload option
                fileInput(
                  ns("local_upload"),
                  width = "60%",
                  label = NULL,
                  placeholder = ".csv / .rds",
                  buttonLabel = list(icon("folder"), "Upload File..."),
                  accept = c(".csv", ".rds")
                )
              ),
              card(
                div(
                  class = "card-container",
                  h3("Filters"),
                  p("Click the 'Add Filters' button to add filters to your data.
            Be sure to click 'Submit' in order to apply the changes.\n
          Any filters added here will be applied across the whole analysis."),
                  actionButton(ns("add_filter"), "Add Filter"),
                  tags$div(id = ns("filters")),
                  div(
                    class = "filters-submit-button",
                    input_task_button(ns("submit_filters"), "Submit Filters")
                  )
                )
              ),
              reactableOutput(ns("filecontents"))
    ),
    nav_panel("Mapping and Filters",
              layout_columns(
                card(
                  column_mapping_ui(ns("column_mapping"))
                )
              )
    ),
    nav_panel("Review Data",
              id = ns("data_navset-review"),
              uiOutput(ns("reviewDataContent")),
              tags$script(HTML("
      $(document).ready(function(){
      $('[data-toggle=\"tooltip\"]').tooltip();
      });
                    ")
              )
    )
  )
  
}

tab_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    ADNCA <- reactiveVal(
      read.csv(
        system.file("shiny/data/Dummy_complex_data.csv", package = "aNCA"),
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

    ADNCA_filt <- reactiveVal(NULL)
    filters <- reactiveValues()

    # Render raw data table
    output$filecontents <- renderReactable({
      req(ADNCA_filt())
      reactable(
        ADNCA_filt(),
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

    # Handle user-provided filters
    observeEvent(input$add_filter, {
      filter_id <- paste0("filter_", input$add_filter)
      insertUI(
        selector = paste0("#", session$ns("filters")),
        ui = input_filter_ui(session$ns(filter_id), colnames(ADNCA()))
      )
      filters[[filter_id]] <- input_filter_server(filter_id)
    })

    # Apply filters to data
    observeEvent(list(input$submit_filters, ADNCA()), {
      applied_filters <- lapply(reactiveValuesToList(filters), \(x) x())
      filtered_data <- apply_filters(ADNCA(), applied_filters)
      ADNCA_filt(filtered_data)
    }, ignoreInit = FALSE)
    
    # Column Mapping
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

    change_to_review_tab <- function() {
      updateTabsetPanel(session, "data_navset", selected = "Review Data")
    }

    column_mapping <- column_mapping_server(
      id = "column_mapping",
      data = ADNCA,
      manual_units = manual_units,
      on_submit = change_to_review_tab
    )

    processed_data <- column_mapping$processed_data
    grouping_variables <- column_mapping$grouping_variables
    
    # Render processed data table
    output$data_processed <- renderReactable({
      req(processed_data())
      col_defs <- generate_col_defs(processed_data())
      reactable(
        processed_data(),
        columns = col_defs,
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

    # Render review data content
    output$reviewDataContent <- renderUI({
      if (!is.null(processed_data()) && nrow(processed_data()) > 0) {
        tagList(
          "This is the data set that will be used for the analysis.
          If you would like to make any changes please return to the 'Mapping and Filters' tab.",
          reactableOutput(ns("data_processed"))
        )
      } else {
        div(
          "Please map your data in the 'Mapping' section before reviewing it."
        )
      }
    })

    list(
      data = processed_data,
      grouping_variables = grouping_variables
    )
  })
}
