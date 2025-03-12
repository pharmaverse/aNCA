#' The user can upload a new data set by clicking the "Browse" button.
#' The data set must be in CSV format and contain the following columns:
#' STUDYID, USUBJID, ANALYTE, PCSPEC, DOSEFRQ, DOSNO, AFRLT, ARRLT, NRRLT, NFRLT,
#' AVAL, AVALU, ROUTE, DOSEA, AGE

tab_data_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "data-tab-container",
    navset_pill(
      id = ns("data_navset"),
      nav_panel("Raw Data Upload",
        data_upload_ui(ns("raw_data")),
        data_filtering_ui(ns("data_filtering"))
      ),
      nav_panel("Column Mapping",
        column_mapping_ui(ns("column_mapping"))
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
  )
}

tab_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #' Load raw ADNCA data
    adnca_raw <- data_upload_server("raw_data")

    #' Filter data
    adnca_filtered <- data_filtering_server("data_filtering", adnca_raw, processed_data)

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
      data = adnca_filtered,
      manual_units = manual_units,
      on_submit = change_to_review_tab
    )

    output$reviewDataContent <- renderUI({
      if (!is.null(processed_data()) && nrow(processed_data()) > 0) {
        tagList(
          "This is the data set that will be used for the analysis.
          If you would like to make any changes please return to the previous tabs.",
          reactableOutput(ns("data_processed"))
        )
      } else {
        div(
          "Please map your data in the 'Column Mapping' section before reviewing it."
        )
      }
    })

    # Reactive value for the processed dataset
    processed_data <- column_mapping$processed_data

    # Global variable to store grouping variables
    grouping_variables <- column_mapping$grouping_variables

    # Update the data table object with the filtered data
    output$data_processed <- renderReactable({
      req(processed_data())

      # Generate column definitions
      col_defs <- generate_col_defs(processed_data())

      reactable(
        processed_data(),
        columns = col_defs,
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = TRUE,
        resizable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        bordered = TRUE,
        height = "98vh"
      )
    })

    list(
      data = processed_data,
      grouping_variables = grouping_variables
    )
  })
}
