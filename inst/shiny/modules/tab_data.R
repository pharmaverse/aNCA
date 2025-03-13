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
        data_mapping_ui(ns("column_mapping"))
      ),
      nav_panel("Review Data",
        id = ns("data_navset-review"),
        card(uiOutput(ns("reviewDataContent")))
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

    observe(print(adnca_filtered()))

    # Call the column mapping module
    column_mapping <- data_mapping_server(
      id = "column_mapping",
      adnca_data = adnca_filtered,
      on_submit = \() updateTabsetPanel(session, "data_navset", selected = "Review Data")
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

    #' Reactive value for the processed dataset
    processed_data <- column_mapping$processed_data

    #' Global variable to store grouping variables
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
