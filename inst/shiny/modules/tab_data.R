#' Module handling pre-processing of data.
#'
#' @details
#' Handles user upload or dummy data, filtering, mapping and reviewing. The general pipeline for
#' the module is:
#' 1. Upload raw adnca data (or return a dummy dataset)
#' 2. Filter the raw data based on user input
#' 3. Process the data based on column mapping specifications
#' The module also allows the user to review the data after performing filtering and mapping -
#' the processed data will go further into the analysis pipeline.
#'
#' @returns list containing two reactives:
#'            - data -> processed data for further analysis
#'            - grouping_variables -> grouping variables specified by the user.

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
        card(
          uiOutput(ns("processed_data_message")),
          reactableOutput(ns("data_processed"))
        )
      )
    ),
    nav_panel("Data Imputation",
              # ToDo: Missing data tab imputation
              card(
                h4("Missing data imputation"),
              ),
              
              card(
                # Add BLQ imputation UI elements
                h4("BLQ imputation"),
                blq_imputation_ui(ns("blq_imputation"))
              )
    ),
    nav_panel("Review Data",
      "This is the data set that will be used for the analysis.
      If you want to make any changes, please do so in the Mapping and Filters tab.",
      reactableOutput(ns("data_processed")),
      tags$script(HTML("
        $(document).ready(function(){
          $('[data-toggle=\"tooltip\"]').tooltip(); 
        });
      "))
    )
  )
}

tab_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #' Load raw ADNCA data
    adnca_raw <- data_upload_server("raw_data")

    #' Filter data
    adnca_filtered <- data_filtering_server("data_filtering", adnca_raw)

    # Call the column mapping module
    column_mapping <- data_mapping_server(
      id = "column_mapping",
      adnca_data = adnca_filtered
    )

    #' Reactive value for the processed dataset
    processed_data <- column_mapping$processed_data
    observeEvent(processed_data(), {
      req(processed_data())
      updateTabsetPanel(session, "data_navset", selected = "Review Data")
    })

    #' Global variable to store grouping variables
    grouping_variables <- column_mapping$grouping_variables

    output$processed_data_message <- renderUI({
      tryCatch({
        req(processed_data())
        div(
          "This is the data set that will be used for the analysis.
          If you would like to make any changes please return to the previous tabs."
        )
      },
      error = function(e) {
        div("Please map your data in the 'Column Mapping' section before reviewing it.")
      })
    })

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

    # Call the BLQ imputation module
    blq_rule <- blq_imputation_server("blq_imputation")

    observeEvent(blq_rule(), {
      req(blq_rule())
      
      updated_data <- processed_data() %>%
        mutate(original_order = 1:n()) %>% 
        group_by(across(any_of(c("STUDYID", "USUBJID", "DOSNO",
                                 "PCSPEC", "ROUTE", grouping_variables())))) %>%
        arrange(ARRLT) %>%
        mutate(AVAL = PKNCA::clean.conc.blq(AVAL, ARRLT, conc.blq = blq_rule())) %>%
        ungroup() %>% 
        arrange(original_order) %>% 
        select(-original_order)
      
      data(updated_data)
    })

    list(
      data = processed_data,
      grouping_variables = grouping_variables
    )
  })
}
