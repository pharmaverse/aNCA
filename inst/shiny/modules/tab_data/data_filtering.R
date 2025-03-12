#' Module responsible for

data_filtering_ui <- function(id) {
  ns <- NS(id)

  card(
    div(
      h3("Filters"),
      p("
        Click the 'Add Filters' button to add filters to your data.
        Be sure to click 'Submit' in order to apply the changes.\n
        Any filters added here will be applied across the whole analysis.
      "),
      div(
        style = "display: grid; grid-template-columns: 49% 49%; gap: 1%;",
        actionButton(ns("add_filter"), "Add Filter"),
        input_task_button(ns("submit_filters"), "Submit Filters")
      ),
      div(id = ns("filters")),
      reactableOutput(ns("filtered_data_display"))
    )

  )

}

data_filtering_server <- function(id, raw_adnca_data, processed_data) {
  moduleServer(id, function(input, output, session) {
    # Handle user-provided filters
    filters <- reactiveValues()

    observeEvent(input$add_filter, {
      # Create a unique ID for each filter
      filter_id <- paste0("filter_", input$add_filter)

      # Insert a new filter UI
      insertUI(
        selector = paste0("#", session$ns("filters")),
        ui = input_filter_ui(session$ns(filter_id), colnames(raw_adnca_data()))
      )

      filters[[filter_id]] <- input_filter_server(filter_id)
    })

    filtered_data <- reactive({
      # Extract filters from reactive values
      applied_filters <- lapply(reactiveValuesToList(filters), \(x) x())

      # Filter and return data
      apply_filters(raw_adnca_data(), applied_filters)
    }) |>
      bindEvent(input$submit_filters, raw_adnca_data(), processed_data())

    output$filtered_data_display <- renderReactable({
      req(filtered_data())
      reactable(
        filtered_data(),
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

    filtered_data
  })
}
