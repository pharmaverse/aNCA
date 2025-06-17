#' Module handling raw data filtering.
#'
#' @details
#' Module handles filtering UI. The user can add new filtering rules, modify their parameters,
#' and submit the filters when ready. The module filters provided raw data based on those
#' specifications.
#'
#' @param id ID of the module.
#' @param raw_adnca_data Reactive with raw adnca data as uploaded by the user (or dummy dataset).
#'
#' @returns A reactive with filtered adnca data.

data_filtering_ui <- function(id) {
  ns <- NS(id)

  div(
    stepper_ui("Filtering"),
    div(
      h3("Filtering"),
      p("Any filters added here will be applied across the whole analysis."),
      div(
        class = "data-filtering-container",
        div(
          class = "filtered-table-container",
          withSpinner(
            reactableOutput(ns("filtered_data_display"))
          )
        ),
        div(
          class = "filters-container",
          uiOutput(ns("filters"))
        )
      )
    )
  )
}

data_filtering_server <- function(id, raw_adnca_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render filters UI
    output$filters <- renderUI({
      req(raw_adnca_data())
      df <- raw_adnca_data()
      lapply(names(df), function(var) generate_filter_ui(var, df[[var]], ns))
    })

    # Reactive: filtered data
    filtered_data <- reactive({
      req(raw_adnca_data())
      df <- raw_adnca_data()
      for (var in names(df)) {
        df <- apply_filter(df, var, input)
      }
      df
    })

    # Render filtered table
    output$filtered_data_display <- renderReactable({
      req(filtered_data())
      wait_for_all_filter_inputs(raw_adnca_data(), input)

      reactable(
        filtered_data(),
        rownames = FALSE,
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        height = "70vh",
        class = "reactable-table"
      )
    })

    # Output
    filtered_data
  })
}
