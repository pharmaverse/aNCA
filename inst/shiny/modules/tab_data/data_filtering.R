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
    card(
      div(
        h3("Filters"),
        p("
          Click the 'Add Filters' button to add filters to your data.
          Be sure to click 'Submit' in order to apply the changes.\n
          Any filters added here will be applied across the whole analysis.
        "),
        div(
          class = "filters-buttons-container",
          actionButton(ns("add_filter"), "Add Filter"),
          input_task_button(ns("submit_filters"), "Submit Filters")
        ),
        div(id = ns("filters")), # container for filter widgets inserted in the server part
        reactableOutput(ns("filtered_data_display"))
      )

    )
  )
}

data_filtering_server <- function(id, raw_adnca_data) {
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

    #' When filters change, show notification reminding the user about submitting
    filter_reminder_notification <- reactiveVal(NULL)
    observe({
      applied_filters <- lapply(reactiveValuesToList(filters), \(x) x()) |>
        purrr::keep(\(x) !is.null(x))
      if (length(applied_filters) == 0) {
        removeNotification(filter_reminder_notification())
      } else {
        showNotification(
          "Remember to submit filters before continuing.",
          id = session$ns("filter_submit_reminder"),
          duration = 0,
          closeButton = FALSE,
          type = "message"
        ) |> filter_reminder_notification()
      }
    })

    filtered_data <- reactive({
      # Extract filters from reactive values
      applied_filters <- lapply(reactiveValuesToList(filters), \(x) x())

      if (length(applied_filters) != 0) {
        applied_filters |>
          purrr::keep(\(x) !is.null(x)) |>
          sapply(\(filt) str_glue("* {filt$column} {filt$condition} {filt$value}")) |>
          paste0(collapse = "\n") %>%
          paste0("Submitting the following filters:\n", .) %>%
          log_info()
      }

      removeNotification(filter_reminder_notification())

      # Filter and return data
      apply_filters(raw_adnca_data(), applied_filters)
    }) |>
      bindEvent(input$submit_filters, raw_adnca_data())

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
