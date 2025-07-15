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
          div(
            style = "display: flex; flex-direction: column; gap: 10px;",
            actionButton(ns("add_filter"), "Add Filter", width = "100%"),
            input_task_button(ns("submit_filters"), "Submit Filters", class = "w-100")
          ),
          accordion(id = ns("filters"), class = "filters-inputs")
        )
      )
    )
  )
}

data_filtering_server <- function(id, raw_adnca_data) {
  moduleServer(id, function(input, output, session) {
    # Handle user-provided filters
    filters <- reactiveValues()

    # Hold information about data types and choices for filters.
    filters_metadata <- reactive({
      req(raw_adnca_data())

      lapply(colnames(raw_adnca_data()), function(col) {
        if (is.numeric(raw_adnca_data()[[col]])) {
          list(type = "numeric")
        } else {
          list(type = "text", choices = unique(raw_adnca_data()[[col]]))
        }
      }) |>
        setNames(colnames(raw_adnca_data())) |>
        purrr::keep(~ .x$type == "numeric" || length(.x$choices) > 1)
    })

    observeEvent(input$add_filter, {
      accordion_panel_close(id = "filters", values = TRUE)

      # Create a unique ID for each filter
      filter_id <- paste0("filter_", input$add_filter)

      # Insert a new filter UI
      accordion_panel_insert(
        id = "filters",
        panel = input_filter_ui(session$ns(filter_id), names(filters_metadata()))
      )

      accordion_panel_open(id = "filters", value = session$ns(filter_id))

      filters[[filter_id]] <- input_filter_server(filter_id, filters_metadata = filters_metadata)
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
      removeNotification(filter_reminder_notification())

      # Extract filters from reactive values
      applied_filters <- lapply(reactiveValuesToList(filters), \(x) x()) |>
        purrr::keep(\(x) !is.null(x))

      if (length(applied_filters) == 0) return(raw_adnca_data())

      applied_filters |>
        sapply(\(filt) str_glue("* {filt$column} {filt$condition} {paste0(filt$value, collapse = ', ')}")) |> # nolint
        paste0(collapse = "\n") %>%
        paste0("Submitting the following filters:\n", .) %>%
        log_info()

      # Filter and return data
      withCallingHandlers({
        apply_filters(raw_adnca_data(), applied_filters)
      }, warning = function(w) {
        log_warn(conditionMessage(w))
        showNotification(
          paste0("Warning during filtering: ", conditionMessage(w)),
          type = "warning",
          duration = 10
        )
        invokeRestart("muffleWarning")
      })
    }) |>
      bindEvent(input$submit_filters, raw_adnca_data())

    output$filtered_data_display <- renderReactable({
      req(filtered_data())
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

    filtered_data
  })
}
