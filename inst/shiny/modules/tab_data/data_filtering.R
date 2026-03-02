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
    div(
      p(
        # TODO (Gerardo): We will need to think how to really tell the user more about this
        # This is intended for removing specific rows, but not whole profiles which is done later
        # Removing whole profiles here in multidose studies can lead to issues with dose times
        "Any filters added here will be applied across the whole analysis.",
        style = "text-align: center;"
      ),
      div(
        class = "data-filtering-container",
        div(
          class = "filtered-table-container",
          card(reactable_ui(ns("filtered_data_display")), class = "border-0 shadow-none")
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

data_filtering_server <- function(id, raw_adnca_data, imported_filters = NULL) {
  moduleServer(id, function(input, output, session) {

    # Handle user-provided filters
    filters <- reactiveValues()
    filter_counter <- reactiveVal(0)

    # Hold information about data types and choices for filters.
    filters_metadata <- reactive({
      req(raw_adnca_data())

      lapply(colnames(raw_adnca_data()), function(col) {
        if (is.numeric(raw_adnca_data()[[col]]) && length(unique(raw_adnca_data()[[col]])) > 20) {
          list(type = "numeric")
        } else {
          list(type = "text", choices = sort(unique(raw_adnca_data()[[col]])))
        }
      }) %>%
        setNames(colnames(raw_adnca_data())) %>%
        purrr::keep(~ .x$type == "numeric" || length(.x$choices) > 1)
    })

    # Helper to insert a single filter panel
    .add_filter <- function(initial_values = NULL) {
      filter_counter(filter_counter() + 1)
      accordion_panel_close(id = "filters", values = TRUE)

      filter_id <- paste0("filter_", filter_counter())

      accordion_panel_insert(
        id = "filters",
        panel = input_filter_ui(session$ns(filter_id), names(filters_metadata()))
      )

      accordion_panel_open(id = "filters", value = session$ns(filter_id))

      filters[[filter_id]] <- input_filter_server(
        filter_id,
        filters_metadata = filters_metadata,
        initial_values = initial_values
      )
    }

    observeEvent(input$add_filter, {
      .add_filter()
    })

    # Restore filters from uploaded settings
    observeEvent(list(imported_filters(),filters_metadata()), {
      if (!is.null(imported_filters)) {
        uploaded_filters <- imported_filters()

        # Remove existing filter panels
        for (fid in names(reactiveValuesToList(filters))) {
          removeUI(
            selector = paste0(
              ".accordion-item[data-value='",
              session$ns(fid), "']"
            )
          )
          filters[[fid]] <- NULL
        }
        filter_counter(0)

        # Add each uploaded filter
        for (filt in uploaded_filters) {
          # Validate that the column exists in the data
          if (filt$column %in% names(filters_metadata())) {
            .add_filter(initial_values = filt)
          }
        }

        # Auto-submit after a delay to let inputs initialize
        shinyjs::delay(500, shinyjs::click("submit_filters"))
      }
    })

    #' When filters change, show notification reminding the user about submitting
    filter_reminder_notification <- reactiveVal(NULL)
    observe({
      applied_filters <- lapply(reactiveValuesToList(filters), function(x) x()) %>%
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
        ) %>% filter_reminder_notification()
      }
    })

    filtered_data <- reactive({
      removeNotification(filter_reminder_notification())

      # Extract filters from reactive values
      applied_filters <- lapply(reactiveValuesToList(filters), function(x) x()) %>%
        purrr::keep(\(x) !is.null(x))

      if (length(applied_filters) == 0) return(raw_adnca_data())

      applied_filters %>%
        sapply(\(filt) str_glue("* {filt$column} {filt$condition} {paste0(filt$value, collapse = ', ')}")) %>% # nolint
        paste0(collapse = "\n") %>%
        paste0("Submitting the following filters:\n", .) %>%
        log_info()

      # Save the filters object
      session$userData$applied_filters <- applied_filters

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
    }) %>%
      bindEvent(input$submit_filters, raw_adnca_data())

    reactable_server(
      "filtered_data_display",
      filtered_data,
      height = "50vh"
    )

    filtered_data
  })
}

