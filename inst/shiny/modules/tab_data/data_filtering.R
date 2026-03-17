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

#' Insert a filter panel into the accordion and start its server module.
#' @param session Shiny session.
#' @param filters ReactiveValues storing filter modules.
#' @param filter_counter ReactiveVal with current filter count.
#' @param filters_metadata Reactive with column metadata.
#' @param initial_values Optional list with column, condition, value to restore.
#' @noRd
.insert_filter_panel <- function(session, filters, filter_counter,
                                 filters_metadata, initial_values = NULL) {
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

#' Remove all existing filter panels and reset the counter.
#' @param session Shiny session.
#' @param filters ReactiveValues storing filter modules.
#' @param filter_counter ReactiveVal with current filter count.
#' @noRd
.clear_filter_panels <- function(session, filters, filter_counter) {
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
}

#' Schedule a callback after n flush cycles.
#' @param session Shiny session.
#' @param n Number of flush cycles to wait.
#' @param callback Function to call after n cycles.
#' @noRd
.delay_flush <- function(session, n, callback) {
  if (n <= 0L) {
    callback()
  } else {
    session$onFlushed(function() .delay_flush(session, n - 1L, callback))
  }
}

data_filtering_server <- function(id, raw_adnca_data, imported_filters) {
  moduleServer(id, function(input, output, session) {
    filters <- reactiveValues()
    filter_counter <- reactiveVal(0)

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

    observeEvent(input$add_filter, {
      .insert_filter_panel(session, filters, filter_counter, filters_metadata)
    })

    # Restore filters from uploaded settings.
    # pending_import stores filters until metadata is ready, avoiding a
    # nested observe to wait for filters_metadata().
    pending_import <- reactiveVal(NULL)

    observeEvent(imported_filters(), {
      req(imported_filters())
      pending_import(imported_filters())
    })

    # Fires when pending_import is set or when filters_metadata becomes
    # available. Clears pending_import after restoring to prevent re-runs.
    observe({
      req(pending_import(), filters_metadata())

      filters_to_restore <- isolate(pending_import())
      pending_import(NULL)

      .clear_filter_panels(session, filters, filter_counter)

      has_filters <- FALSE
      for (filt in filters_to_restore) {
        if (filt$column %in% names(filters_metadata())) {
          .insert_filter_panel(
            session, filters, filter_counter, filters_metadata, filt
          )
          has_filters <- TRUE
        }
      }

      if (has_filters) {
        # Each filter module needs two flush cycles to restore its
        # inputs (column, then condition/value). Delay the submit
        # click until those cycles complete.
        .delay_flush(session, 3L, function() {
          shinyjs::click("submit_filters")
        })
      }
    })

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

      applied_filters <- lapply(reactiveValuesToList(filters), function(x) x()) %>%
        purrr::keep(\(x) !is.null(x))

      if (length(applied_filters) == 0) return(raw_adnca_data())

      applied_filters %>%
        sapply(\(filt) str_glue("* {filt$column} {filt$condition} {paste0(filt$value, collapse = ', ')}")) %>% # nolint
        paste0(collapse = "\n") %>%
        paste0("Submitting the following filters:\n", .) %>%
        log_info()

      session$userData$applied_filters <- applied_filters

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
