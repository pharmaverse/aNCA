#' Create a Filter UI Component
#'
#' This module generates a UI component for creating a filter in a Shiny application.
#' The filter allows users to select a column, condition, and value to filter a dataset.
#'
#' @param id   A unique identifier for the filter.
#' @param cols Character vector containing column names to display for selection.
#'
#' @returns A reactive with the filter specification, a list containing provided:
#'            - column
#'            - condition
#'            - value
#'
#' @details
#' The function creates a UI component with the following elements:
#' \itemize{
#'   \item A horizontal rule (`<hr>`).
#'   \item A dropdown menu for selecting a column from the dataset.
#'   \item A dropdown menu for selecting a condition (e.g., `==`, `>`, `<`, `>=`, `<=`, `!=`).
#'   \item A text input for specifying the filter value.
#'   \item A remove button to remove the filter.
#' }

input_filter_ui <- function(id, cols) {
  ns <- NS(id)

  accordion_panel(
    title = textOutput(ns("title"), inline = TRUE),
    value = id,
    class = "filter-widget-container",
    selectizeInput(
      ns("column"),
      "",
      choices = cols,
      width = "100%",
      options = list(dropdownParent = "body")
    ),
    selectizeInput(
      ns("condition"),
      "",
      choices = c("==", ">", "<", ">=", "<=", "!=", "min-max"),
      width = "100%",
      options = list(dropdownParent = "body")
    ),
    textInput(ns("value_text"), "", width = "100%"),
    pickerInput(
      ns("value_select"),
      "",
      choices = NULL,
      width = "100%",
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        container = "body",
        `dropup-auto` = FALSE
      )
    ),
    actionButton(ns("remove"), "X", class = "btn-danger")
  )
}

input_filter_server <- function(id, filters_metadata, initial_values = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_active <- reactiveVal(TRUE)
    # Count of observeEvent(input$column) firings to skip.
    # Restore triggers column changes that must not apply defaults.
    skip_column_events <- reactiveVal(0L)
    is_numeric <- reactive({
      req(input$column)
      filters_metadata()[[input$column]]$type == "numeric"
    })

    if (!is.null(initial_values)) {
      # Skip: (1) default column from UI init, (2) restored column update
      isolate(skip_column_events(2L))

      session$onFlushed(function() {
        updateSelectizeInput(
          session, "column", selected = initial_values$column
        )
        session$onFlushed(function() {
          meta <- isolate(filters_metadata())
          col_numeric <- meta[[initial_values$column]]$type == "numeric"
          updateSelectInput(
            session, "condition", selected = initial_values$condition
          )
          if (col_numeric) {
            updateTextInput(
              session, "value_text", value = initial_values$value
            )
            
            shinyjs::show(id = ns("value_text"), asis = TRUE)
            shinyjs::hide(id = ns("value_select"), asis = TRUE)
            shinyjs::enable(id = ns("condition"), asis = TRUE)
          } else {
            updatePickerInput(
              session,
              "value_select",
              choices = meta[[initial_values$column]]$choices,
              selected = initial_values$value
            )
            shinyjs::hide(id = ns("value_text"), asis = TRUE)
            shinyjs::show(id = ns("value_select"), asis = TRUE)
            shinyjs::disable(id = ns("condition"), asis = TRUE)
          }
        })
      })
    }

    observeEvent(input$column, {
      remaining <- isolate(skip_column_events())
      if (remaining > 0L) {
        isolate(skip_column_events(remaining - 1L))
        return()
      }

      if (!is_numeric()) {
        updateSelectInput(session, "condition", selected = "==")
        updatePickerInput(
          session,
          "value_select",
          choices = filters_metadata()[[input$column]]$choices,
          selected = filters_metadata()[[input$column]]$choices
        )
      }

      shinyjs::toggleElement("value_text", condition = is_numeric())
      shinyjs::toggleElement("value_select", condition = !is_numeric())
      shinyjs::toggleState("condition", is_numeric())
    })

    output$title <- renderText(paste("Filter: ", input$column))

    # update filter list #
    applied_filters <- reactive({
      req(!is.null(is_numeric()), input$column)
      # check if filter was not removed #
      if (is_active()) {
        list(
          column = input$column,
          condition = input$condition,
          value = if (is_numeric()) input$value_text else input$value_select
        )
      } else {
        # if filter was removed, return NULL #
        NULL
      }
    })

    # remove filter #
    observeEvent(input$remove, {
      removeUI(selector = paste0(".accordion-item[data-value='", session$ns(NULL), "']"))
      is_active(FALSE)
    })

    applied_filters
  })
}
