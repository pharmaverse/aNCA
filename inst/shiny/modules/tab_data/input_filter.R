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

input_filter_server <- function(id, filters_metadata) {
  moduleServer(id, function(input, output, session) {
    is_active <- reactiveVal(TRUE)
    is_numeric <- reactive({
      req(input$column)
      filters_metadata()[[input$column]]$type == "numeric"
    })

    observeEvent(input$column, {
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
