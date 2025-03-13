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
#'   \item A dropdown menu for selecting a condition (e.g., `==`, `>`, `<`, `>=`, `<=`).
#'   \item A text input for specifying the filter value.
#'   \item A remove button to remove the filter.
#' }

input_filter_ui <- function(id, cols) {
  ns <- NS(id)

  div(
    id = ns("filter_container"),
    class = "filter-widget-container",
    selectizeInput(
      ns("column"),
      "",
      choices = cols,
      width = "20em",
      options = list(dropdownParent = "body")
    ),
    selectizeInput(
      ns("condition"),
      "",
      choices = c("==", ">", "<", ">=", "<="),
      width = "5em",
      options = list(dropdownParent = "body")
    ),
    textInput(ns("value"), "", width = "15em"),
    actionButton(ns("remove"), "X", class = "btn-danger")
  )
}

input_filter_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    is_active <- reactiveVal(TRUE)

    # update filter list #
    applied_filters <- reactive({
      # check if filter was not removed #
      if (is_active()) {
        list(
          column = input$column,
          condition = input$condition,
          value = input$value
        )
      } else {
        # if filter was removed, return NULL #
        NULL
      }
    })

    # remove filter #
    observeEvent(input$remove, {
      removeUI(selector = paste0("#", session$ns("filter_container")))
      is_active(FALSE)
    })

    applied_filters
  })
}
