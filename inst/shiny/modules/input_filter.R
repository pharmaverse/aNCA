#' Create a Filter UI Component
#'
#' This module generates a UI component for creating a filter in a Shiny application.
#' The filter allows users to select a column, condition, and value to filter a dataset.
#'
#' @param id   A unique identifier for the filter.
#' @param cols Character vector containing column names to display for selection.
#'
#' @return A Shiny UI component for creating a filter.
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

  tags$div(
    id = ns("filter_container"),
    fluidRow(
      column(width = 4,
        selectInput(
          ns("column"),
          "",
          choices = cols
        )
      ),
      column(width = 2,
        selectInput(
          ns("condition"),
          "",
          choices = c("==", ">", "<", ">=", "<=")
        )
      ),
      column(width = 5,
        textInput(ns("value"), ""),
      ),
      column(width = 1,
        actionButton(ns("remove"), "X", class = "btn-danger")
      )
    )
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
