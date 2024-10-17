#' Create a Filter UI Component
#'
#' This function generates a UI component for creating a filter in a Shiny application.
#' The filter allows users to select a column, condition, and value to filter a dataset.
#'
#' @param filter_id A unique identifier for the filter.
#' @param dataset A data frame containing the dataset to be filtered.
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
#'   \item A submit button to confirm the filter.
#'   \item A remove button to remove the filter.
#' }
#'
#' @examples
#' \dontrun{
#'   # Example usage in a Shiny app:
#'   ui <- fluidPage(
#'     create_filter("filter1", mtcars)
#'   )
#'   server <- function(input, output, session) {}
#'   shinyApp(ui, server)
#' }
#'
#' @import shiny
#' @export
#create filters function
create_filter <- function(filter_id, dataset) {
  tags$div(
    id = filter_id,
    tags$hr(),
    selectInput(
      paste0(filter_id, "_column"),
      "Select Column:",
      choices = colnames(dataset)
    ),
    selectInput(
      paste0(filter_id, "_condition"),
      "Condition:",
      choices = c("==", ">", "<", ">=", "<=")
    ),
    textInput(paste0(filter_id, "_value"), "Value:"),
    actionButton(paste0(filter_id, "_confirm"), "Submit"),
    actionButton(paste0(filter_id, "_remove"), "Remove Filter")
  )
}
