#' Define the UI for a single partial AUC input
#'
#' @param id A unique identifier for the input.
#' @param min_sel_value The default minimum value for the input.
#' @param max_sel_value The default maximum value for the input.
#'
#' @return A Shiny UI component for creating a partial AUC input.
#'
#'

partialAUCInput <- function(id,
                            min_sel_value=0,
                            max_sel_value=NULL) {
  fluidRow(
    id = id,
    column(width = 6, numericInput(paste0("timeInputMin_", id), "Min:", min = 0, value = min_sel_value)),
    column(width = 6, numericInput(paste0("timeInputMax_", id), "Max:", min = 0, value = max_sel_value))
  )
}
