#' Define the UI for a single partial AUC input
#'
#' @param id A unique identifier for the input.
#' @param min_sel_value The default minimum value for the input.
#' @param max_sel_value The default maximum value for the input.
#'
#' @return A Shiny UI component for creating a partial AUC input.
#'
#'

partial_auc_input <- function(id, min_sel_value = 0, max_sel_value = NULL) {
  fluidRow(
    id = id,
    column(
      width = 2, shinyWidgets::sliderTextInput(inputId = paste0("RangeInput_", id), 
                                               label = "Interval range", 
                                               choices = c(0, "C1", 1:100, "Cn", Inf),
                                               selected = c("C1", Inf),
                                               dragRange = TRUE)
    ),
    column(
      width = 2, shinyWidgets::pickerInput(paste0("ParamInput_", id), "Parameters:", 
                                           choices = sort(setdiff(names(PKNCA::PKNCA.options()$single.dose.aucs),
                                                                  c("start", "end"))),
                                           options = list(maxItems = NULL),
                                           multiple = TRUE)
    )
  )
}
