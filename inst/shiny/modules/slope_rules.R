slope_rules_ui <- function(id) {
  ns <- NS(id)

  reactableOutput(ns("slope_rules_table"))
}

slope_rules_server <- function(id, slope_rules, update_trigger) {
  moduleServer(id, function(input, output, session) {
    output$slope_rules_table <- renderReactable({
      reactable(slope_rules())
    }) |>
      bindEvent(update_trigger())

    outputOptions(output, "slope_rules_table", suspendWhenHidden = FALSE)
  })
}