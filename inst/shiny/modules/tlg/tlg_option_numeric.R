tlg_option_numeric_ui <- function(id, opt_def) {
  ns <- NS(id)

  label <- if (is.null(opt_def$label)) id else opt_def$label

  numericInput(
    ns("numeric"),
    label = label,
    value = opt_def$default
  )
}

tlg_option_numeric_server <- function(id, opt_def, data) {
  moduleServer(id, function(input, output, session) {
    reactive({
      input$numeric
    })
  })
}