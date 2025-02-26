tlg_option_text_ui <- function(id, opt_def, data) {
  ns <- NS(id)

  label <- if (is.null(opt_def$label)) id else opt_def$label

  textInput(
    ns("text"),
    label = label,
    value = opt_def$default
  )
}

tlg_option_text_server <- function(id, opt_def, data) {
  moduleServer(id, function(input, output, session) {
    reactive({
      input$text
    })
  })
}