#' Function generating an input widget for TLG option.
#' @param id      id of the input widget
#' @param opt_def definition of the option, as specified in the `yaml` file
#' @param data    data object used for parsing labels, strings, infering placeholder values or
#'                choices etc.
tlg_option_text_ui <- function(id, opt_def, data) {
  ns <- NS(id)

  label <- if (is.null(opt_def$label)) sub(".*-(.*)", "\\1", id) else opt_def$label

  textInput(
    ns("text"),
    label = label,
    value = opt_def$default
  )
}

#' Function generating an input widget server for TLG option.
#' @param id      id of the input widget
#' @param opt_def definition of the option, as specified in the `yaml` file
#' @param data    data object used for parsing labels, strings, infering placeholder values or
#'                choices etc.
#' @returns a reactive with the input value
tlg_option_text_server <- function(id, opt_def, data) {
  moduleServer(id, function(input, output, session) {
    reactive({
      input$text
    })
  })
}