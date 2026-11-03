#' Function generating an input widget for TLG option.
#' @param id      id of the input widget
#' @param opt_def definition of the option, as specified in the `yaml` file
#' @param data    data object used for parsing labels, strings, inferring placeholder values or
#'                choices etc.
tlg_option_numeric_ui <- function(id, opt_def, data) {
  ns <- NS(id)

  label <- if (is.null(opt_def$label)) sub(".*-(.*)", "\\1", id) else opt_def$label

  numericInput(
    ns("numeric"),
    label = label,
    value = opt_def$default
  )
}

#' Function generating an input widget server for TLG option.
#' @param id            id of the input widget
#' @param opt_def       definition of the option, as specified in the `yaml` file
#' @param data          data object used for parsing labels, strings, inferring placeholder
#'                      values or choices etc.
#' @param reset_trigger a reactive expression on which the module will restore its returned value
#'                      to the default one.
#' @param value_override Reactive numeric value restored from settings.
#' @returns a reactive with the input value
tlg_option_numeric_server <- function(id, opt_def, data, reset_trigger,
                                      value_override = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    default <- opt_def$default %||% NA_real_
    value <- reactiveVal(default)
    observeEvent(input$numeric, {
      if (!"numeric" %in% names(input)) return()
      value(input$numeric %||% NA_real_)
    }, ignoreNULL = FALSE)
    observeEvent(value_override(), {
      value(value_override() %||% default)
      updateNumericInput(session, "numeric", value = value())
    }, ignoreNULL = FALSE, priority = 100)
    observeEvent(reset_trigger(), {
      value(default)
      updateNumericInput(session, "numeric", value = default)
    }, ignoreInit = TRUE)

    value
  })
}
