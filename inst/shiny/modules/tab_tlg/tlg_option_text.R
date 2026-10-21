#' Function generating an input widget for TLG option.
#' @param id      id of the input widget
#' @param opt_def definition of the option, as specified in the `yaml` file
#' @param data    data object used for parsing labels, strings, inferring placeholder values or
#'                choices etc.
tlg_option_text_ui <- function(id, opt_def, data) {
  ns <- NS(id)

  label <- if (is.null(opt_def$label)) sub(".*-(.*)", "\\1", id) else opt_def$label

  textAreaInput(
    ns("text"),
    label = label,
    value = opt_def$default,
    rows = 2
  )
}

#' Function generating an input widget server for TLG option.
#' @param id            id of the input widget
#' @param opt_def       definition of the option, as specified in the `yaml` file
#' @param data          data object used for parsing labels, strings, inferring placeholder
#'                      values or choices etc.
#' @param reset_trigger a reactive expression on which the module will restore its returned value
#'                      to the default one.
#' @param value_override Reactive raw label restored from settings; NULL uses the catalog default.
#' @returns a reactive with the input value
tlg_option_text_server <- function(id, opt_def, data, reset_trigger,
                                   value_override = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    default <- if (is.null(opt_def$default)) "" else opt_def$default
    value <- reactiveVal(default)
    observeEvent(input$text, value(input$text))
    observeEvent(value_override(), {
      restored <- value_override()
      value(if (is.null(restored)) default else restored)
      updateTextAreaInput(session, "text", value = value())
    }, ignoreNULL = FALSE, priority = 100)

    #' Reset the input to the declared default upon reset_trigger.
    #'
    #' Set explicitly rather than via `shinyjs::reset()`, which restores the value the
    #' element was *created* with.  Since tlg_module_server() re-renders text widgets
    #' carrying the user's current value forward (so edits survive re-submitting the
    #' order), that creation-time value is the user's own text and resetting to it would
    #' be a no-op.
    observeEvent(reset_trigger(), {
      value(default)
      updateTextAreaInput(session, "text", value = default)
    }, ignoreInit = TRUE)

    value
  })
}
