#' Function generating an input widget for TLG option.
#' @param id      id of the input widget
#' @param opt_def definition of the option, as specified in the `yaml` file
#' @param data    data object used for parsing labels, strings, infering placeholder values or
#'                choices etc.
tlg_option_table_ui <- function(id, opt_def, data) {
  ns <- NS(id)

  label <- if (is.null(opt_def$label)) id else opt_def$label

  actionButton(ns("open_table"), label = label)
}

#' Function generating an input widget server for TLG option.
#' @param id      id of the input widget
#' @param opt_def definition of the option, as specified in the `yaml` file
#' @param data    data object used for parsing labels, strings, infering placeholder values or
#'                choices etc.
#' @returns a reactive with the input value
tlg_option_table_server <- function(id, opt_def, data) {
  moduleServer(id, function(input, output, session) {
    default_table <- lapply(opt_def$default_rows, \(x) {
      as.list(x) %>%
        setNames(opt_def$cols) %>%
        as_tibble() %>%
        mutate(across(everything(), as.character))
    }) %>%
      bind_rows() %>%
      mutate(across(everything(), ~ ifelse(. == "$NA", NA, .)))

    output_table <- reactiveVal(default_table)


    observeEvent(input$open_table, {
      .tlg_option_table_popup()
    })

    output$table <- renderReactable({
      reactable(
        output_table(),
        striped = TRUE,
        bordered = TRUE,
        highlight = TRUE
      )
    })

    output_table
  })
}

.tlg_option_table_popup <- function(session = shiny::getDefaultReactiveDomain()) {
  showModal(modalDialog(
    title = "Edit table",
    reactableOutput(session$ns("table"))
  ))

}