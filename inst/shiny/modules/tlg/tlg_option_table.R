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
        setNames(names(opt_def$cols)) %>%
        as_tibble() %>%
        mutate(across(everything(), as.character))
    }) %>%
      bind_rows() %>%
      mutate(across(everything(), ~ ifelse(. == "$NA", NA, .)))

    output_table <- reactiveVal(default_table)
    edits_table <- reactiveVal(default_table)

    observeEvent(input$open_table, {
      .tlg_option_table_popup()
    })

    output$table <- renderReactable({
      edit_widgets <- imap(opt_def$cols, \(def, colname) {
        if (def$type == "text") {
          colDef(
            cell = text_extra(
              id = session$ns(colname)
            )
          )
        } else {
          stop("ERR")
        }
      })

      reactable(
        output_table(),
        striped = TRUE,
        bordered = TRUE,
        highlight = TRUE,
        columns = edit_widgets
      )
    })

    observe({
      req(output_table())
      # Dynamically attach observers for each column
      purrr::walk(names(opt_def$cols), \(colname) {
        observeEvent(input[[colname]], {
          edit <- input[[colname]]
          edited_vars <- edits_table()
          edited_vars[edit$row, edit$column] <- edit$value
          edits_table(edited_vars)
        })
      })
    })

    observeEvent(input$confirm_changes, {
      removeModal()
      output_table(edits_table())
    })

    observeEvent(input$cancel, {
      removeModal()
      shinyjs::runjs("memory = {};") # needed to properly reset reactable.extras widgets
      edits_table(output_table())
    })

    output_table
  })
}

.tlg_option_table_popup <- function(session = shiny::getDefaultReactiveDomain()) {
  showModal(modalDialog(
    title = "Edit table",
    reactableOutput(session$ns("table")),
    footer = tagList(
      actionButton(session$ns("confirm_changes"), "Confirm", class = "btn-success"),
      actionButton(session$ns("cancel"), "Cancel", class = "btn-danger")
    )
  ))
}