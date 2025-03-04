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
    #' Generates default table based on provided default option definition
    default_table <- lapply(opt_def$default_rows, \(x) {
      as.list(x) %>%
        setNames(names(opt_def$cols)) %>%
        as_tibble() %>%
        mutate(across(everything(), as.character))
    }) %>%
      bind_rows() %>%
      mutate(across(everything(), ~ ifelse(. == "$NA", NA, .)))

    #' Output table with values that is returned by the module
    output_table <- reactiveVal(default_table)

    #' Table for holding edits provided by the user, before confirming.
    edits_table <- reactiveVal(default_table)

    #' For controling reactable re-rendering
    refresh_reactable <- reactiveVal(0)

    #' Shows the editing table
    observeEvent(input$open_table, {
      .tlg_option_table_popup()
    })

    #' Renders the editing table
    output$table <- renderReactable({
      #' Create edit widgets using `reactable.extras` based on provided definitions
      edit_widgets <- imap(opt_def$cols, \(def, colname) {
        colDef(
          cell = switch(
            def$type,
            text = text_extra(id = session$ns(colname)),
            select = dropdown_extra(
              id = session$ns(colname),
              choices = {
                if (isTRUE(def$choices == ".colnames")) {
                  names(data())
                } else if (length(def$choices) == 1 && grepl("^\\$", def$choices)) {
                  unique(data()[, sub("^\\$", "", def$choices)])
                } else {
                  def$choices
                }
              },
              class = "dropdown-extra"
            ),
            stop("Unsupported extra")
          ),
          name = def$label
        )
      })

      reactable(
        edits_table(),
        striped = TRUE,
        bordered = TRUE,
        highlight = TRUE,
        columns = edit_widgets,
        selection = "multiple",
        theme = reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        )
      )
    }) %>%
      shiny::bindEvent(list(output_table(), refresh_reactable()))

    #' Attach observers that listen to any changes provided by the user.
    observe({
      req(output_table())
      purrr::walk(names(opt_def$cols), \(colname) {
        observeEvent(input[[colname]], {
          edit <- input[[colname]]
          edited_vars <- edits_table()
          edited_vars[edit$row, edit$column] <- edit$value
          edits_table(edited_vars)
        })
      })
    })

    #' Add a new row to the edit table
    observeEvent(input$add_row, {
      edits_table() %>%
        add_row() %>%
        edits_table()

      shinyjs::runjs("memory = {};") # needed to properly reset reactable.extras widgets
      refresh_reactable(refresh_reactable() + 1)
    })

    #' Remove a row from the table
    observeEvent(input$remove_row, {
      selected <- getReactableState("table", "selected")
      req(selected)
      edits_table()[-selected, ] %>%
        edits_table()
      shinyjs::runjs("memory = {};") # needed to properly reset reactable.extras widgets
      refresh_reactable(refresh_reactable() + 1)
    })

    #' Confirm changes, apply the changes to output table.
    observeEvent(input$confirm_changes, {
      removeModal()
      output_table(edits_table())
    })

    #' Cancel changes, close the modal, reset values.
    observeEvent(input$cancel, {
      removeModal()
      shinyjs::runjs("memory = {};") # needed to properly reset reactable.extras widgets
      edits_table(output_table())
    })

    output_table
  })
}

#' Generates modal that holds the editing table output.
.tlg_option_table_popup <- function(session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns

  showModal(modalDialog(
    class = "tlg-option-table",
    title = "Edit table",
    div(
      style = "display: flex; flex-direction: column; width: 100%;",
      div(style = "display: flex; gap: 1em; padding-bottom: 1em;",
        actionButton(ns("add_row"), "+ Add +", class = "btn-info"),
        actionButton(ns("remove_row"), "- Remove -", class = "btn-warning")
      ),
      reactableOutput(ns("table"))
    ),
    footer = tagList(
      actionButton(ns("confirm_changes"), "Confirm", class = "btn-success"),
      actionButton(ns("cancel"), "Cancel", class = "btn-danger")
    ),
    size = "xl"
  ))
}