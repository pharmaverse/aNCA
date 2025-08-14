#' Custom reactable module for extending functionalities of base reactable,
#' mainly focused on providing common layout of the reactables across the app
#' and ability to download data from the table directly.
#'
#' @param id ID of the module.
#' @param data Reactive containing data to be displayed.
#' @param download_buttons Character vector containing file extensions to be available for the user
#'                         to download. Currently available: `csv`, `xlsx`.
#' @param file_name Character string with file name, or `reactive()` or callback function that
#'                  returns character string.
#' @param on_render JavaScript code to be executed on the table after it is rendered.
#' @param editable Character vector with names of columns that should be editable.
#' @param edit_debounce Time in milliseconds to debounce the edit events. Default is 750ms.
#' @param ... Any other parameters to be passed to `reactable()` call. Can be simple values,
#'            reactives or callback functions accepting the `data()` as argument.
reactable_ui <- function(id) {
  ns <- NS(id)

  div(
    div(
      class = "d-flex gap-1 justify-content-end mb-2",
      hidden(downloadButton(ns("download_csv"), label = "csv")),
      hidden(downloadButton(ns("download_xlsx"), label = "xlsx"))
    ),
    withSpinner(reactableOutput(ns("table")))
  )
}

reactable_server <- function(
  id, data, download_buttons = c(), file_name = NULL, on_render = NULL, editable = NULL,
  edit_debounce = 750, ...
) {
  moduleServer(id, function(input, output, session) {
    default_reactable_opts <- list(
      searchable = TRUE,
      sortable = TRUE,
      highlight = TRUE,
      wrap = FALSE,
      resizable = TRUE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      compact = TRUE,
      style = list(fontSize = "0.75em"),
      class = "reactable-table"
    )

    args <- list(...)
    reactable_opts <- c(
      default_reactable_opts[!names(default_reactable_opts) %in% names(args)],
      args
    )

    # Show requested download  buttons
    purrr::walk(download_buttons, \(x) shinyjs::show(paste0("download_", x)))

    # Attach observers for editable columns
    table_edit <- reactiveVal(NULL)
    purrr::walk(editable, \(col) {
      observe({
        table_edit(input[[paste0("edit_", col)]])
      })
    })
    table_edit_debounced <- reactive({
      req(table_edit())
      table_edit()
    }) |>
      debounce(edit_debounce)

    output$table <- renderReactable({
      req(data())
      opts <- lapply(reactable_opts, function(x) {
        if (is.reactive(x))  {
          x()
        } else if (is.function(x)) {
          x(data())
        } else {
          x
        }
      })

      if (!is.null(editable)) {
        col_defs <- lapply(editable, function(col) {
          col_def <- lapply(opts$columns[[col]], \(x) x) # unpack other existing colDef-s
          col_def$cell <- text_extra(id = session$ns(paste0("edit_", col)))

          do.call(colDef, col_def)
        }) |>
          setNames(editable)

        if (is.null(opts$columns)) {
          opts$columns <- col_defs
        } else {
          purrr::iwalk(col_defs, \(val, name) {
            opts$columns[[name]] <<- val
          })
        }
      }

      do.call(reactable, c(list(data = data()), opts)) %>%
        htmlwidgets::onRender(on_render)
    })

    output$download_csv <- downloadHandler(
      filename = .reactable_file_name(file_name, "csv", id),
      content = \(con) write.csv(data(), con, row.names = FALSE)
    )

    output$download_xlsx <- downloadHandler(
      filename = .reactable_file_name(file_name, "xlsx", id),
      content = \(con) openxlsx2::write_xlsx(data(), con)
    )

    reactive(
      list(
        selected = getReactableState("table", "selected"),
        edit = table_edit_debounced
      )
    ) |>
      invisible()
  })
}

# Creates file name for export
.reactable_file_name <- function(file_name, ext, id = NULL) {
  f_name <- {
    if (is.character(file_name)) {
      file_name
    } else if (is.reactive(file_name) || is.function(file_name)) {
      file_name()
    } else {
      paste0(Sys.Date(), "_", id)
    }
  }
  paste0(f_name, ".", ext)
}
