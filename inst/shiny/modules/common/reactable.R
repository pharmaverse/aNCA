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

reactable_server <- function(id, data, download_buttons = c(), file_name = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    default_reactable_opts <- list(
      searchable = TRUE,
      sortable = TRUE,
      highlight = TRUE,
      wrap = FALSE,
      resizable = TRUE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      class = "reactable-table",
      columns = generate_col_defs
    )

    args <- list(...)
    reactable_opts <- c(
      default_reactable_opts[!names(default_reactable_opts) %in% names(args)],
      args
    )

    # Show requested download  buttons
    purrr::walk(download_buttons, \(x) shinyjs::show(paste0("download_", x)))

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

      do.call(reactable, c(list(data = data()), opts))
    })

    output$download_csv <- downloadHandler(
      filename = .reactable_file_name(file_name, "csv"),
      content = \(con) write.csv(data(), con, row.names = FALSE)
    )

    output$download_xlsx <- downloadHandler(
      filename = .reactable_file_name(file_name, "xlsx"),
      content = \(con) openxlsx2::write_xlsx(data(), con)
    )
  })
}

# Creates file name for export
.reactable_file_name <- function(file_name, ext) {
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
