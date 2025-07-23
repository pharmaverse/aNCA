#' Custom reactable module for extending functionalities of base reactable,
#' mainly focused on providing common layout of the reactables across the app
#' and ability to download data from the table directly.
#'
#' @param id ID of the module.
#' @param data Reactive containing data to be displayed.
#' @param download_buttons Character vector containing file extensions to be available for the user
#'                         to download. Currently available: `csv`, `xlsx`.
#' @param ... Any other parameters to be passed to `reactable()` call.
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

reactable_server <- function(id, data, download_buttons = c(), ...) {
  moduleServer(id, function(input, output, session) {
    # Show requested download  buttons
    purrr::walk(download_buttons, \(x) shinyjs::show(paste0("download_", x)))

    output$table <- renderReactable({
      req(data())
      reactable(
        data(),
        ...
      )
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        paste0(id, "_", Sys.Date(), ".csv")
      },
      content = function(con) {
        write.csv(data(), con, row.names = FALSE)
      }
    )

    output$download_xlsx <- downloadHandler(
      filename = function() {
        paste0(id, "_", Sys.Date(), ".xlsx")
      },
      content = function(con) {
        openxlsx2::write_xlsx(data(), con)
      }
    )
  })
}