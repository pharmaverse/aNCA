download_code_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("show_modal"), NULL, icon = icon("code"))
}

download_code_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show_modal, {
      showModal(modalDialog(
        title = "Download Code",
        "This feature is under development.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
  })
}