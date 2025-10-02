#' Creates and displays a full loading popup with a spinner.
#' Needs to be closed separately by running `removeModal()`
#'
#' @param text Text to be displayed as modal title.
#'
loading_popup <- function(text = "Loading...") {
  modal_body <- div(
    class = "loading-spinner-container",
    div(id = "loading-spinner")
  )

  showModal(modalDialog(
    modal_body,
    title = span(text, id = "loading-title"),
    footer = modalButton("Close"),
    size = "s"
  ))
}
