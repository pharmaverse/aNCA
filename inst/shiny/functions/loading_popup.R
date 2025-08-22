#' Creates and displays a full loading popup with a spinner.
#' Needs to be closed separately by running `removeModal()`
#'
#' @param text Text to be displayed as modal title.
#'
loading_popup <- function(text = "Loading...") {
  modal_body <- div(
    style = "height: 4em;",
    div(id = "loading-spinner"),
    tags$style("
      #loading-spinner {
        color: #3f8abf;
        position: relative;
        font-size: 0.5em;
        background: #3f8abf;
        animation: escaleY 1s infinite ease-in-out;
        width: 1em;
        height: 1em;
        animation-delay: -0.16s;
        top: 2em;
      }
      #loading-spinner:before,
      #loading-spinner:after {
        content: '';
        position: absolute;
        top: 0;
        left: 2em;
        background: #3f8abf;
        width: 1em;
        height: 1em;
        animation: escaleY 1s infinite ease-in-out;
      }
      #loading-spinner:before {
        left: -2em;
        animation-delay: -0.32s;
      }

      @keyframes escaleY {
        0%, 80%, 100% {
          box-shadow: 0 0;
          height: 4em;
        }
        40% {
          box-shadow: 0 -2em;
          height: 5em;
        }
      }

      h4.modal-title:has(#loading-title) {
        width: 100%;
      }
    ")
  )

  showModal(modalDialog(
    modal_body,
    title = span(text, id = "loading-title"),
    footer = NULL,
    size = "m"
  ))
}
