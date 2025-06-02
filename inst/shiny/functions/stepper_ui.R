#' Generate a Stepper UI Component
#'
#' Creates a horizontal stepper navigation UI using HTML tags, with one active
#' step based on the current tab.
#'
#' @param tab A character string indicating the currently active tab or step.
#'   Must be one of: `"Upload"`, `"Filtering"`, `"Mapping"`, or `"Preview"`.
#'   An error is thrown if an invalid value is provided.
#'
#' @return A `tags$div` HTML element representing the stepper UI.
#'
#' @export
stepper_ui <- function(tab) {
  steps <- c("Upload", "Filtering", "Mapping", "Preview")

  if (!tab %in% steps) {
    stop(sprintf(
      "Invalid tab '%s'. Must be one of: %s",
      tab, paste(shQuote(steps), collapse = ", ")
    ))
  }

  tags$div(
    class = "stepper-wrapper",
    tags$ol(
      class = "c-stepper",
      lapply(steps, function(step) {
        tags$li(
          class = if (tab == step) "c-stepper__item active" else "c-stepper__item",
          tags$h3(class = "c-stepper__title", step)
        )
      })
    )
  )
}
