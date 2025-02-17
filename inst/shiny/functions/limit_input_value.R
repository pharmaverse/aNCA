#' Limit Input Value
#'
#' This function observes changes to a specified input and ensures that its value stays within the specified minimum and maximum limits.
#' If the input value is outside these limits, it updates the input value to the nearest limit and shows a notification.
#'
#' @param input The Shiny input object.
#' @param input The Shiny session.
#' @param input_id The ID of the input to be limited.
#' @param max_value The maximum allowed value for the input (default is Inf).
#' @param min_value The minimum allowed value for the input (default is -Inf).
#' @param label A label for the input, used in the notification message (default is an empty string).
#' @param update_function The function used to update the input value (default is `updateNumericInput`).
#'
#' @importFrom shiny observeEvent showNotification
#' @export
limit_input_value <- function(input, session, id, max_value = Inf, min_value = -Inf, label = "", update_function = updateNumericInput) {
  observeEvent(input[[id]], {
    input_value <- input[[id]]
    
    if (!is.na(input_value)) {
      if (input_value < min_value) {
        update_function(session, id, "", value = min_value)
        showNotification(paste0(label, " input min value is ", min_value))
      }
      if (input_value > max_value) {
        update_function(session, id, "", value = max_value)
        showNotification(paste0(label, " input max value is ", max_value))
      }
    }
  })
}