#' Limit Input Value
#'
#' This function ensures an input's value stays within the specified min-max limits.
#' If the value is outside the limits, updates it to the closest limit and notifies.
#'
#' @param input The Shiny input object.
#' @param input The Shiny session.
#' @param input_id The ID of the input to be limited.
#' @param max_value The maximum allowed value for the input (default is Inf).
#' @param min_value The minimum allowed value for the input (default is -Inf).
#' @param label A label for the input, used in the notification message. Default is an empty char.
#' @param update_fun The input updating function (default is `updateNumericInput`).
#'
limit_input_value <- function(input, session, id, max_value = Inf, min_value = -Inf, label = "",
                              update_fun = updateNumericInput) {
  observeEvent(input[[id]], {
    input_value <- input[[id]]

    if (!is.na(input_value)) {
      if (input_value < min_value) {
        update_fun(session, id, value = min_value)
        showNotification(paste0(label, " input min value is ", min_value), type = "warning")
      }
      if (input_value > max_value) {
        update_fun(session, id, value = max_value)
        showNotification(paste0(label, " input max value is ", max_value), type = "warning")
      }
    }
  })
}