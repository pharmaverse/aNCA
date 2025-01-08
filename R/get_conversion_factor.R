#' Transform Units
#'
#' This function transforms a value from an initial unit to a target unit.
#'
#' @param initial_unit A character string representing the initial unit.
#' @param target_unit A character string representing the target unit.
#' @return A numeric value for the conversion factor from the initial to the target unit,
#' or NA if the units are not convertible.
#' @examples
#' get_conversion_factor("meter", "kilometer")
#' get_conversion_factor("sec", "min")
#' @importFrom units set_units
#' @export
get_conversion_factor <- Vectorize(function(initial_unit, target_unit) {
  tryCatch({
    conversion <- units::set_units(
      units::set_units(1, initial_unit, mode = "standard"),
      target_unit, mode = "standard"
    )
    unname(as.numeric(conversion))
  }, error = function(e) {
    if (initial_unit == target_unit) {
      1
    } else {
      NA
    }
  })
}, USE.NAMES = FALSE)
