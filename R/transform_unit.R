#' Transform Units
#'
#' This function transforms a value from an initial unit to a target unit.
#'
#' @param initial_unit A character string representing the initial unit.
#' @param target_unit A character string representing the target unit.
#' @return A numeric value for the conversion factor from the initial to the target unit,
#' or NA if the units are not convertible.
#' @examples
#' transform_unit("meter", "kilometer")
#' transform_unit("sec", "min")
#' @importFrom units set_units
#' @export
transform_unit <- function(initial_unit, target_unit) {
  vec_fun <- Vectorize(function(initial_unit, target_unit) {
    tryCatch({
      conversion <- units::set_units(
        units::set_units(1, initial_unit, mode = "standard"),
        target_unit, mode = "standard"
      )
      as.numeric(conversion)
    }, error = function(e) {
      if (initial_unit == target_unit) {
      1
    } else {
      NA
    }
    })
  })
  unname(vec_fun(initial_unit, target_unit))
}
