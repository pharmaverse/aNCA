#' Transform Units
#'
#' This function transforms a value from an initial unit to a target unit.
#'
#' @param initial_unit A character string representing the initial unit.
#' @param target_unit A character string representing the target unit.
#' @returns A numeric value for the conversion factor from the initial to the target unit,
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


#' Convert Numeric Value and Unit to ISO 8601 Duration
#'
#' The function converts a numeric value and its associated time unit into ISO 8601 duration string.
#'
#' @param value A numeric value representing the time/duration.
#' @param unit A character string representing the unit of the time/duration.
#' @return A character string representing the duration in ISO 8601 format.
#' @examples
#' convert_to_iso8601_duration(200, "h") # Returns "PT200H"
#' convert_to_iso8601_duration(5, "d")  # Returns "P5D"
#' @export
convert_to_iso8601_duration <- Vectorize(function(value, unit) {

  # Input validation
  if (!is.numeric(value)) {
    stop("'value' must be a numeric.")
  }
  if (!is.character(unit)) {
    stop("'unit' must be a character string.")
  }
  if (!grepl("^[ymwdhs]", unit)) {
    stop("Unsupported unit. Accepted units start with 'y', 'm', 'w', 'd', 'h', or 's'.")
  }

  # Try to standardize the unit using units::set_units
  unit <- tryCatch(
    {
      units::set_units(1, unit, mode = "standard") %>% units::deparse_unit()
    },
    error = function(e) {
      # If an error occurs, return the original unit
      unit
    }
  )

  # Ensure the unit is lowercase for consistency
  unit <- tolower(unit)

  # Define a mapping of units to ISO 8601 components
  unit_map <- c(
    y = "Y",   # Years
    month = "M",   # Months
    w = "W",   # Weeks
    d = "D",   # Days
    h = "H",   # Hours
    min = "M", # Minutes
    s = "S"    # Seconds
  )

  # Construct the ISO 8601 duration
  if (unit %in% c("h", "min", "s")) {
    # Time components need a "T" prefix
    paste0("PT", value, unit_map[unit])
  } else if (unit %in% names(unit_map)) {
    # Date components
    paste0("P", value, unit_map[unit])
  } else {
    # Take the first letter of the unit & assume is date
    paste0("P", value, toupper(substr(unit, 1, 1)))
  }
})
