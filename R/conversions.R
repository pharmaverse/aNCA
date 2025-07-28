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
    if (isTRUE(paste0(initial_unit) == paste0(target_unit))) {
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
#' @returns A character string representing the duration in ISO 8601 format.
#' @details
#' It is a sensitive function that assumes that a valid time unit is given by the user.
#' That means that if other units starting with 'y', 'm', 'w', 'd', 'h', or 's' are provided,
#' it will make a naive guess that it refers to a time unit as year, month, week, day, hour, sec...
#'
#' @examples
#' aNCA:::convert_to_iso8601_duration(200, "h") # Returns "PT200H"
#' aNCA:::convert_to_iso8601_duration(5, "d")  # Returns "P5D"
#' @keywords internal
convert_to_iso8601_duration <- Vectorize(function(value, unit) {

  # NA values remain NA
  if (is.na(value)) return(NA)

  # Input validation
  if (!is.numeric(value)) {
    stop("'value' must be a numeric.")
  }
  if (is.infinite(value)) {
    value <- tolower(value)
  }
  if (!is.character(unit)) {
    stop("'unit' must be a character string.")
  }
  if (!grepl("^[ymwdhs]", tolower(unit))) {
    stop("Unsupported unit. Accepted units start with 'y', 'm', 'w', 'd', 'h', or 's'.")
  }

  # Try to standardize the unit using units::set_units
  unit <- tryCatch(
    {
      units::set_units(1, tolower(unit), mode = "standard") %>% units::deparse_unit()
    },
    error = function(e) {
      # If an error occurs, return the original unit
      unit
    }
  )

  # Construct the ISO 8601 duration
  if (unit %in% c("h", "min", "s")) {
    # Time components need a "PT" prefix
    paste0("PT", value, toupper(substr(unit, 1, 1)))
    # Period components need a "P" prefix
    # Note: Months and years are not standardized by units package
  } else {
    paste0("P", value, toupper(substr(unit, 1, 1)))
  }
})



#' Convert Volume Units to Match Concentration Denominator Units
#'
#' This function identifies rows associated with excretion samples (e.g., urine, feces, bile)
#' and adjusts the `VOLUME` and `VOLUMEU` columns so that the volume unit matches the
#' denominator unit in the corresponding concentration unit (`AVALU`).
#' This is necessary for PKNCA calculation of excretion parameters.
#'
#' It uses the \pkg{units} package to perform unit-safe conversions. If a direct conversion
#' between volume and the concentration denominator is not possible (e.g., between mass and volume),
#' a fallback conversion is attempted using a neutral density of `1 (target_unit / original_unit)`.
#' The function modifies only the `VOLUME` and `VOLUMEU` columns when necessary and leaves all
#' other data unchanged.
#'
#' @param df A data frame containing pharmacokinetic data.
#' @param avalu A character string specifying the column name for
#' concentration values (default: "AVALU").
#' @param volume A character string specifying the column name for
#' volume or mass values (default: "VOLUME").
#' @param volumeu A character string specifying the column name for
#' volume or mass units (default: "VOLUMEU").
#'  It must contain the following columns:
#'   \describe{
#'     \item{PCSPEC}{Sample type (e.g., urine, feces, bile, plasma).}
#'     \item{AVAL}{Concentration values.}
#'     \item{AVALU}{Concentration units (e.g., "ug/mL", "mg/g").}
#'     \item{VOLUME}{Volume or mass values for integration.}
#'     \item{VOLUMEU}{Units for the `VOLUME` column (e.g., "mL", "g").}
#'   }
#'
#' @returns A modified data frame with `VOLUME` and `VOLUMEU` converted (where necessary)
#'  so that multiplying `AVAL * VOLUME` results in a unit with consistent dimensionality
#'  (typically mass or moles).
#' A new column `AMOUNTU` is created to represent the product of `AVALU` and `VOLUMEU`.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Parses the denominator from `AVALU` (e.g., "ug/mL" → "mL").
#'   \item Attempts to convert the corresponding `VOLUME` to that unit.
#'   \item If direct conversion fails, assumes a neutral density of 1
#'    (i.e., `1 unit_target / unit_original`) and retries.
#'   \item Leaves units unchanged for non-excreta samples or already-valid combinations.
#' }
#'
#' The function assumes that the `AVALU` column contains concentration units
#' in the form of "x/y" (e.g., "ug/mL", "mg/g").
#' @importFrom dplyr `%>%` mutate sym
#' @importFrom purrr pmap_chr
#' @importFrom units set_units drop_units
#' @importFrom stringr str_split str_trim
#' @examples
#' df <- data.frame(
#'   PCSPEC = c("urine", "feces", "plasma"),
#'   AVAL = c(100, 5, 70),
#'   AVALU = c("ug/mL", "mg/g", "ng/mL"),
#'   VOLUME = c(2, 1.5, 3),
#'   VOLUMEU = c("L", "mL", "mL"),
#'   stringsAsFactors = FALSE
#' )
#'
#' df_converted <- convert_volume_units(df)
#'
#' @export
convert_volume_units <- function(df,
                                 avalu = "AVALU",
                                 volume = "VOLUME",
                                 volumeu = "VOLUMEU") {

  required_cols <- c(avalu, volume, volumeu)
  if (!all(required_cols %in% names(df))) {
    return(df)
  }

  for (i in seq_len(nrow(df))) {
    concu <- df[[avalu]][i]
    vol <- df[[volume]][i]
    volu <- df[[volumeu]][i]

    if (any(is.na(c(concu, vol, volu)))) next

    unit_parts <- str_split(concu, "/", simplify = TRUE)
    if (ncol(unit_parts) != 2) next

    denom_unit <- str_trim(unit_parts[2])

    tryCatch({
      u_vol <- set_units(vol, volu, mode = "standard")
      u_vol_new <- tryCatch(
        set_units(u_vol, denom_unit, mode = "standard"),
        error = function(e) {
          # Attempt indirect conversion via neutral density
          density_unit <- paste0(denom_unit, "/", volu)
          density <- set_units(1, density_unit, mode = "standard")
          u_vol * density
        }
      )
      df[[volume]][i] <- drop_units(u_vol_new)
      df[[volumeu]][i] <- denom_unit

      log_conversion(i, vol, volu, u_vol_new, denom_unit, concu, verbose = TRUE)
      TRUE
    }, error = function(e) {
      warning(glue::glue("Row {i}: Failed to convert {vol} {volu} to {denom_unit}
                         (concentration: {concu}): {e$message}"))
    })

  }

  df %>%
    mutate(
      AMOUNTU = pmap_chr(
        list(!!sym(avalu), !!sym(volumeu)),
        function(avalu_val, volumeu_val) {
          tryCatch({
            if (is.na(avalu_val) || is.na(volumeu_val)) return(NA_character_)
            u1 <- set_units(1, avalu_val, mode = "standard")
            u2 <- set_units(1, volumeu_val, mode = "standard")
            deparse_unit(u1 * u2)
          }, error = function(e) {
            NA_character_
          })
        }
      )
    )
}

#'Log conversions applied to dataset
#'
#' This helper function logs the conversion of volume units in a dataset.
#'
#' @param row The row number where the conversion is applied.
#' @param vol The original volume value.
#' @param volu The original volume unit.
#' @param u_vol_new The new volume value after conversion.
#' @param denom_unit The denominator unit derived from the concentration unit.
#' @param concu The concentration unit.
#' @param verbose A logical indicating whether to log the conversion (default: TRUE).
#' @returns NULL if units remained the same, or log info of the conversions that were applied
#'
#' @keywords internal
log_conversion <- function(row, vol, volu, u_vol_new, denom_unit, concu, verbose = TRUE) {
  same_units <- denom_unit == volu

  if (!verbose || same_units) return()

  msg <- sprintf(
    "Row %d: Converted volume from %.6g %s to %.6g %s based on concentration unit %s",
    row, vol, volu, u_vol_new, denom_unit, concu
  )
  message(msg)
}
