#' Calculate the midpoint collection time of the last measurable excretion rate
#'
#' @param conc The concentration in the excreta (e.g., urine or feces)
#' @param volume The volume (or mass) of the sample
#' @param time The starting time of the collection interval
#' @param duration.conc The duration of the collection interval
#' @param check Should the concentration and time data be checked?
#' @return The midpoint collection time of the last measurable excretion rate, or NA/0 if not available
#' @export
pk.calc.ertlst <- function(conc, volume, time, duration.conc, check = TRUE) {

  # Generate messages about missing concentrations/volumes
  message_all <- generate_missing_messages(conc, volume,
                                           name_a = "concentrations",
                                           name_b = "volumes")

  if (all(is.na(conc))) {
    ret <- NA_real_
  } else if (all(conc %in% c(0, NA))) {
    ret <- 0
  } else {
    midtime <- time + duration.conc / 2
    midtime <- time + duration.conc / 2
    ret <- max(midtime[!(conc %in% c(NA, 0))])
  }

  if (length(message_all) != 0) {
    message <- paste(message_all, collapse = "; ")
    ret <- structure(ret, exclude = message)
  }
  ret
}

# Add the column to the interval specification
PKNCA::add.interval.col("ertlst",
                 FUN="pk.calc.ertlst",
                 unit_type="time",
                 pretty_name="Tlast excretion rate",
                 desc="The midpoint collection time of the last measurable excretion rate (typically in urine or feces)")

PKNCA::PKNCA.set.summary(
  name="ertlst",
  description="median and range",
  point = PKNCA::business.median,
  spread = PKNCA::business.range
)

#' Calculate the maximum excretion rate
#'
#' @param conc The concentration in the excreta (e.g., urine or feces)
#' @param volume The volume (or mass) of the sample
#' @param time The starting time of the collection interval
#' @param duration.conc The duration of the collection interval
#' @param check Should the concentration data be checked?
#' @return The maximum excretion rate, or NA if not available
#' @export
pk.calc.ermax <- function(conc, volume, time, duration.conc, check = TRUE) {
  
  # Generate messages about missing concentrations/volumes
  message_all <- generate_missing_messages(conc, volume,
                                           name_a = "concentrations",
                                           name_b = "volumes")
  
  if (length(conc) == 0 || all(is.na(conc))) {
    ret <- NA
  } else {
    er <- conc * volume / duration.conc
    ret <- max(er, na.rm=TRUE)
  }
  
  if (length(message_all) != 0) {
    message <- paste(message_all, collapse = "; ")
    ret <- structure(ret, exclude = message)
  }
  ret
}

#' Calculate the midpoint collection time of the maximum excretion rate
#'
#' @param conc The concentration in the excreta (e.g., urine or feces)
#' @param volume The volume (or mass) of the sample
#' @param time The starting time of the collection interval
#' @param duration.conc The duration of the collection interval
#' @param check Should the concentration and time data be checked?
#' @param first.tmax If TRUE, return the first time of maximum excretion rate; otherwise, return the last
#' @param options List of PKNCA global options set
#' @return The midpoint collection time of the maximum excretion rate, or NA if not available
#' @export
pk.calc.ertmax <- function( #nolint
    conc, volume, time, duration.conc, check = TRUE, first.tmax = NULL, options = list() #nolint
) {

  # Generate messages about missing concentrations/volumes
  message_all <- generate_missing_messages(conc, volume,
                                           name_a = "concentrations",
                                           name_b = "volumes")

  if (length(conc) == 0 || all(conc %in% c(NA, 0))) {
    ret <- NA
  } else {
    er <- conc * volume / duration.conc
    ermax <- pk.calc.ermax(conc, volume, time, duration.conc, check = FALSE)
    midtime <- time + duration.conc / 2
    ret <- midtime[er %in% ermax]
    
    first.tmax <- PKNCA::PKNCA.choose.option(name="first.tmax", value = first.tmax, options = options)
    if (first.tmax) {
      ret <- ret[1]
    } else {
      ret <- ret[length(ret)]
    }
  }

  if (length(message_all) != 0) {
    message <- paste(message_all, collapse = "; ")
    ret <- structure(ret, exclude = message)
  }
  ret
}

PKNCA::add.interval.col(
  "ertmax",
  FUN = "pk.calc.ertmax",
  unit_type = "time",
  pretty_name = "Tmax excretion rate",
  desc = "The midpoint collection time of the maximum excretion rate (typically in urine or feces)"
)

PKNCA::PKNCA.set.summary(
  name = "ertmax",
  description = "median and range",
  point = PKNCA::business.median,
  spread = PKNCA::business.range
)



# Helper to generate missing-data checking messages for paired vectors
#
# This function accepts two columns/vectors (for example, concentrations
# and volumes). It computes missingness internally and produces a character
# vector of human-readable messages describing the missingness that matches
# the style used in the package (used previously in `pk.calc.ae`).
generate_missing_messages <- function(a, b,
                                      name_a = deparse(substitute(a)),
                                      name_b = deparse(substitute(b))) {

  mask_a <- is.na(a)
  mask_b <- is.na(b)

  mask_both <- mask_a & mask_b
  mask_a_only <- mask_a & !mask_both
  mask_b_only <- mask_b & !mask_both

  msg_both <- msg_a <- msg_b <- NA_character_
  n <- length(mask_a)

  if (all(mask_both)) {
    msg_both <- sprintf("All %s and %s are missing", name_a, name_b)
  } else if (any(mask_both)) {
    msg_both <- sprintf("%g of %g %s and %s are missing", sum(mask_both), n, name_a, name_b)
  }

  if (all(mask_a_only)) {
    msg_a <- sprintf("All %s are missing", name_a)
  } else if (any(mask_a_only)) {
    msg_a <- sprintf("%g of %g %s are missing", sum(mask_a_only), n, name_a)
  }

  if (all(mask_b_only)) {
    msg_b <- sprintf("All %s are missing", name_b)
  } else if (any(mask_b_only)) {
    msg_b <- sprintf("%g of %g %s are missing", sum(mask_b_only), n, name_b)
  }

  # Return non-NA messages
  stats::na.omit(c(msg_both, msg_a, msg_b))
}



####################################################################################

#' Calculate the total urine volume
#'
#' @param volume The volume (or mass) of the sample
#' @return The sum of urine volumes for the interval
#' @export
pk.calc.volpk <- function(volume) { #nolint
  if (length(volume) == 0) return(NA_real_)
  sum(volume)
}
PKNCA::add.interval.col(
  "volpk",
  FUN = "pk.calc.volpk",
  values = c(FALSE, TRUE),
  unit_type = "volume",
  pretty_name = "Total Urine Volume",
  desc = "The sum of urine volumes for the interval"
)
PKNCA::PKNCA.set.summary(
  name = "volpk",
  description = "geometric mean and geometric coefficient of variation",
  point = PKNCA::business.geomean,
  spread = PKNCA::business.geocv
)

################################################################################


# Define column interval for fraction excreted (aNCA: #669, PKNCA: #473)
PKNCA::add.interval.col(
  "fe",
  FUN = "pk.calc.fe",
  unit_type = "amount_dose",
  pretty_name = "Fraction excreted",
  values = c(FALSE, TRUE),
  depends = "ae",
  desc = "The fraction of the dose excreted"
)
