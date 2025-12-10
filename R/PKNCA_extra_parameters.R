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
