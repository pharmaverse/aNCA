#' Classify a parameter exclusion by its source
#'
#' Determines whether a parameter record was excluded by a default flag rule,
#' by a manual (user) exclusion, by both, or not at all. Used to drive the
#' red / yellow / orange colour coding shared by the ADPP Exclusions table and
#' the parameter boxplots.
#'
#' @param is_flag Logical vector. `TRUE` where the record was excluded by a
#'   default flag rule (i.e. the PKNCA `exclude` column is populated).
#' @param is_manual Logical vector. `TRUE` where the record was excluded
#'   manually by the user (the `.pp_excl` marker).
#'
#' @returns A character vector, same length as the inputs, with values
#'   `"both"`, `"flag"`, `"manual"`, or `"none"`. `NA` inputs are treated as
#'   `FALSE`.
#' @keywords internal
#' @noRd
.classify_exclusion <- function(is_flag, is_manual) {
  n <- max(length(is_flag), length(is_manual))
  if (n == 0) return(character(0))
  is_flag <- rep_len(!is.na(is_flag) & is_flag, n)
  is_manual <- rep_len(!is.na(is_manual) & is_manual, n)

  out <- rep("none", n)
  out[is_flag & is_manual] <- "both"
  out[is_flag & !is_manual] <- "flag"
  out[!is_flag & is_manual] <- "manual"
  out
}

# Exclusion colour palette shared by the ADPP Exclusions table (row
# background) and the parameter boxplots (excluded-point crosses).
# Keyed by the categories returned by `.classify_exclusion()`.
EXCL_TYPE_COLORS <- c(
  flag   = "#FFCCCC", # red    — default flag exclusion
  manual = "#FFF3CD", # yellow — custom (user) exclusion
  both   = "#FFD9B3"  # orange — both flag and manual
)

# Stronger, saturated variants for plot point crosses (backgrounds above are
# too light to read as small markers on a plot).
EXCL_TYPE_POINT_COLORS <- c(
  flag   = "#D62728", # red
  manual = "#E8B800", # amber/yellow
  both   = "#FF7F0E"  # orange
)

#' Map exclusion categories to colours.
#'
#' @param type Character vector of categories from `.classify_exclusion()`.
#' @param point Logical. Use the saturated point palette (`TRUE`) or the light
#'   background palette (`FALSE`, default).
#' @returns Character vector of hex colours; `NA` for `"none"`/unknown.
#' @keywords internal
#' @noRd
.exclusion_type_color <- function(type, point = FALSE) {
  palette <- if (point) EXCL_TYPE_POINT_COLORS else EXCL_TYPE_COLORS
  unname(palette[type])
}
