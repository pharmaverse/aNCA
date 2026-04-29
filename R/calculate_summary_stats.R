#' Get the most frequent non-NA unit from a character vector
#'
#' Returns `NA_character_` when all values are `NA` or the vector is empty,
#' so grouped `mutate()` always gets a consistent character type.
#'
#' @param x Character vector of unit values (may contain NAs).
#' @returns A single character string (the mode) or `NA_character_`.
#' @keywords internal
#' @noRd
.mode_unit <- function(x) {
  counts <- table(x)
  if (length(counts) == 0) return(NA_character_)
  names(sort(counts, decreasing = TRUE))[1]
}

#' Calculate Summary Statistics
#'
#' This function calculates various summary statistics for formatted output of PKNCA::pk.nca().
#'
#' @param data         A data frame containing results of
#'                     Non Compartmental Analysis using PKNCA package. Assumes
#'                     presence of columns: PPORRES, PPSTRES, PPSTRESU, PPTESTCD
#' @param input_groups A character vector specifying the columns to group by.
#'                     Here. the hierarchical order matters
#'                     Default is "PPSTRESU".
#' @returns A data frame with summary statistics for each group and parameter.
#' @details The function calculates the following statistics for numeric variables:
#' \itemize{
#'   \item Geometric mean (`geomean`)
#'   \item Geometric coefficient of variation (`geocv`)
#'   \item Arithmetic mean (`mean`)
#'   \item Standard deviation (`sd`)
#'   \item Minimum value (`min`)
#'   \item Maximum value (`max`)
#'   \item Median value (`median`)
#'   \item Count of missing values (`count.missing`)
#'   \item Count (`count`)
#' }
#' The resulting summary statistics are rounded to three decimal places.
#' If units are different, they are standardized to the group's most frequent first unit.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stats sd median
#' @export
#' @examples
#' data <- data.frame(
#'   ATPTREF = c(1, 1, 1, 1, 1, 1),
#'   PPTESTCD = c("A", "A", "B", "B", "C", "C"),
#'   PPORRES = c(10, 20, 5, 15, NA, 30),
#'   PPSTRES = c(10, 20, 5, 15, NA, 30),
#'   PPORRESU = c("mg/L", "mg/L", "ng/mL", "ng/mL", "µg/L", "µg/L"),
#'   PPSTRESU = c("mg/L", "mg/L", "ng/mL", "ng/mL", "µg/L", "µg/L")
#' )
#' calculate_summary_stats(data)
calculate_summary_stats <- function(data, input_groups = "ATPTREF") {

  # Return an empty data frame if the input data is empty
  if (nrow(data) == 0) {
    return(data.frame(
      Statistic = character(),
      Value = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  # Return a summary table with statistics
  data %>%

    # Only use unique records and calculate the conversion factor between PPSTRESU/PPORRESU
    unique() %>%
    mutate(
      conv_factor = get_conversion_factor(PPORRESU, PPSTRESU)
    ) %>%

    # Group by the input groups and the parameter test codes
    group_by(across(all_of(c(input_groups, "PPTESTCD")))) %>%

    # Standardize units to the most frequent one within the groups (or first otherwise)
    mutate(
      ModeUnit = .mode_unit(PPSTRESU)
    ) %>%
    ungroup() %>%
    mutate(
      ModeConv_factor = get_conversion_factor(PPORRESU, ModeUnit),
      PPSTRES = PPORRES * ModeConv_factor,
      PPTESTCD = ifelse(
        is.na(ModeUnit) | ModeUnit == "",
        PPTESTCD,
        paste0(PPTESTCD, "[", ModeUnit, "]")
      ),
      PPSTRES_log = log(ifelse(PPSTRES > 0, PPSTRES, NA_real_)),
    ) %>%

    # Group by the input groups and the parameter test codes
    group_by(across(all_of(c(input_groups, "PPTESTCD")))) %>%

    # Calculate summary statistics
    summarise(
      Geomean = exp(mean(PPSTRES_log, na.rm = TRUE)),
      Geocv = (sd(PPSTRES, na.rm = TRUE) / exp(mean(PPSTRES_log, na.rm = TRUE))) * 100,
      Mean = mean(PPSTRES, na.rm = TRUE),
      SD = sd(PPSTRES, na.rm = TRUE),
      Min = ifelse(all(is.na(PPSTRES)), NA, min(PPSTRES, na.rm = TRUE)),
      Max = ifelse(all(is.na(PPSTRES)), NA, max(PPSTRES, na.rm = TRUE)),
      Median = median(PPSTRES, na.rm = TRUE),
      Count.missing = sum(is.na(PPSTRES)),
      Count.total = n(),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), function(x) round(x, 3))) %>%

    # Pivot the data to return groups/statistics as rows & parameters as columns
    pivot_longer(
      cols = c(Geomean, Geocv, Mean, SD, Min, Max, Median, Count.missing, Count.total),
      names_to = "Statistic",
      values_to = "Value"
    ) %>%
    pivot_wider(
      names_from = PPTESTCD,
      values_from = Value
    )
}
