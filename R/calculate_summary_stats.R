#' Calculate Summary Statistics
#'
#' This function calculates various summary statistics for formatted output of PKNCA::pk.nca().
#'
#' @param data         A data frame containing results of
#'                     Non Compartmental Analysis using PKNCA package
#' @param input_groups A character vector specifying the columns to group by.
#'                     Here. the hierarchical order matters
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
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stats sd median
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#' DOSNO = c(1, 1, 1, 1, 1, 1),
#' PPTESTCD = c("A", "A", "B", "B", "C", "C"),
#' PPSTRES = c(10, 20, 5, 15, NA, 30),
#' PPSTRESU = c("mg/L", "mg/L", "ng/mL", "ng/mL", "µg/L", "µg/L")
#' )
#' calculate_summary_stats(data)
#' }

calculate_summary_stats <- function(data, input_groups = "DOSNO") {

  # Calculate summary statistics, using all value rows
  # (note: this will give more weight to subjects with more valid records)

  summary_stats <- data %>%
    group_by(across(all_of(c(input_groups, "PPTESTCD")))) %>%
    unique() %>%
    summarise(
      Geomean = exp(mean(log(PPSTRES), na.rm = TRUE)),
      Geocv = (sd(PPSTRES, na.rm = TRUE) / exp(mean(log(PPSTRES), na.rm = TRUE))) * 100,
      Mean = mean(PPSTRES, na.rm = TRUE),
      SD = sd(PPSTRES, na.rm = TRUE),
      Min = ifelse(all(is.na(PPSTRES)), NA, min(PPSTRES, na.rm = TRUE)),
      Max = ifelse(all(is.na(PPSTRES)), NA, max(PPSTRES, na.rm = TRUE)),
      Median = median(PPSTRES, na.rm = TRUE),
      Count.missing = sum(is.na(PPSTRES)),
      Count.total = n()
    ) %>%
    ungroup() %>%
    mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
    pivot_longer(
      cols = c(Geomean, Geocv, Mean, SD, Min, Max, Median, Count.missing, Count.total),
      names_to = "Statistic",
      values_to = "Value"
    ) %>%
    pivot_wider(
      names_from = PPTESTCD,
      values_from = Value
    )

  # Include units for all column names
  pttestcd_with_units <- data %>%
    select(PPTESTCD, PPSTRESU) %>%
    unique() %>%
    pull(PPSTRESU, PPTESTCD)

  summary_stats <- summary_stats %>%
    rename_with(~ifelse(
      gsub("_.*", "", .x) %in% names(pttestcd_with_units),
      paste0(.x, "[", pttestcd_with_units[gsub("_.*", "", .x)], "]"),
      .x
    ))

  return(summary_stats)

}
