#' Calculate Summary Statistics
#'
#' This function calculates various summary statistics for formatted output of PKNCA::pk.nca().
#'
#' @param input_groups A character vector specifying the columns to group by.
#'                     Here. the hierarchical order matters
#' @param res_pknca     A data frame containing results of
#'                     Non Compartmental Analysis using PKNCA package
#' @return A data frame with summary statistics for each group and parameter.
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
#' @importFrom stats sd
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   STUDYID = rep(1, 10),
#'   USUBJID = rep(1:2, each = 5),
#'   DOSEA = rep(c(10, 20), each = 5),
#'   ANALYTE = rep(c("A", "B"), each = 5),
#'   PCSPEC = rep(c("X", "Y"), each = 5),
#'   VALUE = rnorm(10)
#' )
#' input_groups <- c("STUDYID", "USUBJID", "DOSEA", "ANALYTE", "PCSPEC")
#' calculate_summary_stats(data, input_groups)
#' }

calculate_summary_stats <- function(res_pknca, input_groups = "DOSNO") {

  data <- res_pknca$result

  # Join subject data to allow the user to group by it
  data <- merge(
    data,
    res_pknca$data$conc$data %>%
      select(any_of(c(input_groups, unname(unlist(res_pknca$data$conc$columns$groups)))))
  )

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
      Min = min(PPSTRES, na.rm = TRUE),
      Max = max(PPSTRES, na.rm = TRUE),
      Median = median(PPSTRES, na.rm = TRUE),
      Count.missing = sum(is.na(PPSTRES)),
      Count.total = n()
    ) %>%
    ungroup() %>%
    mutate(across(where(is.numeric), round, digits = 3))%>%
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
