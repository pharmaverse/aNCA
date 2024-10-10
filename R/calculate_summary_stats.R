#' Calculate Summary Statistics
#'
#' This function calculates various summary statistics for formatted output of PKNCA::pk.nca().
#'
#' @param input_groups A character vector specifying the columns to group by.
#'                     Here. the hierachrical order matters
#' @param resPKNCA     A data frame containing results of
#'                     Non Compartmental Analysis using PKNCA package
#' @return A data frame with summary statistics for each group and parameter.
#' @details The function calculates the following statistics for numeric variables:
#' \itemize{
#'   \item Geometric mean (`geomean`)
#'   \item Geometric coefficient of variation (`geocv`)
#'   \item Arithmetic mean (`mean`)
#'   \item Coefficient of variation (`CV`)
#'   \item Standard deviation (`sd`)
#'   \item Minimum value (`min`)
#'   \item Maximum value (`max`)
#'   \item Median value (`median`)
#'   \item Count (`count`)
#' }
#' The function excludes the columns `ANALYTE`, `DOSEA`, `PCSPEC`, `STUDYID`, and `USUBJID`
#' from the numeric summarization because they are possible grouping variables
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

calculate_summary_stats <- function(resPKNCA = resNCA(), input_groups = "DOSNO") {

  # Disconsider interval records for the summary statistics
  data <- resPKNCA$result %>%
    filter(end == Inf) %>%
    mutate(start = 0)

  # Join subject data to allow the user to group by it
  data <- merge(
    data,
    resPKNCA$data$conc$data %>%
      select(any_of(c(input_groups, unname(unlist(resPKNCA$data$conc$columns$groups)))))
  )

  # Calculate summary statistics, using all value rows
  # (note: this will give more weight to subjects with more valid records)
  summary_stats <- data %>%
    group_by(across(all_of(c(input_groups, "PPTESTCD")))) %>%
    unique() %>%
    summarise(
      geomean = exp(mean(log(PPORRES), na.rm = TRUE)),
      geocv = (sd(PPORRES, na.rm = TRUE) / exp(mean(log(PPORRES), na.rm = TRUE))) * 100,
      mean = mean(PPORRES, na.rm = TRUE),
      CV = (sd(PPORRES, na.rm = TRUE) / mean(PPORRES, na.rm = TRUE)) * 100,
      sd = sd(PPORRES, na.rm = TRUE),
      min = min(PPORRES, na.rm = TRUE),
      max = max(PPORRES, na.rm = TRUE),
      median = median(PPORRES, na.rm = TRUE),
      count.missing = sum(is.na(PPORRES)),
      count.total = n()
    ) %>%
    ungroup() %>%
    mutate(across(where(is.numeric), round, digits = 3))

  return(summary_stats)
  # Note: It is not producing the same results as old function...
  # Maybe you did give the same weight to each subject?
  # (calculate stat of each subkect and then aggregate)
  # Should we first make descriptive stats for each subject and then summarize those?
}