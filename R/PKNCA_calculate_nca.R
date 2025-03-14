#' Calculates results for PKNCA analysis.
#'
#' @details
#' This function+ calculates results for PKNCA analysis using `PKNCA::pk.nca()`.
#' It then joins the results with the dosing data, to create a full results data frame
#' with the start and end times for each dose, from first and most recent dose.
#'
#' @param pknca_data Data object created using PKNCA::PKNCAdata() function.
#'
#' @returns Results object with start and end times for each dose, from first dose
#' and from most recent dose
#'
#' @examples
#' example_data <- data.frame(
#'   STUDYID = rep("STUDY001", 6),
#'   PCSPEC = rep("Plasma", 6),
#'   ROUTE = rep("IV", 6),
#'   DRUG = rep("DrugA", 6),
#'   USUBJID = rep("SUBJ001", 6),
#'   DOSNO = rep(1, 6),
#'   ANALYTE = rep("AnalyteA", 6),
#'   AVAL = c(0, 5, 10, 7, 3, 1),
#'   AVALU = rep("ng/mL", 6),
#'   DOSEA = rep(100, 6),
#'   DOSEU = rep("mg", 6),
#'   AFRLT = c(0, 1, 2, 3, 4, 6),
#'   ARRLT = c(0, 1, 2, 3, 4, 6),
#'   NFRLT = c(0, 1, 2, 3, 4, 6),
#'   ADOSEDUR = rep(0.5, 6),
#'   RRLTU = rep("hour", 6)
#' )
#'
#' # Create a PKNCAdata object
#' pknca_data <- PKNCA_create_data_object(example_data)
#'
#' # Perform NCA calculations
#' nca_results <- PKNCA_calculate_nca(pknca_data)
#'
#' @export
PKNCA_calculate_nca <- function(pknca_data) { # nolint: object_name_linter
  results <- PKNCA::pk.nca(data = pknca_data, verbose = FALSE)

  results$result <- results$result %>%
    inner_join(
      select(pknca_data$dose$data, -exclude, -pknca_data$conc$columns$groups$group_analyte)
      # TODO: add `by = `argument to avoid warnings
    ) %>%
    mutate(
      start_dose = start - !!sym(results$data$dose$columns$time),
      end_dose = end - !!sym(results$data$dose$columns$time)
    ) %>%
    select(names(results$result), start_dose, end_dose)

  results
}