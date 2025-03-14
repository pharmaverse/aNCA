#' Calculates results for PKNCA analysis.
#'
#' @details
#' TODO
#'
#' @param pknca_data Data object created using PKNCA::PKNCAdata() function.
#'
#' @returns Results object with... TODO
#'
#' @examples
#' TODO
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