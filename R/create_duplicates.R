#' Create duplicates in concentration data with Pre-dose and Last Values for Dosing Cycles
#'
#' This function duplicates and adjusts concentration data to ensure all dosing cycles have
#' complete pre-dose and last concentration values. It is designed for use in pharmacokinetic
#' analyses where dosing intervals and concentration values need to be aligned for each dose.
#'
#' @param conc_data A data frame containing concentration data.
#' @param groups A character vector of column names to use for grouping
#'               (e.g., c("USUBJID", "ANALYTE")).
#' @param dosno Column name for the dose number (default: "DOSNO").
#' @param arrlt Column name for time from the most recent dose.
#' @param afrlt Column name for time from the first dose.
#' @param nrrlt Column name for the numeric relative time.
#' @param nfrlt Column name for the nominal relative time.
#'
#' @return A data frame with adjusted concentration data, including:
#'   - Duplicated pre-dose values assigned to the previous dose.
#'   - Duplicated last values assigned to the next dose if pre-dose values are missing,
#'   filtered so only samples within 24 hours of the next dose are included.
#'   - Sorted by the grouping variables and relative time.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' conc_data <- data.frame(
#'  USUBJID = c("001", "001", "001", "001", "001", "001"),
#'  DOSNO = c(1, 1, 1, 2, 2, 2),
#'  ARRLT = c(-1, 0, 1, 1, 2, 3),
#'  AFRLT = c(0, 0.5, 1, 3, 4, 5),
#'  NRRLT = c(0, 1, 2, 1, 2, 3),
#'  NFRLT = c(0, 0.5, 1, 3, 4, 5),
#' result <- create_duplicates(conc_data,
#'                             groups = c("USUBJID"), dosno = "DOSNO")
#' }
#'
#' @export
create_duplicates <- function(conc_data,
                              groups = slopes_groups(),
                              dosno = "DOSNO",
                              arrlt = "ARRLT",
                              afrlt = "AFRLT",
                              nrrlt = "NRRLT",
                              nfrlt = "NFRLT") {
  # Helper to construct grouping keys
  group_keys <- function(data, keys) {
    data %>%
      group_by(across(all_of(keys)))
  }

  #If only one dose, return the original data
  if (n_distinct(conc_data[[dosno]]) == 1) {
    return(conc_data)
  }
  # Step 1: Identify the dosing times (ARRLT == 0)
  dose_times <- conc_data %>%
    mutate(dose_time = .data[[afrlt]] - .data[[arrlt]],
           nom_dose_time = .data[[nfrlt]] - .data[[nrrlt]]) %>%
    select(all_of(groups), dose_time, nom_dose_time) %>%
    group_keys(c(groups)) %>%
    summarize(dose_time = first(dose_time),
              nom_dose_time = first(nom_dose_time),
              .groups = "drop")

  # Step 2: Calculate dosing intervals
  dosing_intervals <- dose_times %>%
    group_keys(setdiff(groups, dosno)) %>%
    mutate(
      interval_prev = dose_time - lag(dose_time),
      nom_interval_prev = nom_dose_time - lag(nom_dose_time),
      interval_next = lead(dose_time) - dose_time,
      nom_interval_next = lead(nom_dose_time) - nom_dose_time,
      next_dose = interval_next + dose_time,
      next_nom_dose = nom_interval_next + nom_dose_time,
      across(c(interval_prev, nom_interval_prev, interval_next,
               nom_interval_next, next_dose, next_nom_dose),
             ~ replace_na(., 0))
    ) %>%
    ungroup()


  # Step 3: Duplicate predose values to be clast for the previous dose
  predose_duplicates <- conc_data %>%
    filter(.data[[arrlt]] < 0, .data[[dosno]] > 1) %>%
    left_join(dosing_intervals, by = c(groups)) %>%
    mutate(
      !!dosno := .data[[dosno]] - 1,
      !!arrlt := .data[[arrlt]] + interval_prev,
      !!nrrlt := .data[[nrrlt]] + nom_interval_prev
    ) %>%
    select(-interval_prev, -nom_interval_prev, -interval_next,
           -nom_interval_next, -next_dose, -next_nom_dose,
           -dose_time, -nom_dose_time)

  # Step 4: Identify missing predose values for the next dose
  missing_predose <- dose_times %>%
    anti_join(
      conc_data %>% filter(.data[[arrlt]] < 0),
      by = c(groups)
    ) %>%
    mutate(!!dosno := .data[[dosno]] - 1)

  # Step 5: Duplicate last value of the previous dose to become predose of next
  last_values <- conc_data %>%
    semi_join(missing_predose, by = c(groups)) %>%
    group_keys(c(groups)) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    left_join(dosing_intervals, by = c(groups)) %>%
    mutate(
      !!dosno := .data[[dosno]] + 1,
      !!arrlt := .data[[afrlt]] - next_dose,
      !!nrrlt := .data[[nfrlt]] - next_nom_dose
    ) %>%
    filter(arrlt >= -24) %>%
    select(-interval_prev, -nom_interval_prev, -interval_next,
           -nom_interval_next, -next_dose, -next_nom_dose,
           -dose_time, -nom_dose_time)

  # Step 6: Combine all data
  conc_data <- conc_data %>%
    bind_rows(predose_duplicates, last_values) %>%
    arrange(across(all_of(c(groups, arrlt))))

  conc_data
}
