#' Create duplicates in concentration data with Predose and Last Values for Dosing Cycles
#'
#' This function duplicates and adjusts concentration data to ensure all dosing cycles have 
#' complete predose and last concentration values. It is designed for use in pharmacokinetic 
#' analyses where dosing intervals and concentration values need to be aligned for each dose.
#'
#' @param conc_data A data frame containing concentration data.
#' @param groups A character vector of column names to use for grouping
#'               (e.g., c("USUBJID", "ANALYTE", "PCSPEC")).
#' @param dosno Column name for the dose number (default: "DOSNO").
#' @param arrlt Column name for time from the most recent dose (default: "ARRLT").
#' @param afrlt Column name for time from the first dose (default: "AFRLT").
#' @param nrrlt Column name for the numeric relative time (default: "NRRLT").
#' @param nfrlt Column name for the nominal relative time (default: "NFRLT").
#'
#' @return A data frame with adjusted concentration data, including:
#'   - Duplicated predose values assigned to the previous dose.
#'   - Duplicated last values assigned to the next dose if predose values are missing.
#'   - Sorted by the grouping variables and relative time.
#'
#' @examples
#' # Example usage
#' result <- create_duplicates(conc_data,
#'                             groups = c("USUBJID", "ANALYTE", "PCSPEC"), dosno = "DOSNO")
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
    group_keys(groups) %>%
    mutate(
      interval_prev = dose_time - lag(dose_time),
      interval_prev = replace_na(interval_prev, 0),
      nom_interval_prev = nom_dose_time - lag(nom_dose_time),
      nom_interval_prev = replace_na(nom_interval_prev, 0),
    ) %>%
    ungroup()
  
  
  # Step 3: Duplicate predose values to be clast for the previous dose
  predose_duplicates <- conc_data %>%
    filter(.data[[arrlt]] <= 0, .data[[dosno]] > 1) %>%
    left_join(dosing_intervals, by = c(groups)) %>%
    mutate(
      !!dosno := .data[[dosno]] - 1,
      !!arrlt := .data[[arrlt]] + interval_prev,
      !!nrrlt := .data[[nrrlt]] + nom_interval_prev
    ) %>%
    select(-interval_prev, -nom_interval_prev)
  
  # Step 4: Identify missing predose values for the next dose
  missing_predose <- dose_times %>%
    anti_join(
      conc_data %>% filter(.data[[arrlt]] < 0),
      by = c(groups)
    )
  
  # Step 5: Duplicate last value of the previous dose to become predose of next
  last_values <- conc_data %>%
    semi_join(missing_predose, by = c(groups)) %>%
    group_keys(c(groups)) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    left_join(dosing_intervals, by = c(groups)) %>%
    mutate(
      !!dosno := .data[[dosno]] + 1,
      !!arrlt := .data[[arrlt]] - interval_prev,
      !!nrrlt := .data[[nrrlt]] - nom_interval_prev
    ) %>%
    select(-interval_prev, -nom_interval_prev)
  
  # Step 6: Combine all data
  conc_data <- conc_data %>%
    bind_rows(predose_duplicates, last_values) %>%
    arrange(across(all_of(c(groups, arrlt))))
  
  return(conc_data)
}
