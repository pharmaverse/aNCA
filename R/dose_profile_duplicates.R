#' Create duplicates in concentration data with Pre-dose and Last Values for Dosing Cycles
#'
#' This function duplicates and adjusts concentration data to ensure all dosing cycles have
#' complete pre-dose and last concentration values. It is designed for use in pharmacokinetic
#' analyses where dosing intervals and concentration values need to be aligned for each dose.
#'
#' @param conc_data A data frame containing concentration data.
#' @param groups A character vector of column names to use for grouping
#'               (e.g., c("USUBJID", "PARAM", "DOSNOA")).
#' @param dosno Column name for the dose number (default: "DOSNOA").
#' @param arrlt Column name for time from the most recent dose.
#' @param afrlt Column name for time from the first dose.
#' @param nrrlt Column name for the numeric relative time.
#' @param nfrlt Column name for the nominal relative time.
#'
#' @returns A data frame with adjusted concentration data, including:
#'   - Duplicated pre-dose values assigned to the previous dose.
#'   - Duplicated last values assigned to the next dose if pre-dose values are missing,
#'   filtered so only samples within 4 hours of the next dose are included.
#'   - Sorted by the grouping variables and relative time.
#'
#' @import dplyr
#' @examples
#' # Example usage
#' conc_data <- data.frame(
#' USUBJID = c("001", "001", "001", "001", "001", "001", "001", "001", "001", "001"),
#' AVAL = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#' DOSNOA = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3),
#' ARRLT = c(-1, 0, 1, -1, 0, 1, 2, 0, 1, 2),
#' AFRLT = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8),
#' NRRLT = c(-1, 0, 1, -1, 0, 1, 2, 0, 1, 2),
#' NFRLT = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8)
#' )
#' result <- dose_profile_duplicates(conc_data,
#'                             groups = c("USUBJID", "DOSNOA"), dosno = "DOSNOA")
#'
#' @export
dose_profile_duplicates <- function(conc_data,
                                    groups = c("USUBJID", "DOSNOA", "PARAM"),
                                    dosno = "DOSNOA",
                                    arrlt = "ARRLT",
                                    afrlt = "AFRLT",
                                    nrrlt = "NRRLT",
                                    nfrlt = "NFRLT") {
  
  dosno_sym <- sym(dosno)

  #If only one dose, return the original data
  if (n_distinct(conc_data[[dosno]]) == 1) {
    return(conc_data %>%
             group_by(across(all_of(groups))) %>%
             arrange(across(all_of(c(groups, arrlt)))) %>%
             mutate(IX = seq_len(n())) %>%
             ungroup())
  }

  # Determine dose_time and nominal_dose_time per profile group
  dose_times <- conc_data %>%
    mutate(
      dose_time = !!sym(afrlt) - !!sym(arrlt),
      nom_dose_time = !!sym(nfrlt) - !!sym(nrrlt)
    ) %>%
    select(all_of(groups), dose_time, nom_dose_time) %>%
    group_by(across(all_of(groups))) %>%
    summarize(
      dose_time = first(dose_time),
      nom_dose_time = first(nom_dose_time),
      .groups = "drop"
    )
  
  # Create prev/next dosno mapping within each subject/param group
  dose_order <- dose_times %>%
    group_by(across(all_of(setdiff(groups, dosno)))) %>%
    arrange(dose_time, .by_group = TRUE) %>%
    mutate(
      prev_dosno = lag(!!sym(dosno)),
      next_dosno = lead(!!sym(dosno)),
      interval_prev = dose_time - lag(dose_time),
      nom_interval_prev = nom_dose_time - lag(nom_dose_time),
      interval_next = lead(dose_time) - dose_time,
      nom_interval_next = lead(nom_dose_time) - nom_dose_time,
      next_dose_time = lead(dose_time),
      next_nom_dose_time = lead(nom_dose_time),
      across(c(interval_prev, nom_interval_prev, interval_next,
               nom_interval_next), ~ replace_na(., 0))
    ) %>%
    ungroup()
  
  # Duplicate pre-dose values to previous dose
  predose_duplicates <- conc_data %>%
    filter(!!sym(arrlt) < 0) %>%
    left_join(dose_order, by = groups) %>%
    filter(!is.na(prev_dosno)) %>%
    mutate(
      !!dosno := prev_dosno,
      !!sym(arrlt) := !!sym(arrlt) + interval_prev,
      !!sym(nrrlt) := !!sym(nrrlt) + nom_interval_prev
    ) %>%
    select(-prev_dosno, -next_dosno, -interval_prev, -interval_next,
           -nom_interval_prev, -nom_interval_next, -dose_time,
           -nom_dose_time, -next_dose_time, -next_nom_dose_time)

  # Identify which dose groups are missing predose samples
  existing_predose <- conc_data %>% filter(!!sym(arrlt) < 0)
  missing_predose <- dose_times %>%
    anti_join(existing_predose, by = groups) %>%
    left_join(dose_order, by = groups) %>%
    filter(!is.na(prev_dosno)) %>%
    mutate(!!dosno := prev_dosno)
  
  # Duplicate last value of previous dose as predose of next dose
  last_values <- conc_data %>%
    semi_join(missing_predose, by = c(groups)) %>%
    group_by(across(all_of(groups))) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    left_join(dose_order, by = groups) %>%
    mutate(
      !!dosno := next_dosno,
      !!sym(arrlt) := !!sym(afrlt) - next_dose_time,
      !!sym(nrrlt) := !!sym(nfrlt) - next_nom_dose_time
    ) %>%
    filter(!!sym(arrlt) >= -4) %>%
    select(-prev_dosno, -next_dosno, -interval_prev, -interval_next,
           -nom_interval_prev, -nom_interval_next, -dose_time,
           -nom_dose_time, -next_dose_time, -next_nom_dose_time)
  
  # Combine all
  bind_rows(conc_data, predose_duplicates, last_values) %>%
    group_by(across(all_of(groups))) %>%
    arrange(across(all_of(c(groups, arrlt)))) %>%
    mutate(IX = seq_len(n())) %>%
    ungroup()
}
