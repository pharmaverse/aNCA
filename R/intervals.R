#' Create Dose Intervals Dataset
#'
#' This function creates a dataset with dose intervals and specified pharmacokinetic parameters.
#'
#' @param pknca_conc A PKNCAdose object containing the concentration data.
#' @param pknca_dose A PKNCAdose object containing the dose data.
#' @param params A character vector specifying the pharmacokinetic parameters to include.
#' @param start_from_last_dose Logical defining if start is at time of last dose or C1.
#'
#' @returns A data frame containing the dose intervals and specified pharmacokinetic parameters.
#'
#' @details
#' The function performs the following steps:
#'   - Creates a vector with all pharmacokinetic parameters.
#'   - Based on dose times, creates a data frame with start and end times.
#'   - If TAU column is present in data, sets last dose end time to start + TAU,
#'   or if TAU is NA then either Inf if only one dose present, or max end time if not.
#'   - If no TAU column in data, sets last dose end time to the time of last sample
#'   or Inf if single dose data.
#'   - Adds logical columns for each specified parameter.
#'
#'  Assumes that multiple dose data will have a TAU column
#'  or contain multiple doses in dataset
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   dose_intervals <- format_pkncadata_intervals(pknca_conc, pknca_dose, params)
#' }
#'
#' @import dplyr
#' @importFrom stats setNames
#' @export
format_pkncadata_intervals <- function(pknca_conc,
                                       pknca_dose,
                                       start_from_last_dose = TRUE) {
  if (!inherits(pknca_conc, "PKNCAconc")) {
    stop("Input pknca_conc must be a PKNCAconc object from the PKNCA package.")
  }
  
  if (!inherits(pknca_dose, "PKNCAdose")) {
    stop("Input pknca_dose must be a PKNCAdose object from the PKNCA package.")
  }
  
  required_columns <- c(unname(unlist(pknca_dose$columns$groups)), pknca_dose$columns$time)
  missing_columns <- setdiff(required_columns, colnames(pknca_dose$data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }
  
  # Select relevant group columns
  conc_groups <- unname(unlist(pknca_conc$columns$groups))
  dose_groups <- unname(unlist(pknca_dose$columns$groups))
  time_column <- pknca_dose$columns$time
  # Obtain all possible pknca parameters
  params <- setdiff(names(PKNCA::get.interval.cols()),
                    c("start", "end"))
  
  # Select conc data and for time column give priority to non-predose samples
  sub_pknca_conc <- pknca_conc$data %>%
    select(any_of(c(conc_groups, "ARRLT", "NCA_PROFILE", "DOSNOA", "TAU", "VOLUME")))
  
  has_tau <- "TAU" %in% names(sub_pknca_conc)
  
  # Select dose data and use its time column as a time of last dose reference
  sub_pknca_dose <- pknca_dose$data %>%
    group_by(!!!syms(dose_groups)) %>%
    mutate(is_one_dose = length(unique(DOSNOA)) == 1) %>%
    ungroup() %>%
    select(any_of(c(dose_groups,
                    time_column, "DOSNOA", "is_one_dose")))
  
  # Based on dose times create a data frame with start and end times
  dose_intervals <- left_join(sub_pknca_dose,
                              sub_pknca_conc,
                              by = intersect(names(sub_pknca_dose), c(conc_groups, "DOSNOA")),
                              relationship = "many-to-many") %>%
    
    # Pick 1 per concentration group and dose number
    group_by(!!!syms(dose_groups), DOSNOA) %>%
    mutate(max_end = max(ARRLT, na.rm = TRUE)) %>% # calculate max end time for Dose group
    filter(ARRLT >= 0) %>% # filter out negative ARRLT values
    group_by(!!!syms(c(conc_groups, "DOSNOA"))) %>%
    slice(1) %>% # slice one row per conc group
    ungroup() %>%
    
    # Make start from last dose (pknca_dose) or first concentration (pknca_conc)
    mutate(start = if (start_from_last_dose) !!sym(time_column)
           else !!sym(time_column) + !!sym("ARRLT")) %>%
    group_by(!!!syms(conc_groups)) %>%
    arrange(start) %>%
    
    # Make end based on next dose time (if no more, Tau or last NFRLT)
    mutate(end = if (has_tau) {
      case_when(
        !is.na(lead(!!sym(time_column))) ~ lead(!!sym(time_column)),
        is.na(TAU) & is_one_dose ~ Inf,
        is.na(TAU) ~ start + max_end,
        TRUE ~ start + TAU
      )
    } else {
      case_when(
        !is.na(lead(!!sym(time_column))) ~ lead(!!sym(time_column)),
        is_one_dose ~ Inf,
        TRUE ~ start + max_end
      )
    }
    ) %>%
    ungroup() %>%
    select(any_of(c("start", "end", conc_groups,
                    "NCA_PROFILE", "DOSNOA", "VOLUME"))) %>%
    
    # Create logical columns with only TRUE for the NCA parameters requested by the user
    mutate(!!!setNames(rep(FALSE, length(params)), params)) %>%
    # Identify the intervals as the base ones for the NCA analysis
    mutate(type_interval = "main")
  
}

#' Update an intervals data frame with user-selected parameters by study type
#'
#' @param data A PKNCAdata object containing intervals and dosing data.
#' @param parameter_selections A named list of selected PKNCA parameters by study type.
#' @param study_types_df A data frame mapping analysis profiles to their study type.
#' @param auc_data A data frame containing partial AUC ranges.
#' @param impute Logical indicating whether to impute start values for parameters.
#'
#' @returns An updated PKNCAdata object with parameter intervals based on user selections.
#'
update_main_intervals <- function(data, parameter_selections,
                                       study_types_df, auc_data, impute = TRUE) {
  
  all_pknca_params <- setdiff(names(PKNCA::get.interval.cols()), c("start", "end"))
  
  # Determine the grouping columns from the study_types_df
  grouping_cols <- setdiff(names(study_types_df), c("type", "USUBJID_Count"))
  
  # 1. Add the 'type' column to the intervals data
  intervals_with_types <- data$intervals %>%
    left_join(study_types_df, by = grouping_cols)
  
  # 3. Iterate over selections to set new flags to TRUE
  updated_intervals <- intervals_with_types
  if (length(parameter_selections) > 0) {
    for (study_type in names(parameter_selections)) {
      params_for_type <- parameter_selections[[study_type]]
      if (length(params_for_type) > 0) {
        updated_intervals <- updated_intervals %>%
          mutate(across(
            .cols = all_of(params_for_type),
            .fns = ~ if_else(type == study_type, TRUE, .)
          ))
      }
    }
  }
  
  # Return the final data frame, removing the temporary 'type' column
  data$intervals <- updated_intervals %>%
    select(-type)
  
  # # Add partial AUCs if any
  
  auc_ranges <- auc_data %>%
    filter(!is.na(start_auc), !is.na(end_auc), start_auc >= 0, end_auc > start_auc)
  
  # Make a list of intervals from valid AUC ranges
  intervals_list <- pmap(auc_ranges, function(start_auc, end_auc) {
    data$intervals %>%
      mutate(
        start = start + start_auc,
        end = start + (end_auc - start_auc),
        across(where(is.logical), ~FALSE),
        aucint.last = TRUE,
        type_interval = "manual"
      )
  })
  
  data$intervals <- bind_rows(
    data$intervals,
    intervals_list
  ) %>%
    unique()
  
  data$impute <- NA
  
  # Impute start values if requested
  if (impute) {
    data <- create_start_impute(data)
    
    # Don't impute parameters that are not AUC dependent
    params_auc_dep <- metadata_nca_parameters %>%
      filter(grepl("auc|aumc", PKNCA) | grepl("auc", Depends)) %>%
      pull(PKNCA)
    
    params_not_to_impute <- metadata_nca_parameters %>%
      filter(!grepl("auc|aumc", PKNCA),
             !grepl(paste0(params_auc_dep, collapse = "|"), Depends)) %>%
      pull(PKNCA) |>
      intersect(names(PKNCA::get.interval.cols()))
    
    all_impute_methods <- na.omit(unique(data$intervals$impute))
    
    data$intervals <- Reduce(function(d, ti_arg) {
      interval_remove_impute(
        d,
        target_impute = ti_arg,
        target_params = params_not_to_impute
      )
    }, all_impute_methods, init = data$intervals)
  }
  
  data
}

#' Update  a PKNCAdata object for slope calculations
#'
#' @param data A PKNCAdata object containing intervals and dosing data.
#' @param params A character vector of parameters to include in slope calculations.
#' @param should_impute_c0 Logical indicating whether to impute start values for parameters.
#'
#' @returns A fully configured `PKNCAdata` object.
#'
#' @import dplyr
#'
#' @export
update_slopes_intervals <- function(
    data,
    params = NULL,
    should_impute_c0 = TRUE
) {

  data$intervals <- data$intervals %>%
    mutate(across(any_of(params), ~ TRUE, .names = "{.col}"))
  
  data$impute <- NA
  
  # Impute start values if requested
  if (should_impute_c0) {
    data <- create_start_impute(data)
    
    # Don't impute parameters that are not AUC dependent
    params_auc_dep <- metadata_nca_parameters %>%
      filter(grepl("auc|aumc", PKNCA) | grepl("auc", Depends)) %>%
      pull(PKNCA)
    
    params_not_to_impute <- metadata_nca_parameters %>%
      filter(!grepl("auc|aumc", PKNCA),
             !grepl(paste0(params_auc_dep, collapse = "|"), Depends)) %>%
      pull(PKNCA) |>
      intersect(names(PKNCA::get.interval.cols()))
    
    all_impute_methods <- na.omit(unique(data$intervals$impute))
    
    data$intervals <- Reduce(function(d, ti_arg) {
      interval_remove_impute(
        d,
        target_impute = ti_arg,
        target_params = params_not_to_impute
      )
    }, all_impute_methods, init = data$intervals)
  }
  
  data
}
