#' Create Dose Intervals Dataset
#'
#' This function creates a dataset with dose intervals and specified pharmacokinetic parameters.
#'
#' @param pknca_conc A PKNCAdose object containing the concentration data.
#' @param pknca_dose A PKNCAdose object containing the dose data.
#' @param start_from_last_dose Logical defining if start is at time of last dose or C1.
#' @param keep_interval_cols Optional character vector of additional columns
#'  to keep in the intervals
#' data frame.
#'
#' @returns A data frame containing the dose intervals and specified pharmacokinetic parameters.
#'
#' @details
#' The function performs the following steps:
#'   - Creates a vector with all pharmacokinetic parameters.
#'   - Based on dose times, creates a data frame with start and end times.
#'   - If TRTRINT column is present in data, sets last dose end time to start + TRTRINT,
#'   or if TRTRINT is NA then either Inf if only one dose present, or max end time if not.
#'   - If no TRTRINT column in data, sets last dose end time to the time of last sample
#'   or Inf if single dose data.
#'   - Adds logical columns for each specified parameter.
#'
#'  Assumes that multiple dose data will have a TRTRINT column
#'  or contain multiple doses in dataset
#'
#' @examples
#' adnca <- adnca_example
#' pknca_data <- PKNCA_create_data_object(adnca)
#' pknca_conc <- pknca_data$conc
#' pknca_dose <- pknca_data$dose
#' dose_intervals <- format_pkncadata_intervals(pknca_conc, pknca_dose)
#'
#' @import dplyr
#' @importFrom stats setNames
#' @export
format_pkncadata_intervals <- function(pknca_conc,
                                       pknca_dose,
                                       start_from_last_dose = TRUE,
                                       keep_interval_cols = NULL) {
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
  params <- setdiff(
    names(PKNCA::get.interval.cols()),
    c("start", "end")
  )

  # Select conc data and for time column give priority to non-predose samples
  sub_pknca_conc <- pknca_conc$data %>%
    select(any_of(c(
      conc_groups, "ARRLT", "ATPTREF", "DOSNOA",
      "TRTRINT", "VOLUME", keep_interval_cols
    )))

  has_trtrint <- "TRTRINT" %in% names(sub_pknca_conc)

  # Select dose data and use its time column as a time of last dose reference
  sub_pknca_dose <- pknca_dose$data %>%
    group_by(!!!syms(dose_groups)) %>%
    mutate(is_one_dose = length(unique(DOSNOA)) == 1) %>%
    ungroup() %>%
    select(any_of(c(
      dose_groups,
      time_column, "DOSNOA", "is_one_dose"
    )))

  # Based on dose times create a data frame with start and end times
  dose_intervals <- left_join(
    sub_pknca_dose,
    sub_pknca_conc,
    by = intersect(names(sub_pknca_dose), c(conc_groups, "DOSNOA")),
    relationship = "many-to-many"
  ) %>%
    # Pick 1 per concentration group and dose number
    group_by(!!!syms(dose_groups), DOSNOA) %>%
    mutate(max_end = max(ARRLT, na.rm = TRUE)) %>% # calculate max end time for Dose group
    group_by(!!!syms(c(conc_groups, "DOSNOA"))) %>%
    slice(1) %>% # slice one row per conc group
    ungroup() %>%
    # Make start from last dose (pknca_dose) or first concentration (pknca_conc)
    mutate(start = if (start_from_last_dose) {
      !!sym(time_column)
    } else {
      !!sym(time_column) + !!sym("ARRLT")
    }) %>%
    group_by(!!!syms(conc_groups)) %>%
    arrange(start) %>%
    # Make end based on next dose time (if no more, TRTRINT or last NFRLT)
    mutate(end = if (has_trtrint) {
      case_when(
        !is.na(lead(!!sym(time_column))) ~ lead(!!sym(time_column)),
        is.na(TRTRINT) & is_one_dose ~ Inf,
        is.na(TRTRINT) ~ start + max_end,
        TRUE ~ start + TRTRINT
      )
    } else {
      case_when(
        !is.na(lead(!!sym(time_column))) ~ lead(!!sym(time_column)),
        is_one_dose ~ Inf,
        TRUE ~ start + max_end
      )
    }) %>%
    ungroup() %>%
    # Remove scientifically invalid intervals where start > end
    filter(start <= end) %>%
    select(any_of(c("start", "end", conc_groups,
                    "ATPTREF", "DOSNOA", "VOLUME", keep_interval_cols))) %>%

    # Create logical columns with only TRUE for the NCA parameters requested by the user
    mutate(!!!setNames(rep(FALSE, length(params)), params)) %>%
    # Identify the intervals as the base ones for the NCA analysis
    mutate(type_interval = "main")
}

#' Derive study types from a PKNCAdata object
#'
#' Extracts concentration and dose data, merges dose duration if needed,
#' and calls [detect_study_types()] to classify each group.
#'
#' @param data A PKNCAdata object.
#'
#' @returns A deduplicated data frame with grouping columns and a `type` column.
#'
#' @importFrom dplyr mutate left_join filter select slice_max distinct across all_of
#' @noRd
#' @keywords internal
.derive_study_types <- function(data) {
  conc_data <- data$conc$data
  conc_groups <- group_vars(data$conc)
  dose_groups <- group_vars(data$dose)
  groups <- intersect(dose_groups, conc_groups)
  groups <- groups[vapply(groups, function(col) {
    !is.null(col) && length(unique(conc_data[[col]])) > 1
  }, logical(1))]

  # Blank METABFL unconditionally so metabolite-specific types are not
  # assigned at the interval level.
  conc_data$METABFL <- ""

  # Dose duration may live in dose data only; merge it for detect_study_types.
  duration_col <- data$dose$columns$duration
  if (!is.null(duration_col) && !duration_col %in% names(conc_data)) {
    dose_data <- data$dose$data
    if (duration_col %in% names(dose_data)) {
      conc_time <- data$conc$columns$time
      dose_time <- data$dose$columns$time
      join_by <- intersect(dose_groups, conc_groups)

      dose_subset <- dose_data[, unique(c(join_by, dose_time, duration_col)), drop = FALSE]
      dose_subset <- unique(dose_subset)
      names(dose_subset)[names(dose_subset) == dose_time] <- ".dose_time"

      conc_data <- conc_data %>%
        mutate(.ROWID = seq_len(n())) %>%
        left_join(dose_subset, by = join_by, relationship = "many-to-many") %>%
        filter(.dose_time <= .data[[conc_time]] | is.na(.dose_time)) %>%
        slice_max(.dose_time, n = 1, by = .ROWID, with_ties = FALSE) %>%
        select(-".ROWID", -".dose_time")
    } else {
      conc_data[[duration_col]] <- 0
    }
  }

  study_types_df <- detect_study_types(
    conc_data,
    groups,
    metabfl_column = "METABFL",
    route_column = data$dose$columns$route,
    volume_column = data$conc$columns$volume
  )

  # Deduplicate by grouping columns to prevent interval row duplication
  # when detect_study_types produces multiple types per group.
  grouping_cols <- setdiff(names(study_types_df), "type")
  study_types_df %>%
    distinct(across(all_of(grouping_cols)), .keep_all = TRUE)
}

#' Update an intervals data frame with user-selected parameters by study type
#'
#' @param data A PKNCAdata object containing intervals and dosing data.
#' @param parameter_selections A named list of selected PKNCA parameters by study type.
#' @param int_parameters A data frame containing partial AUC ranges.
#' @param impute Logical indicating whether to impute start values for parameters.
#' @param blq_imputation_rule A list defining the Below Limit of Quantification (BLQ)
#' imputation rule using PKNCA format. The list should either contain three elements named:
#' `first`, `middle`, and `last` or two elements named `before.tmax` and `after.tmax`.
#' Each element can be a numeric value (substituting the BLQ value), or a string such as
#' `"drop"` (ignores the value) or `"keep"` (keeps the value as 0). Default is NULL,
#' which does not specify any BLQ imputation in any interval.
#'
#' @importFrom dplyr left_join mutate across where select all_of if_else bind_rows filter
#' @importFrom dplyr group_by ungroup slice_max distinct
#' @importFrom purrr pmap
#' @returns An updated PKNCAdata object with parameter intervals based on user selections.
#' @export
update_main_intervals <- function(
  data,
  parameter_selections = NULL,
  int_parameters = NULL,
  impute = TRUE,
  blq_imputation_rule = NULL
) {
  if (is.null(parameter_selections)) parameter_selections <- list()
  if (is.null(int_parameters)) {
    int_parameters <- data.frame(
      parameter = character(), start_auc = numeric(), end_auc = numeric()
    )
  }

  all_pknca_params <- setdiff(names(PKNCA::get.interval.cols()), c("start", "end"))

  study_types_df <- .derive_study_types(data)

  grouping_cols <- setdiff(names(study_types_df), "type")
  missing_columns <- setdiff(grouping_cols, colnames(data$intervals))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Add the 'type' column to the intervals data
  intervals_with_types <- data$intervals %>%
    left_join(study_types_df, by = grouping_cols)

  # Iterate over selections to set new flags to TRUE
  updated_intervals <- intervals_with_types
  if (length(parameter_selections) > 0) {
    for (study_type in names(parameter_selections)) {
      params_for_type <- parameter_selections[[study_type]]
      if (length(params_for_type) > 0) {
        updated_intervals <- updated_intervals %>%
          mutate(across(
            .cols = all_of(params_for_type),
            .fns = \(x) if_else(type == study_type, TRUE, x)
          ))
      }
    }
  }

  # Return the final data frame, removing the temporary 'type' column
  data$intervals <- updated_intervals %>%
    select(-type)

  # Add partial AUCs if any
  auc_ranges <- int_parameters %>%
    filter(!is.na(start_auc), !is.na(end_auc), start_auc >= 0, end_auc > start_auc) %>%
    mutate(parameter = translate_terms(parameter, "PPTESTCD", "PKNCA"))

  # Make a list of intervals from valid AUC ranges
  intervals_list <- pmap(auc_ranges, function(start_auc, end_auc, parameter) {
    data$intervals %>%
      mutate(
        start = start + start_auc,
        end = start + (end_auc - start_auc),
        across(where(is.logical), ~FALSE),
        !!sym(parameter) := TRUE,
        type_interval = "manual"
      )
  })

  data$intervals <- bind_rows(
    data$intervals,
    intervals_list
  ) %>%
    unique()

  data$impute <- NA_character_

  # Impute start values if requested
  if (impute) {
    data <- create_start_impute(data)
  }

  ############################################
  # Define a BLQ imputation method for PKNCA
  # and apply it only for non-observational parameters

  if (!is.null(blq_imputation_rule)) {
    data$intervals <- data$intervals %>%
      mutate(
        impute = ifelse(
          is.na(impute) | impute == "",
          "blq",
          paste0("blq, ", impute)
        )
      )
  }

  ############################################
  # Remove any imputation from the observational parameters
  data <- rm_impute_obs_params(data, metadata_nca_parameters)

  data
}

#' Apply Imputation for PKNCA Data
#'
#' Applies imputation rules to a PKNCAdata object, imputing start values and then
#' selectively removing imputation for parameters that are not dependent on AUC.
#'
#' @param data A PKNCAdata object.
#' @param metadata_nca_parameters A data frame mapping PKNCA parameters (`PKNCA`)
#' and information on their parameter dependencies ('Depends').
#' @returns A PKNCAdata object with imputation rules applied.
#' @import dplyr
#'
rm_impute_obs_params <- function(data, metadata_nca_parameters = metadata_nca_parameters) {
  # Don't impute parameters that are not AUC dependent
  params_auc_dep <- metadata_nca_parameters %>%
    filter(grepl("auc|aumc", PKNCA) | grepl("auc", Depends)) %>%
    pull(PKNCA)

  params_not_to_impute <- metadata_nca_parameters %>%
    filter(
      !grepl("auc|aumc", PKNCA),
      !grepl(paste0(params_auc_dep, collapse = "|"), Depends)
    ) %>%
    pull(PKNCA) %>%
    intersect(names(PKNCA::get.interval.cols()))

  all_impute_methods <- na.omit(unique(data$intervals$impute))
  if (length(all_impute_methods) == 0) {
    return(data)
  }
  all_impute_methods <- all_impute_methods %>%
    strsplit(split = ",") %>%
    unlist() %>%
    trimws() %>%
    unique()

  data$intervals <- Reduce(function(d, ti_arg) {
    interval_remove_impute(
      d,
      target_impute = ti_arg,
      target_params = params_not_to_impute
    )
  }, all_impute_methods, init = data$intervals)

  data
}
