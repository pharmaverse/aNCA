#' Calculate Matrix Ratios
#' This function calculates the  ratios for a given data set,
#' based on the shared time points for each matrix concentration sample.
#' The user can input multiple tissues for which ratios should be calculated.
#'
#' The ratios are calculated as specimen1 / specimen 2.
#'
#' @param data A data frame containing the concentration data.
#' @param matrix_col A character string specifying the column name for the matrix type.
#' @param conc_col A character string specifying the column name for the concentration data.
#' @param units_col A character string specifying the column name for the units.
#' @param groups A character vector of grouping variables to use for the analysis.
#'                  Must include time column, USUBJID, and optionally, other grouping variables.
#' @param spec1 A character string specifying the value for
#'               the first specimen type(s) in the matrix_col.
#' @param spec2 A character string specifying the value for
#'               the second specimen type(s) in the matrix_col.
#'
#' @returns A data frame containing the ratios.
#' @examples
#' data <- data.frame(
#' USUBJID = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A"),
#' NFRLT = c(0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2),
#' MATRIX = c("BLOOD", "BLOOD", "BLOOD", "PLASMA", "PLASMA", "PLASMA",
#'            "BRAIN", "BRAIN", "BRAIN", "LIVER", "LIVER", "LIVER"),
#' CONC = c(10, 20, 15, 25, 30, 40, 5, 10, 8, 12, 18, 16),
#' UNITS = rep("ng/mL", 12)
#' )
#' multiple_matrix_ratios(data, "MATRIX", "CONC", "UNITS", c("NFRLT", "USUBJID"), "BLOOD", "PLASMA")
#'
#' @import dplyr
#' @export
multiple_matrix_ratios <- function(data, matrix_col, conc_col, units_col,
                                   groups = c("NFRLT", "USUBJID"),
                                   spec1, spec2) {

  # Separate Samples
  df_spec1 <- data %>%
    filter(!!sym(matrix_col) %in% spec1) %>%
    rename(Spec1_Value = !!sym(conc_col),
           Spec1_Label = !!sym(matrix_col),
           Spec1_Units = !!sym(units_col)) %>%
    select(all_of(groups), Spec1_Value, Spec1_Label, Spec1_Units)

  df_spec2 <- data %>%
    filter(!!sym(matrix_col) %in% spec2) %>%
    rename(Spec2_Value = !!sym(conc_col),
           Spec2_Label = !!sym(matrix_col),
           Spec2_Units = !!sym(units_col)) %>%
    select(all_of(groups), Spec2_Value, Spec2_Label, Spec2_Units)

  # Merge Data
  left_join(df_spec1, df_spec2, by = groups, relationship = "many-to-many") %>%
    filter(!is.na(Spec1_Value) & !is.na(Spec2_Value)) %>%
    rowwise() %>%
    mutate(
      Ratio_Type = paste0(Spec1_Label, "/", Spec2_Label),
      Ratio = Spec1_Value / Spec2_Value,
      Ratio = signif(Ratio, 3)
    ) %>%
    filter(Spec1_Label != Spec2_Label) %>%
    select(all_of(groups), Ratio_Type, Spec1_Value, Spec1_Units, Spec2_Value, Spec2_Units, Ratio)
}

' Calculate Ratios from PKNCA Results
#'
#' This function calculates ratios of PPORRES values from a PKNCA results object (e.g., res$results),
#' matching rows according to user-specified parameters, matching columns, and denominator_groups variables.
#'
#' @param results A data.frame, typically res$results, containing PPORRES and grouping columns.
#' @param parameter Character. The PPTESTCD value to use for the calculation (e.g., "AUCINF").
#' @param match_cols Character vector of column names to match between numerator and denominator_groups, or a data.frame specifying columns and values to filter.
#' @param denominator_groups A data.frame specifying denominator_groups. At is minimum, it must contain the contrast variable value/s for the denominator.
#' @param numerator_groups A data.frame specifying numerator_groups. Optional argument. By default is NULL and all rows not in the denominator_groups will be used as numerator.
#' @param adjusting_factor Numeric. A factor to adjust the calculated ratios. Default is 1.
#'
#' @return A data.frame with the original columns, plus columns for numerator, denominator_groups, and the calculated ratio.
#'
#' @examples
#' # Example usage:
#' # calculate_ratios(res$results, parameter = "AUCINF", match_cols = c("USUBJID", "VISIT"), denominator_groups = c("TREATMENT"))
#' # calculate_ratios(res$results, parameter = "AUCINF", match_cols = c("USUBJID"), denominator_groups = data.frame(TREATMENT = "Placebo"))
#' @export
#' 
calculate_ratios <- function(data, parameter, match_cols, denominator_groups, numerator_groups = NULL, adjusting_factor = 1, custom.pptestcd = NULL, custom.pptest = NULL) {
  UseMethod("calculate_ratios", data)
}

calculate_ratios.data.frame <- function(data, parameter, match_cols, denominator_groups, numerator_groups = NULL, adjusting_factor = 1, custom.pptestcd = NULL, custom.pptest = NULL) {

  if (!any(data$PPTESTCD == parameter)) {
    warning(paste0("No parameter with PPTESTCD: '", paste(parameter, collapse = ","), "' is not found in the PKNCA results."))
  }
  contrast_var <- setdiff(names(denominator_groups), match_cols)
  if (length(contrast_var) < 1) {
    stop("The denominator_groups must contain at least one contrast variable that is not in match_cols.")
  }

  # Filter for the parameter of interest
  df <- data[data$PPTESTCD == parameter, ]

  # Define the denominator rows
  df_den <- merge(df, denominator_groups)

  # Define the numerator rows, which should exclude the denominator_groups
  if (!is.null(numerator_groups)) {
    df_num <- merge(df, numerator_groups)
  } else {
    df_num <- df
  }
  df_num <- anti_join(df_num, df_den, by = intersect(names(df_num), names(df_den)))

  # Join numerator and denominator by their matching columns
  merge(df_num, df_den, by = c(match_cols, "PPTESTCD"), suffixes = c("", "_den")) %>%

    # If possible compute conversion factors for the units of numerator and denominator
    mutate(
      PPORRESU_factor = get_conversion_factor(PPORRESU_den, PPORRESU),
      PPSTRESU_factor = if ("PPSTRESU" %in% names(.)) {
        get_conversion_factor(PPSTRESU_den, PPSTRESU)
      } else NULL,
      PPORRES_den = ifelse (!is.na(PPORRESU_factor), PPORRES_den * PPORRESU_factor, PPORRES_den),
      PPSTRES_den = if ("PPSTRESU" %in% names(.)) {
        ifelse(!is.na(PPSTRESU_factor), PPSTRESU_factor * PPSTRES_den, PPSTRES_den)
      } else NULL

    ) %>%
    group_by(across(all_of(c(match_cols, contrast_var, "PPTESTCD", paste0(contrast_var, "_den"))))) %>%
    unique() %>%
    # Use mean values in case of multiple denominator rows per numerator
    mutate(
      PPORRES_den = mean(PPORRES_den, na.rm = TRUE),
      PPSTRES_den = mean(PPSTRES_den, na.rm = TRUE),
      n = n()
    ) %>%
    ungroup() %>%
    mutate(
      PPORRES = (PPORRES / PPORRES_den) * adjusting_factor,
      PPSTRES = if ("PPSTRES" %in% names(.)) {
        (PPSTRES / PPSTRES_den) * adjusting_factor
      } else NULL,
      PPORRESU = ifelse(!is.na(PPORRESU_factor), "fraction", paste0(PPORRESU, "/", PPORRESU_den)),
      PPSTRESU = if ("PPSTRESU" %in% names(.)) {
        ifelse(!is.na(PPORRESU_factor), "fraction", paste0(PPSTRESU, "/", PPSTRESU_den))
      } else NULL,
      PPTESTCD = if (!is.null(custom.pptestcd)) {
        custom.pptestcd
      } else {
        ifelse(n > 1, paste0(PPTESTCD, "_RATIO (mean)"), paste0(PPTESTCD, "_RATIO"))
      },
      PPTEST = if (!is.null(custom.pptest)) {
        custom.pptest
      } else {
        ifelse(n > 1, paste0(PPTEST, " Ratio (mean)"), paste0(PPTEST, " Ratio"))
      }
    ) %>%
    # Keep same foramt as the input (PKNCAresults)
    select(any_of(names(df))) %>%
    unique()
}

calculate_ratios.PKNCAresults <- function(data, parameter, match_cols, denominator_groups, numerator_groups = NULL, adjusting_factor = 1, custom.pptestcd = NULL, custom.pptest = NULL) {
  # Check if match_cols and denominator_groups are valid group columns
  # Make checks on the input formats
  if (!all(c(match_cols, names(denominator_groups), names(numerator_groups)) %in% c(names(PKNCA::getGroups(data)), "start", "end"))) {
    stop(paste0(
      "match_cols and denominator_groups must contain valid group column names in PKNCAres: ",
      paste(names(PKNCA::getGroups(data)), collapse = ", ")
    ))
  }

  # Calculate ratios using the data.frame method
  ratios_result <- calculate_ratios.data.frame(
    data = data$result,
    parameter = parameter,
    match_cols = match_cols,
    denominator_groups = denominator_groups,
    numerator_groups = numerator_groups,
    adjusting_factor = adjusting_factor,
    custom.pptestcd = custom.pptestcd,
    custom.pptest = custom.pptest
  )

  # Update the PKNCA results with the new ratios
  data$result <- bind_rows(data$result, ratios_result)
  data
}


calculate_ratios_app <- function(PKNCAresults, parameter, contrast_var = "ANALYTE", reference_values = "A", match_cols = c("USUBJID", "start", "end"), aggregate_subject = "no", adjusting_factor = 1) {
  match_cols <- unique(c(dplyr::group_vars(PKNCAresults$data), "end"))
  if (aggregate_subject == "yes") {
    match_cols <- list(setdiff(match_cols, "USUBJID"))
  } else if (aggregate_subject == "no") {
    if (!"USUBJID" %in% match_cols) {
      stop("USUBJID must be included in match_cols when aggregate_subject is 'never'.")
    }
    match_cols <- list(match_cols)
  } else if (aggregate_subject == "if-needed") {
    if ("USUBJID" %in% match_cols) {
      # Perform both individual and aggregate calculations and then eliminate duplicates in the group columns
      match_cols <- list(match_cols, setdiff(match_cols, "USUBJID"))
    }
  }

  all_ratios <- data.frame()
  for (ix in seq_along(match_cols)) {
  ratio_calculations <- calculate_ratios(
    PKNCAresults, 
    parameter = parameter,
    match_cols = match_cols[[ix]],
    denominator_groups = data.frame(contrast_var = reference_values),
    numerator_groups = NULL,
    adjusting_factor = adjusting_factor,
    custom.pptestcd = NULL,
    custom.pptest = NULL
  )
  all_ratios <- bind_rows(all_ratios, ratio_calculations)
  }
  unnest(all_ratios) %>%
    # Make sure there are no duplicate rows for: parameter, contrast_var, and match_cols
    distinct(across(all_of(c(parameter, group_vars(PKNCAresults$data), "end", "impute"))), .keep_all = TRUE)
}

create_ratio_intervals <- function(PKNCAdata, parameter = "cmax", contrast_var = "PARAM", reference_values = "A", aggregate_subject = "never", adjusting_factor = 1) {
  if (!inherits(PKNCAdata, "PKNCAdata")) {
    stop("PKNCAdata must be a PKNCAdata object.")
  }
  params_available <- setdiff(names(PKNCA::get.interval.cols()), c("start", "end"))
  if (!all(parameter %in% params_available)) {
    param_missing <- setdiff(parameter, params_available)
    warning(paste0("No parameter with PPTESTCD: '", paste(param_missing, collapse = ","), "' is not found in the PKNCA intervals."))
  }
  contrast_var <- setdiff(names(denominator_groups), match_cols)
  if (length(contrast_var) < 1) {
    stop("The denominator_groups must contain at least one contrast variable that is not in match_cols.")
  }

  # Filter for the parameter of interest
  df <- data[data$PPTESTCD == parameter, ]

  # Define the denominator rows
  df_den <- merge(df, denominator_groups)

  # Define the numerator rows, which should exclude the denominator_groups
  if (!is.null(numerator_groups)) {
    df_num <- merge(df, numerator_groups)
  } else {
    df_num <- df
  }
  df_num <- anti_join(df_num, df_den, by = intersect(names(df_num), names(df_den)))

  # Join numerator and denominator by their matching columns
  merge(df_num, df_den, by = c(match_cols, "PPTESTCD"), suffixes = c("", "_den"))



  
  match_cols <- setdiff(c(names(PKNCA::getGroups(PKNCAdata)), "start", "end"), c(contrast_var))

  denominator_groups <- data.frame(contrast_var = reference_values)
  names(denominator_groups) <- contrast_var

  if (aggregate_subject == "always") {
    match_cols <- setdiff(match_cols, "USUBJID")
  } else if (aggregate_subject == "never") {
    if (!"USUBJID" %in% match_cols) {
      stop("USUBJID must be included in match_cols when aggregate_subject is 'never'.")
    } 
  } else if (aggregate_subject == "if_available") {
    
  }
}

# This is a wrapper for the PKNCAresults method
# calculate_ratios.PKNCAresults(
#   PKNCAres,
#   parameter = parameter,
#   match_cols = match_cols,
#   denominator_groups = denominator_groups,
#   numerator_groups = numerator_groups,
#   adjusting_factor = adjusting_factor,
#   custom.pptestcd = custom.pptestcd,
#   custom.pptest = custom.pptest
# )


#' Add interval row to PKNCA data intervals
#' #' This function adds a new interval row to the PKNCA data intervals.
#' @param PKNCAdata
#' @param groups Data frame containing the groups to be added.
#' @param parameters Character vector of parameter names to be added. If a different value to TRUE/1 is needed, it should be provided as a data.frame.
#' @param impute Character name of the imputate column value (if exists)
#'
#' @details Always recommended to apply the method on the PKNCAdata object, as it will also work in the addition of new groups that are not defined in the PKNCA data intervals.

add_interval_row <- function(o_data, groups, parameters, impute = NULL) {
  UseMethod("add_interval_row", o_data)
}

add_interval_row.data.frame <- function(o_data, groups, parameters, impute = NULL, ...) {

  # Perform checks on the groups input
  all_params <- setdiff(names(get.interval.cols()), c("start", "end"))
  group_names_o_data <- setdiff(names(o_data), all_params)
  missing_group_cols <- setdiff(names(groups), group_names_o_data)
  if (length(missing_group_cols) > 0) {
    stop(paste("The following group columns are missing in the PKNCA data:", paste(missing_group_cols, collapse = ", ")))
  }
  
  # Based on the groups data.frame, define all the actual groups matching in the PKNCA data
  groups_o_data <- o_data[, group_names_o_data, drop = FALSE]
  groups <- unique(left_join(groups, groups_o_data, by = names(groups)))
  missing_groups <- anyNA(groups)
  if (missing_groups) {
    stop(paste(
      "The following groups are not present in the intervals data.frame: ",
      paste(apply(groups, 1, paste, collapse = ", "), collapse = "; "),
      ". Please, if your groups are valid and completely defined try using the function on the PKNCAdata object."
    ))
  }
  
  # If specified consider the impute column for the matching
  if (!is.null(impute)) {
    groups <- bind_cols(groups, data.frame(impute = impute))
    if (!is.null(o_data$impute)) {
      o_data$intervals[["impute"]] <- NA_character
    }
  }
  
  # If parameters is not a data.frame, convert it to a data.frame with TRUE values
  if (!is.data.frame(parameters)) {
    parameters <- as.data.frame(matrix(TRUE, nrow = 1, ncol = length(parameters), dimnames = list(NULL, parameters)))
  }
  
  # Ensure that, unless already present, the parameters are added to the interval groups
  for (ix_parameter in seq_len(ncol(parameters))) {

    param <- parameters[, ix_parameter, drop = FALSE]
    groups_with_parameter <- cbind(groups, param)
    intervals_missing <- anti_join(groups_with_parameter, o_data, by = names(groups_with_parameter))
    ix_intervals_only_missing_param <- merge(
      intervals_missing[, names(groups), drop = FALSE],
      o_data %>% mutate(n = row_number())
    ) %>%
      pull(n)
    o_data[ix_intervals_only_missing_param, names(param)] <- param[[1]]
    intervals_missing_groups <- anti_join(groups_with_parameter, o_data[-ix_intervals_only_missing_param,], by = names(groups_with_parameter))
    o_data <- bind_rows(o_data, intervals_missing)
  }
  o_data
}

add_interval_row.PKNCAdata <- function(o_data, groups, parameters, impute = NULL, ...) {
  # Ensure that the groups are valid PKNCA groups
  groups <- unique(merge(groups, getGroups(o_data$conc)))
  
  # If specified consider the impute column for the matching
  if (!is.null(impute)) {
    if (!impute %in% names(o_data$data)) {
      if (!is.na(o_data$impute) && !is.null(o_data$impute)) {
        o_data$intervals[["impute"]] <- o_data$impute
      }
    }
  }
  o_data$intervals <- add_interval_row(o_data$intervals, groups, parameters, impute = impute)
  o_data
}

add_ratio_interval_row <- function(o_data, groups, parameters, impute = NULL) {
  UseMethod("add_ratio_interval_row", o_data)
}