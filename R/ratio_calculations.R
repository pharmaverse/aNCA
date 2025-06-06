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
calculate_ratios <- function(results, parameter, match_cols, denominator_groups, numerator_groups = NULL, adjusting_factor = 1, custom.pptestcd = NULL, custom.pptest = NULL) {
  UseMethod("calculate_ratios")
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
      PPSTRES = (PPSTRES / PPSTRES_den) * adjusting_factor,
      PPORRESU = ifelse(PPORRESU == PPORRESU_den, "fraction", paste0(PPORRESU, "/", PPORRESU_den)),
      PPSTRESU = ifelse(PPSTRESU == PPSTRESU_den, "fraction", paste0(PPSTRESU, "/", PPSTRESU_den)),
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

calculate_ratios.PKNCAresults <- function(PKNCAres, parameter, match_cols, denominator_groups, numerator_groups = NULL, adjusting_factor = 1, custom.pptestcd = NULL, custom.pptest = NULL) {
  # Check if match_cols and denominator_groups are valid group columns
    # Make checks on the input formats
  if (!all(c(match_cols, names(denominator_groups)) %in% names(PKNCA::getGroups(PKNCAres)))) {
    stop(paste0(
      "match_cols and denominator_groups must contain valid group column names in PKNCAres: ",
      paste(names(PKNCA::getGroups(PKNCAres)), collapse = ", ")
    ))
  }

  # Calculate ratios using the data.frame method
  results <- calculate_ratios.data.frame(
    PKNCAres$data$conc,
    parameter = parameter,
    match_cols = match_cols,
    denominator_groups = denominator_groups,
    numerator_groups = numerator_groups,
    adjusting_factor = adjusting_factor,
    custom.pptestcd = custom.pptestcd,
    custom.pptest = custom.pptest
  )

  # Update the PKNCA results with the new ratios
  PKNCAres$results <- bind_rows(PKNCAres$results, results)
  PKNCAres
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
  calculate_ratios.PKNCAresults(
    PKNCAres,
    parameter = parameter,
    match_cols = match_cols,
    denominator_groups = denominator_groups,
    numerator_groups = numerator_groups,
    adjusting_factor = adjusting_factor,
    custom.pptestcd = custom.pptestcd,
    custom.pptest = custom.pptest
  )
}
