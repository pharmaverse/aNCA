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
#'   USUBJID = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A"),
#'   NFRLT = c(0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2),
#'   MATRIX = c(
#'     "BLOOD", "BLOOD", "BLOOD", "PLASMA", "PLASMA", "PLASMA",
#'     "BRAIN", "BRAIN", "BRAIN", "LIVER", "LIVER", "LIVER"
#'   ),
#'   CONC = c(10, 20, 15, 25, 30, 40, 5, 10, 8, 12, 18, 16),
#'   UNITS = rep("ng/mL", 12)
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
    rename(
      Spec1_Value = !!sym(conc_col),
      Spec1_Label = !!sym(matrix_col),
      Spec1_Units = !!sym(units_col)
    ) %>%
    select(all_of(groups), Spec1_Value, Spec1_Label, Spec1_Units)

  df_spec2 <- data %>%
    filter(!!sym(matrix_col) %in% spec2) %>%
    rename(
      Spec2_Value = !!sym(conc_col),
      Spec2_Label = !!sym(matrix_col),
      Spec2_Units = !!sym(units_col)
    ) %>%
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

#' Calculate Ratios from PKNCA Results
#'
#' @param data A PKNCAresults object or its result data.frame.
#' @param test_parameter Character. The PPTESTCD value to use for the test (numerator) calculation (e.g., "AUCINF").
#' @param ref_parameter Character. The PPTESTCD value to use for the reference (denominator) calculation. Defaults to test_parameter.
#' @param match_cols Character vector of column names to match between test and reference groups
#'  or a data.frame specifying columns and values.
#' @param ref_groups A data.frame specifying reference groups.
#' At its minimum, contains the contrast variable value(s) for the reference.
#' @param test_groups A data.frame specifying test groups. Optional.
#' By default is NULL, allowing rows not in ref_groups be used as test.
#' @param adjusting_factor Numeric to multiply the ratio. Default is 1.
#' @param custom_pptestcd Optional character. If provided, will be used as the PPTESTCD value.
#' @returns A data.frame result object with the calculated ratios.
#' @export
#' @export
calculate_ratios <- function(
  data,
  test_parameter,
  ref_parameter = test_parameter,
  match_cols,
  ref_groups,
  test_groups = NULL,
  adjusting_factor = 1,
  custom_pptestcd = NULL
) {
  UseMethod("calculate_ratios", data)
}
#' @export

#' @export
calculate_ratios.data.frame <- function(
  data,
  test_parameter,
  ref_parameter = test_parameter,
  match_cols,
  ref_groups,
  test_groups = NULL,
  adjusting_factor = 1,
  custom_pptestcd = NULL
) {
  if (!any(data$PPTESTCD == test_parameter)) {
    warning(
      paste0(
        "No test_parameter with PPTESTCD: '",
        paste(test_parameter, collapse = ","),
        "' found in the PKNCA results."
      )
    )
  }
  if (!any(data$PPTESTCD == ref_parameter)) {
    warning(
      paste0(
        "No ref_parameter with PPTESTCD: '",
        paste(ref_parameter, collapse = ","),
        "' found in the PKNCA results."
      )
    )
  }

  # Deduce what are all the group columns in the result data.frame
  extra_res_cols <- c(
    "PPTEST", "PPORRES", "PPORRESU", "PPSTRES", "PPSTRESU", "type_interval", "exclude"
  )
  group_cols <- setdiff(colnames(data), extra_res_cols)

  # Filter for the test and reference parameters
  df_test <- data[data$PPTESTCD == test_parameter, ]
  df_ref <- data[data$PPTESTCD == ref_parameter, ]

  # Define the denominator rows
  df_den <- merge(df_ref, ref_groups)

  # Define the test rows, which should exclude the ref_groups
  df_num <- {
    if (!is.null(test_groups)) {
      merge(df_test, test_groups)
    } else {
      anti_join(df_test, df_den, by = intersect(names(df_test), names(df_den)))
    }
  }

  # Join test and denominator by their matching columns
  merge(df_num, df_den, by = c(match_cols), suffixes = c("", "_den")) %>%
    # If possible compute conversion factors for the units of test and denominator
    mutate(
      PPORRESU_factor = get_conversion_factor(PPORRESU_den, PPORRESU),
      PPSTRESU_factor = if ("PPSTRESU" %in% names(.)) {
        get_conversion_factor(PPSTRESU_den, PPSTRESU)
      } else {
        NULL
      },
      PPORRES_den = ifelse(!is.na(PPORRESU_factor), PPORRES_den * PPORRESU_factor, PPORRES_den),
      PPSTRES_den = if ("PPSTRESU" %in% names(.)) {
        ifelse(!is.na(PPSTRESU_factor), PPSTRESU_factor * PPSTRES_den, PPSTRES_den)
      } else {
        NULL
      }
    ) %>%
    group_by(across(any_of(c(match_cols, group_cols, "PPTESTCD", paste0(group_cols, "_den")))) ) %>%
    unique() %>%
    # Use mean values in case of multiple denominator rows per test
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
      } else {
        NULL
      },
      PPORRESU = ifelse(!is.na(PPORRESU_factor), "fraction", paste0(PPORRESU, "/", PPORRESU_den)),
      PPSTRESU = if ("PPSTRESU" %in% names(.)) {
        ifelse(!is.na(PPORRESU_factor), "fraction", paste0(PPSTRESU, "/", PPSTRESU_den))
      } else {
        NULL
      },
      PPTESTCD = if (!is.null(custom_pptestcd)) {
        custom_pptestcd
      } else {
        ifelse(n > 1, paste0("RA", test_parameter, " (mean)"), paste0("RA", test_parameter))
      }
    ) %>%
    # Keep same format as the input (PKNCAresults)
    select(any_of(names(df_test))) %>%
    unique()
}
#' @export

#' @export
calculate_ratios.PKNCAresults <- function(
  data,
  test_parameter,
  ref_parameter = test_parameter,
  match_cols,
  ref_groups,
  test_groups = NULL,
  adjusting_factor = 1,
  custom_pptestcd = NULL
) {
  # Check if match_cols and ref_groups are valid group columns
  # Make checks on the input formats
  cols_used_for_ratios <- c(match_cols, names(ref_groups), names(test_groups))
  if (!all(cols_used_for_ratios %in% c(names(PKNCA::getGroups(data)), "start", "end"))) {
    stop(paste0(
      "match_cols and ref_groups must contain valid group column names in PKNCAres: ",
      paste(names(PKNCA::getGroups(data)), collapse = ", ")
    ))
  }

  # Calculate ratios using the data.frame method
  ratios_result <- calculate_ratios.data.frame(
    data = data$result,
    test_parameter = test_parameter,
    ref_parameter = ref_parameter,
    match_cols = match_cols,
    ref_groups = ref_groups,
    test_groups = test_groups,
    adjusting_factor = adjusting_factor,
    custom_pptestcd = custom_pptestcd
  )

  # Update the PKNCA results with the new ratios
  data$result <- bind_rows(data$result, ratios_result)
  data
}
