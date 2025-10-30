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
#' @param test_parameter Character. The PPTESTCD value to use as test (numerator).
#' @param ref_parameter Character. The PPTESTCD value to use as reference (denominator).
#' Defaults to test_parameter.
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

  # Define the reference and test data based on the parameters and groups
  df_ref <- as.data.frame(data)[data$PPTESTCD == ref_parameter, , drop = FALSE]
  df_ref <- merge(df_ref, ref_groups)

  df_test <- as.data.frame(data)[data$PPTESTCD == test_parameter, , drop = FALSE]
  df_test <- {
    if (!is.null(test_groups)) {
      merge(df_test, test_groups)
    } else {
      anti_join(df_test, ref_groups)
    }
  }

  # Define the key column(s) to use for group reference in PPANMETH
  ref_cols <- colnames(ref_groups[, !names(ref_groups) %in% match_cols, drop = FALSE])

  # Join test and denominator by their matching columns
  df_merged <- merge(df_test, df_ref, by = c(match_cols), suffixes = c("", "_ref"))

  # Early exit if df_merge is empty
  if (nrow(df_merged) == 0) {

    # Create an empty data frame based on df_test
    out_df <- df_test %>%
      slice(0) %>%
      mutate(
        # PPANMETH would have been in the resulting dataset
        PPANMETH = character(0),

        # Ensure PPORRESU and PSSTRESU have the right type
        PPORRESU = if ("PPORRESU" %in% names(.)) character(0) else NULL,
        PPSTRESU = if ("PPSTRESU" %in% names(.)) character(0) else NULL
      ) %>%
      select(any_of(c(names(df_test), "PPANMETH")))

    return(out_df)
  }

  merge(df_test, df_ref, by = c(match_cols), suffixes = c("", "_ref")) %>%
    # If possible compute conversion factors for the units of test and denominator
    mutate(
      PPORRESU_factor = get_conversion_factor(PPORRESU_ref, PPORRESU),
      PPSTRESU_factor = if ("PPSTRESU" %in% names(.)) {
        get_conversion_factor(PPSTRESU_ref, PPSTRESU)
      } else {
        NULL
      },
      PPORRES_ref = ifelse(!is.na(PPORRESU_factor), PPORRES_ref * PPORRESU_factor, PPORRES_ref),
      PPSTRES_ref = if ("PPSTRESU" %in% names(.)) {
        ifelse(!is.na(PPSTRESU_factor), PPSTRESU_factor * PPSTRES_ref, PPSTRES_ref)
      } else {
        NULL
      }
    ) %>%
    group_by(across(any_of(c(match_cols, group_cols, "PPTESTCD", paste0(group_cols, "_ref"))))) %>%
    unique() %>%
    # Use mean values in case of multiple denominator rows per test
    mutate(
      PPORRES_ref = mean(PPORRES_ref, na.rm = TRUE),
      PPSTRES_ref = mean(PPSTRES_ref, na.rm = TRUE),
      n = n()
    ) %>%
    ungroup() %>%
    mutate(
      PPORRES = (PPORRES / PPORRES_ref) * adjusting_factor,
      PPSTRES = if ("PPSTRES" %in% names(.)) {
        (PPSTRES / PPSTRES_ref) * adjusting_factor
      } else {
        NULL
      },
      PPORRESU = ifelse(!is.na(PPORRESU_factor), "fraction", paste0(PPORRESU, "/", PPORRESU_ref)),
      PPSTRESU = if ("PPSTRESU" %in% names(.)) {
        ifelse(!is.na(PPORRESU_factor), "fraction", paste0(PPSTRESU, "/", PPSTRESU_ref))
      } else {
        NULL
      }
    ) %>%
    rowwise() %>%
    mutate(
      ppanmeth_test_groups = paste0(
        paste(paste(ref_cols, c_across(all_of(ref_cols)), sep = ": "), collapse = ", ")
      ),
      ppanmeth_ref_groups = paste0(
        paste(
          paste(paste0(ref_cols), c_across(all_of(paste0(ref_cols, "_ref"))), sep = ": "),
          collapse = ", "
        )
      )
    ) %>%
    ungroup() %>%
    mutate(
      PPANMETH = ifelse(
        ppanmeth_test_groups == ppanmeth_ref_groups,
        paste0(PPTESTCD, " TO ", PPTESTCD_ref),
        paste0(PPTESTCD, " TO ", PPTESTCD_ref, " [", ppanmeth_ref_groups, "]")
      ),
      PPTESTCD = if (!is.null(custom_pptestcd)) {
        custom_pptestcd
      } else {
        ifelse(n > 1, paste0("RA", test_parameter, " (mean)"), paste0("RA", test_parameter))
      }
    ) %>%
    # Make sure all basic columns are still character (even when empty)
    mutate(
      PPORRESU = as.character(PPORRESU),
      PPSTRESU = as.character(PPSTRESU),
      PPANMETH = as.character(PPANMETH)
    ) %>%
    # Keep same format as the input (PKNCAresults)
    select(any_of(c(names(df_test), "PPANMETH"))) %>%
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
  if (!"PPANMETH" %in% names(data$result)) {
    data$result$PPANMETH <- ""
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
