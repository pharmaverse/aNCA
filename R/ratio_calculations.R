#' Calculate Matrix Ratios
#' This function calculates ratios for a given data set,
#' based on the shared time points for each matrix concentration sample.
#' The user can input two tissues for which ratios should be calculated.
#'
#' The ratios are calculated as specimen1 / specimen 2.
#'
#' @param data A data frame containing the concentration data.
#' @param matrix_col A character string specifying the column name for the matrix type.
#' @param conc_col A character string specifying the column name for the concentration data.
#' @param units_col A character string specifying the column name for the units.
#' @param groups A character vector of grouping variables to use for the analysis.
#'                  Must include TIME, USUBJID, and optionally, other grouping variables.
#' @param spec1 A character string specifying the value for
#'             the first specimen type in the matrix_col.
#' @param spec2 A character string specifying the value for
#'             the second specimen type in the matrix_col.
#'
#' @returns A data frame containing the ratios.
#'
#' @examples
#' data <- data.frame(
#'  USUBJID = c("A", "A", "A", "A", "A", "A"),
#'  TIME = c(0, 1, 2, 0, 1, 2),
#'  MATRIX = c("BLOOD", "BLOOD", "BLOOD", "PLASMA", "PLASMA", "PLASMA"),
#'  CONC = c(10, 20, 15, 25, 30, 40)
#'  )
#' single_matrix_ratio(data, "MATRIX", "CONC", groups = c("TIME", "USUBJID"), "BLOOD", "PLASMA")
#'
#' @import dplyr
#' @export
single_matrix_ratio <- function(data,
                                matrix_col, conc_col, units_col,
                                groups = c("TIME", "USUBJID"),
                                spec1, spec2) {

  # Separate Matrix Samples
  df_spec1 <- data %>%
    filter(!!sym(matrix_col) == spec1) %>%
    rename(!!spec1 := !!sym(conc_col),
           Spec1_Unit = !!sym(units_col)) %>%
    select(all_of(groups), !!sym(spec1), Spec1_Unit)

  df_spec2 <- data %>%
    filter(!!sym(matrix_col) == spec2) %>%
    rename(!!spec2 := !!sym(conc_col),
           Spec2_Unit = !!sym(units_col)) %>%
    select(all_of(groups), !!sym(spec2), Spec2_Unit)

  # Merge  Data
  df_ratio <- left_join(df_spec1, df_spec2, by = groups) %>%
    filter(!is.na(!!sym(spec1)) & !is.na(!!sym(spec2))) %>%
    mutate(
      Ratio = signif(!!sym(spec1) / !!sym(spec2), 3),
      Unit = paste0("(", Spec1_Unit, ") / (", Spec2_Unit, ")")
    ) %>%
    select(all_of(groups), !!sym(spec1), !!sym(spec2), Ratio, Unit)

  df_ratio
}

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
#'                  Must include TIME, USUBJID, and optionally, other grouping variables.
#' @param spec1 A character string specifying the value for
#'               the first specimen type(s) in the matrix_col.
#' @param spec2 A character string specifying the value for
#'               the second specimen type(s) in the matrix_col.
#'
#' @returns A data frame containing the ratios.
#'
#' @import dplyr
#' @export
multiple_matrix_ratios <- function(data, matrix_col, conc_col, units_col,
                                   groups = c("TIME", "USUBJID"),
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
  df_ratio <- left_join(df_spec1, df_spec2, by = groups, relationship = "many-to-many") %>%
    filter(!is.na(Spec1_Value) & !is.na(Spec2_Value)) %>%
    mutate(
      Ratio = signif(Spec1_Value / Spec2_Value, 3),
      `Spec1:Spec2` = paste0(Spec1_Label, "-", Spec2_Label),
      Unit = paste0("(", Spec1_Units, ") / (", Spec2_Units, ")")
    ) %>%
    select(all_of(groups), `Spec1:Spec2`, Spec1_Value, Spec2_Value, Ratio, Unit)

  df_ratio
}
