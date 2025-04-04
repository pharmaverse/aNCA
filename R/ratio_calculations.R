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
