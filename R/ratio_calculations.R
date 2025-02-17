#' Calculate Blood to Plasma Ratios
#' This function calculates the blood to plasma ratios for a given data set,
#' based on the shared time points for each matrix concentration sample.
#' 
#' @param data A data frame containing the concentration data.
#' @param matrix_col A character string specifying the column name for the matrix type.
#' @param conc_col A character string specifying the column name for the concentration data.
#' @param bpp_groups A character vector of grouping variables to use for the analysis.
#'                  Must include TIME, USUBJID, and optionally, other grouping variables.
#' @param blood A character string specifying the value for the blood matrix in matrix_col.
#' @param plasma A character string specifying the value for the plasma matrix in matrix_col.
#' 
#' @returns A data frame containing the blood to plasma ratios.
#' 
#' @examples
#' data <- data.frame(
#'  USUBJID = c("A", "A", "A", "A", "A", "A"),
#'  TIME = c(0, 1, 2, 0, 1, 2),
#'  MATRIX = c("BLOOD", "BLOOD", "BLOOD", "PLASMA", "PLASMA", "PLASMA"),
#'  CONC = c(10, 20, 15, 25, 30, 40)
#'  )
#' bpp_ratios(data, "MATRIX", "CONC", bpp_groups = c("TIME", "USUBJID"), "BLOOD", "PLASMA")
#' 
#' @import dplyr
#' @export
bpp_ratios <- function(data, matrix_col, conc_col,
                       bpp_groups = c("TIME", "USUBJID"),
                       blood, plasma) {
  
  # Separate Blood and Plasma Samples
  df_blood <- data %>%
    filter(!!sym(matrix_col) == blood) %>%
    rename(BLOOD_CONC = !!sym(conc_col)) %>%
    select(all_of(bpp_groups), BLOOD_CONC)
  
  df_plasma <- data %>%
    filter(!!sym(matrix_col) == plasma) %>%
    rename(PLASMA_CONC = !!sym(conc_col)) %>%
    select(all_of(bpp_groups), PLASMA_CONC)
  
  # Merge Blood and Plasma Data
  df_bpp <- left_join(df_blood, df_plasma, by = bpp_groups) %>%
    filter(!is.na(BLOOD_CONC) & !is.na(PLASMA_CONC)) %>%
    mutate(
      BPP_RATIO = signif(BLOOD_CONC / PLASMA_CONC, 3)
    ) %>%
    select(all_of(bpp_groups), PLASMA_CONC, BLOOD_CONC, BPP_RATIO)
  
  return(df_bpp)
}
