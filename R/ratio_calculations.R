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

" Calculate Ratios from PKNCA Results
#"
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
calculate_ratios <- function(data, parameter, match_cols, denominator_groups, numerator_groups = NULL, adjusting_factor = 1, custom.pptestcd = NULL) {
  UseMethod("calculate_ratios", data)
}

calculate_ratios.data.frame <- function(data, parameter, match_cols, denominator_groups, numerator_groups = NULL, adjusting_factor = 1, custom.pptestcd = NULL) {
  if (!any(data$PPTESTCD == parameter)) {
    warning(paste0("No parameter with PPTESTCD: '", paste(parameter, collapse = ","), "' is not found in the PKNCA results."))
  }

  # Deduce what are all the group columns in the result data.frame
  extra_res_cols <- c("PPTEST", "PPORRES", "PPORRESU", "PPSTRES", "PPSTRESU", "type_interval", "exclude")
  group_cols <- setdiff(colnames(data), extra_res_cols)

  # Filter for the parameter of interest
  df <- data[data$PPTESTCD == parameter, ]

  # Define the denominator rows
  df_den <- merge(df, denominator_groups)

  # Define the numerator rows, which should exclude the denominator_groups
  if (!is.null(numerator_groups)) {
    df_num <- merge(df, numerator_groups)
  } else {
    df_num <- df
    df_num <- anti_join(df_num, df_den, by = intersect(names(df_num), names(df_den)))
  }

  # Join numerator and denominator by their matching columns
  merge(df_num, df_den, by = c(match_cols, "PPTESTCD"), suffixes = c("", "_den")) %>%
    # If possible compute conversion factors for the units of numerator and denominator
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
    group_by(across(any_of(c(match_cols, group_cols, "PPTESTCD", paste0(group_cols, "_den"))))) %>%
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
      } else {
        NULL
      },
      PPORRESU = ifelse(!is.na(PPORRESU_factor), "fraction", paste0(PPORRESU, "/", PPORRESU_den)),
      PPSTRESU = if ("PPSTRESU" %in% names(.)) {
        ifelse(!is.na(PPORRESU_factor), "fraction", paste0(PPSTRESU, "/", PPSTRESU_den))
      } else {
        NULL
      },
      PPTESTCD = if (!is.null(custom.pptestcd)) {
        custom.pptestcd
      } else {
        ifelse(n > 1, paste0("RA", PPTESTCD, " (mean)"), paste0("RA", PPTESTCD))
      }
    ) %>%
    # Keep same foramt as the input (PKNCAresults)
    select(any_of(names(df))) %>%
    unique()
}

calculate_ratios.PKNCAresults <- function(data, parameter, match_cols, denominator_groups, numerator_groups = NULL, adjusting_factor = 1, custom.pptestcd = NULL) {
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
    custom.pptestcd = custom.pptestcd
  )

  # Update the PKNCA results with the new ratios
  data$result <- bind_rows(data$result, ratios_result)
  data
}

calculate_ratio_app <- function(res, parameter, numerator = "(all)", reference = "PARAM: Analyte01", aggregate_subject = "no", adjusting_factor = 1.4, custom.pptestcd = NULL) {
  reference_colname <- gsub("(.*): (.*)", "\\1", reference)
  match_cols <- setdiff(unique(c(dplyr::group_vars(res), "start", "end")), reference_colname)

  ########### This is very App specific ###############
  if ("NCA_PROFILE" %in% reference_colname) {
    match_cols <- setdiff(match_cols, c("start", "end"))
  }
  if ("ROUTE" %in% reference_colname && aggregate_subject == "no") {
    match_cols <- setdiff(match_cols, c("start", "end"))
  }
  #####################################################

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

  if (numerator == "(all)") {
    numerator_groups <- NULL
  } else {
    num_colname <- gsub("(.*): (.*)", "\\1", numerator)
    num_value <- gsub("(.*): (.*)", "\\2", numerator)
    numerator_groups <- data.frame(
      matrix(
        num_value,
        nrow = 1,
        ncol = length(num_colname),
        dimnames = list(NULL, num_colname)
      )
    )
  }

  reference_colname <- gsub("(.*): (.*)", "\\1", reference)
  reference_value <- gsub("(.*): (.*)", "\\2", reference)
  denominator_groups <- data.frame(
    matrix(
      reference_value,
      nrow = 1,
      ncol = length(reference_colname),
      dimnames = list(NULL, reference_colname)
    )
  )


  all_ratios <- data.frame()
  for (ix in seq_along(match_cols)) {
    ratio_calculations <- calculate_ratios(
      data = res$result,
      parameter = parameter,
      match_cols = match_cols[[ix]],
      denominator_groups = denominator_groups,
      numerator_groups = numerator_groups,
      adjusting_factor = adjusting_factor,
      custom.pptestcd = custom.pptestcd
    )
    all_ratios <- bind_rows(all_ratios, ratio_calculations)
  }
  # Assuming there cannot be more than 1 reference + PPTESTCD combination for the same group...
  # If aggregate_subject = 'if-needed', then this will remove cases when subject is not needed
  unnest(all_ratios) %>%
    # Make sure there are no duplicate rows for: parameter, contrast_var, and match_cols
    distinct(across(all_of(
      c("PPTESTCD", group_vars(res$data), "end"))
    ), .keep_all = TRUE)
}

#' Apply Ratio Calculations to PKNCAresult Object
#'
#' This function takes a PKNCAresult object and a data.frame containing ratio calculation parameters,
#' applies the `calculate_ratio_app` function for each row, and updates the PKNCAresult object.
#'
#' @param res A PKNCAresult object.
#' @param ratio_table A data.frame containing columns: Parameter, Reference, Numerator, AggregateSubject, AdjustingFactor.
#' @return The updated PKNCAresult object with added rows in the `result` data.frame.
#' @export
calculate_table_ratios_app <- function(res, ratio_table) {

  # Make a list to save all results
  ratio_results <- vector("list", nrow(ratio_table))

  # Loop through each row of the ratio_table
  for (i in seq_len(nrow(ratio_table))) {

    ratio_results[[i]] <- calculate_ratio_app(
      res = res,
      parameter = ratio_table$Parameter[i],
      numerator = ratio_table$Numerator[i],
      reference = ratio_table$Reference[i],
      aggregate_subject = ratio_table$AggregateSubject[i],
      adjusting_factor = as.numeric(ratio_table$AdjustingFactor[i]),
      custom.pptestcd = if (ratio_table$PPTESTCD[i] == "") NULL else ratio_table$PPTESTCD[i]
    )
  }
browser()
  # Combine all results into the original PKNCAresult object
  res$result <- do.call(rbind, c(list(res$result), ratio_results))
  res
}
