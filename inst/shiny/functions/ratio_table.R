#' Calculate a Ratio for the App. These functions communicate the table UI with the PKNCA via of ratio calculation
#'
#' @param res A PKNCAresult object.
#' @param parameter Character. The PPTESTCD value to use for the calculation.
#' @param test Character. The test group (numerator). Default is "(all other levels)".
#' @param reference Character. The reference group (denominator).
#' @param aggregate_subject Character. Aggregation mode: "yes", "no", or "if-needed".
#' @param adjusting_factor Numeric that multiplies the calculated ratio. Default is 1.
#' @param custom_pptestcd Optional character. If provided, will be used as the PPTESTCD value.
#' @returns A data.frame with the calculated ratios for the specified settings.
calculate_ratio_app <- function(
  res,
  parameter,
  test = "(all other levels)",
  reference = "PARAM: Analyte01",
  aggregate_subject = "no",
  adjusting_factor = 1,
  custom_pptestcd = NULL
) {
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
      # Perform both individual & aggregated calculations, then eliminate duplicates
      match_cols <- list(match_cols, setdiff(match_cols, "USUBJID"))
    }
  }

  if (test == "(all other levels)") {
    test_groups <- NULL
  } else {
    num_colname <- gsub("(.*): (.*)", "\\1", test)
    num_value <- gsub("(.*): (.*)", "\\2", test)
    test_groups <- data.frame(
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
  ref_groups <- data.frame(
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
      ref_groups = ref_groups,
      test_groups = test_groups,
      adjusting_factor = adjusting_factor,
      custom_pptestcd = custom_pptestcd
    )
    all_ratios <- bind_rows(all_ratios, ratio_calculations)
  }
  # Assuming there cannot be more than 1 reference + PPTESTCD combination for the same group...
  # If aggregate_subject = 'if-needed', then this will remove cases when subject is not needed
  unnest(all_ratios) %>%
    # Make sure there are no duplicate rows for: parameter, contrast_var, and match_cols
    distinct(across(
      all_of(c("PPTESTCD", group_vars(res$data), "end"))
    ),
    .keep_all = TRUE)
}

#' Apply Ratio Calculations to PKNCAresult Object
#'
#' This function takes a PKNCAresult object and a data.frame specifying ratio calculations
#'
#' @param res A PKNCAresult object.
#' @param ratio_table Data.frame with columns:
#' Parameter, Reference, Test, AggregateSubject, AdjustingFactor.
#' @returns The updated PKNCAresult object with added rows in the `result` data.frame.
#' @export
calculate_table_ratios_app <- function(res, ratio_table) {

  # Make a list to save all results
  ratio_results <- vector("list", nrow(ratio_table))

  # Loop through each row of the ratio_table
  for (i in seq_len(nrow(ratio_table))) {

    ratio_results[[i]] <- calculate_ratio_app(
      res = res,
      parameter = ratio_table$Parameter[i],
      test = ratio_table$Test[i],
      reference = ratio_table$Reference[i],
      aggregate_subject = ratio_table$AggregateSubject[i],
      adjusting_factor = as.numeric(ratio_table$AdjustingFactor[i]),
      custom_pptestcd = if (ratio_table$PPTESTCD[i] == "") NULL else ratio_table$PPTESTCD[i]
    )
  }

  # Combine all results into the original PKNCAresult object
  res$result <- do.call(rbind, c(list(res$result), ratio_results))
  res
}
