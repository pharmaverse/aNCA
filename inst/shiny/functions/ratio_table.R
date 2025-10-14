#' Links the table ratio of the App with the ratio calculations via PKNCA results
#'
#' @param res A PKNCAresult object.
#' @param test_parameter Character. The PPTESTCD value to use as test (numerator).
#' @param ref_parameter Character. The PPTESTCD value to use as reference (denominator).
#' Defaults to test_parameter.
#' @param test_group Character. The test group (numerator). Default is "(all other levels)".
#' @param ref_group Character. The reference group (denominator).
#' @param aggregate_subject Character. Aggregation mode: "yes", "no", or "if-needed".
#' @param adjusting_factor Numeric that multiplies the calculated ratio. Default is 1.
#' @param custom_pptestcd Optional character. If provided, will be used as the PPTESTCD value.
#' @returns A data.frame with the calculated ratios for the specified settings.
calculate_ratio_app <- function(
  res,
  test_parameter,
  ref_parameter = test_parameter,
  test_group = "(all other levels)",
  ref_group = "PARAM: Analyte01",
  aggregate_subject = "no",
  adjusting_factor = 1,
  custom_pptestcd = NULL
) {
  reference_colname <- gsub("(.*): (.*)", "\\1", ref_group)
  match_cols <- setdiff(unique(c(dplyr::group_vars(res), "start", "end")), reference_colname)

  ########### This is very App specific ###############
  if ("AVISIT" %in% reference_colname) {
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

  if (test_group == "(all other levels)") {
    test_groups <- NULL
  } else {
    num_colname <- gsub("(.*): (.*)", "\\1", test_group)
    num_value <- gsub("(.*): (.*)", "\\2", test_group)
    test_groups <- data.frame(
      matrix(
        num_value,
        nrow = 1,
        ncol = length(num_colname),
        dimnames = list(NULL, num_colname)
      )
    )
  }

  reference_colname <- gsub("(.*): (.*)", "\\1", ref_group)
  reference_value <- gsub("(.*): (.*)", "\\2", ref_group)
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
      test_parameter = test_parameter,
      ref_parameter = ref_parameter,
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
  all_ratios %>%
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
#' TestParameter, RefParameter, Reference, Test, AggregateSubject, AdjustingFactor.
#' @returns The updated PKNCAresult object with added rows in the `result` data.frame.
#' @export
calculate_table_ratios_app <- function(res, ratio_table) {
  # Make a list to save all results
  ratio_results <- vector("list", nrow(ratio_table))

  # Loop through each row of the ratio_table
  for (i in seq_len(nrow(ratio_table))) {
    ratio_results[[i]] <- calculate_ratio_app(
      res = res,
      test_parameter = ratio_table$TestParameter[i],
      ref_parameter = ratio_table$RefParameter[i],
      test_group = ratio_table$TestGroups[i],
      ref_group = ratio_table$RefGroups[i],
      aggregate_subject = ratio_table$AggregateSubject[i],
      adjusting_factor = as.numeric(ratio_table$AdjustingFactor[i]),
      custom_pptestcd = if (ratio_table$PPTESTCD[i] == "") NULL else ratio_table$PPTESTCD[i]
    )

    if (nrow(ratio_results[[i]]) == 0) {
      warning(
        "Ratio ", ratio_table$PPTESTCD[i], " not computed.",
        "No comparable groups found between RefGroups",
        " (", ratio_table$RefGroups[i], ")",
        "and TestGroups",
        " (", ratio_table$TestGroups[i], ")"
      )
    }
  }
  if (!"PPANMETH" %in% names(res$result)) {
    res$result$PPANMETH <- ""
  }

  # Combine all results into the original PKNCAresult object
  res$result <- do.call(rbind, c(list(res$result), ratio_results))
  res
}
