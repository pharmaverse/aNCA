#' Validate PKNCA Parameters
#'
#' This function validates that the reshaped PKNCA results contain the expected values
#' for each parameter based on the original `pknca_res` object.
#'
#' @param pknca_res The original PKNCA results object.
#' @param pivoted_res The pivoted results from `pivot_wider_pknca_results`.
#' @returns A logical value indicating whether all checks passed.
.validate_pknca_params <- function(pknca_res, pivoted_res) {

  # Extract unique parameter names with units
  names_params <- pknca_res$result %>%
    mutate(
      PPSTRESU2 = ifelse(
        PPSTRESU %in% c("", "unitless", "fraction"),
        "",
        paste0("[", PPSTRESU, "]")
      ),
      PPTESTCD2 = ifelse(
        type_interval == "manual",
        paste0(PPTESTCD, "_", start_dose, "-", end_dose, PPSTRESU2),
        paste0(PPTESTCD, PPSTRESU2)
      ),
    ) %>%
    pull(PPTESTCD2) %>%
    unique()

  # Iterate over each parameter and validate values
  for (param_col in names_params) {
    pptestcd <- gsub("\\[.*\\]", "", param_col)  # Remove units from parameter name

    if (grepl("AUCINT_[0-9]+\\-[0-9]", param_col)) {
      auc_start <- gsub("AUCINT_([0-9]+)\\-[0-9]", "\\1", pptestcd)
      auc_end <- gsub("AUCINT_[0-9]+\\-([0-9]+)", "\\1", pptestcd)
      pptestcd <- gsub("_[0-9]+\\-[0-9]", "", pptestcd)

      original_data <- pknca_res$result %>%
        filter(
          start_dose == auc_start,
          end_dose == auc_end,
          type_interval == "manual"
        )
      reshaped_data <- pivoted_res %>%
        filter(!is.na(!!sym(param_col)))
    } else {
      original_data <- pknca_res$result
      reshaped_data <- pivoted_res
    }

    original_vals <- original_data %>%
      formatters::var_labels_remove() %>%
      filter(PPTESTCD == pptestcd) %>%
      pull(PPSTRES)
    result_vals <- reshaped_data %>%
      formatters::var_labels_remove() %>%
      pull(param_col)

    have_similar_vals <- all(round(result_vals, 3) %in% round(original_vals, 3))
    have_same_length <- length(result_vals) == length(original_vals)
    if (!(have_similar_vals && have_same_length)) {
      stop(paste("Mismatch in values for parameter:", param_col))
    }
  }
}


describe("pivot_wider_pknca_results", {

  pknca_res <- FIXTURE_PKNCA_RES %>%
    filter(USUBJID %in% 1:5)
  pivoted_res <- expect_no_error(pivot_wider_pknca_results(pknca_res))

  # Store the expected parameter columns in pivoted_res
  exp_main_param_cols <- pknca_res$result %>%
    filter(type_interval == "main") %>%
    mutate(param_col = ifelse(
      PPSTRESU != "",
      paste0(PPTESTCD, "[", PPSTRESU, "]"),
      PPTESTCD
    )) %>%
    pull(param_col) %>%
    unique()

  it("produces a data.frame", {
    expect_s3_class(pivoted_res, "data.frame")
  })

  it("produces a data.frame with expected format when only reshaping main intervals", {
    res_only_main <- pknca_res
    res_only_main$result <- pknca_res$result  %>%
      filter(type_interval == "main")
    pivoted_res_only_main <- pivot_wider_pknca_results(res_only_main)
    expect_s3_class(pivoted_res_only_main, "data.frame")

    # Check that the result contains expected columns
    group_columns <- PKNCA::getGroups(res_only_main$data$conc)
    expected_colnames <- c(
      "start", "end",
      # Parameter columns transformed by the function
      exp_main_param_cols,
      # Columns currently made by the function
      "LAMZIX", "LAMZMTD", "Exclude",
      # Concentration group columns
      colnames(group_columns),
      # Columns from dose data
      intersect(colnames(res_only_main$data$dose$data), colnames(pivoted_res_only_main))
    )
    expect_setequal(colnames(pivoted_res_only_main), expected_colnames)

    # Check number of rows is the expected (1 per results group)
    expect_equal(nrow(pivoted_res_only_main), nrow(unique(PKNCA::getGroups(res_only_main))))

    # Check parameter values match the ones in the PKNCA results object
    expect_no_error(.validate_pknca_params(res_only_main, pivoted_res_only_main))
  })

  it("reshapes PKNCA results correctly when also considering AUC intervals", {
    # Check that the result is a data frame
    expect_s3_class(pivoted_res, "data.frame")

    # Check that the result contains expected columns
    group_columns <- PKNCA::getGroups(pknca_res$data$conc)
    expected_colnames <- c(
      "start", "end",
      # Parameter columns transformed by the function
      exp_main_param_cols,
      "AUCINT_0-2[hr*ng/mL]", "AUCINT_2-4[hr*ng/mL]",
      # Columns currently made by the function
      "LAMZIX", "LAMZMTD", "Exclude",
      colnames(group_columns),
      intersect(colnames(pknca_res$data$dose$data), colnames(pivoted_res))
    )
    expect_setequal(colnames(pivoted_res), expected_colnames)
    main_res <- filter(pknca_res, type_interval == "main")
    expect_equal(nrow(pivoted_res), nrow(unique(PKNCA::getGroups(main_res))))

    expect_no_error(.validate_pknca_params(pknca_res, pivoted_res))
  })

  it("rounds numeric values to three decimals", {
    expected_num_param_cols <- c(
      "CMAX[ng/mL]", "TMAX[hr]", "TLST[hr]", "LAMZ[1/hr]",
      "R2", "R2ADJ", "LAMZLL[hr]", "LAMZNPT[count]", "LAMZSPN",
      "AUCINT_0-2[hr*ng/mL]", "AUCINT_2-4[hr*ng/mL]"
    )
    actual_values <- pivoted_res[, expected_num_param_cols]
    expected_values <- round(actual_values, 3)
    diff_values <- actual_values - expected_values

    all_non_nan_values_are_rounded <- all(is.na(diff_values) | diff_values == 0)
    expect_true(all_non_nan_values_are_rounded)
  })

  it("adds appropriate labels to columns (CDISC PPTEST)", {
    labels <- formatters::var_labels(pivoted_res)
    expected_labels <- c(
      USUBJID = NA, PARAM = NA, start = NA, end = NA, NCA_PROFILE = NA, DOSNOA = NA,
      AFRLT = NA, ARRLT = NA, NFRLT = NA, NRRLT = NA, ROUTE = NA,
      DOSEA = NA, DRUG = NA, ADOSEDUR = NA, DOSEU = NA,
      `AUCLST[hr*ng/mL]` = "AUC to Last Nonzero Conc",
      `CMAX[ng/mL]` = "Max Conc",
      `TMAX[hr]` = "Time of CMAX",
      `TLST[hr]` = "Time of Last Nonzero Conc",
      `CLST[ng/mL]` = "Last Nonzero Conc",
      `LAMZ[1/hr]` = "Lambda z",
      `R2` = "R Squared",
      `R2ADJ` = "R Squared Adjusted",
      `LAMZLL[hr]` = "Lambda z Lower Limit",
      `LAMZNPT[count]` = "Number of Points for Lambda z",
      `CLSTP[ng/mL]` = "Clast pred",
      `LAMZHL[hr]` = "Half-Life Lambda z",
      `LAMZSPN` = "Lambda z Span",
      `AUCIFO[hr*ng/mL]` = "AUC Infinity Obs",
      `AUCINT_0-2[hr*ng/mL]` = "AUC from T1 to T2",
      `AUCINT_2-4[hr*ng/mL]` = "AUC from T1 to T2",
      `LAMZIX` = NA, `LAMZMTD` = NA, `Exclude` = NA
    )
    expect_equal(labels, expected_labels)
  })

  it("handles exclude values correctly", {
    # Modify pknca_res$result to include exclude values
    res_with_exclude <- pknca_res
    res_with_exclude$result <- res_with_exclude$result %>%
      mutate(
        exclude = ifelse(USUBJID == 1 & NCA_PROFILE == 1, "Reason 1; Reason 2", NA_character_)
      )

    # Apply pivot_wider_pknca_results
    result <- pivot_wider_pknca_results(res_with_exclude)

    # Check that the Exclude column combines and deduplicates exclude values
    exclude_values <- result %>% filter(USUBJID == 1 & NCA_PROFILE == 1) %>% pull(Exclude)
    expect_equal(exclude_values, "Reason 1, Reason 2")

    # Check that rows without exclude values have NA in the Exclude column
    exclude_values_na <- result %>% filter(USUBJID == 2 & NCA_PROFILE == 2) %>% pull(Exclude)
    expect_true(all(is.na(exclude_values_na)))
  })
})
