# Use the testing data to produce results
pwres <- pivot_wider_pknca_results(TEST_PKNCA_RES)

#' Validate PKNCA Parameters
#'
#' This function validates that the reshaped PKNCA results contain the expected values
#' for each parameter based on the original `TEST_PKNCA_RES` object.
#'
#' @param TEST_PKNCA_RES The original PKNCA results object.
#' @param result The reshaped results from `pivot_wider_pknca_results`.
#' @return A logical value indicating whether all checks passed.
#' @export
.validate_pknca_params <- function(TEST_PKNCA_RES, result) {

  # Extract unique parameter names with units
  names_params <- TEST_PKNCA_RES$result %>%
    mutate(PPTESTCD2 = case_when(
      type_interval == "manual" ~ paste0(PPTESTCD, "_", start, "-", end),
      PPSTRESU == "" | PPSTRESU == "unitless" ~ PPTESTCD,
      TRUE ~ paste0(PPTESTCD, "[", PPSTRESU, "]")
    )) %>%
    pull(PPTESTCD2) %>%
    unique()

  # Iterate over each parameter and validate values
  for (param_col in names_params) {
    pptestcd <- gsub("\\[.*\\]", "", param_col)  # Remove units from parameter name

    if (grepl("AUCINT_[0-9]+\\-[0-9]", param_col)) {
      pptestcd <- gsub("_[0-9]+\\-[0-9]", "", param_col)
      auc_start <- gsub("AUCINT_([0-9]+)\\-[0-9]", "\\1", param_col)
      auc_end <- gsub("AUCINT_[0-9]+\\-([0-9]+)", "\\1", param_col)

      original_data <- TEST_PKNCA_RES$result %>%
        filter(start == auc_start,
               end == auc_end)
      reshaped_res <- result %>%
        filter(start == auc_start,
               end == auc_end)
    } else {
      original_data <- TEST_PKNCA_RES$result
      reshaped_res <- result
    }

    original_vals <- original_data %>%
      formatters::var_labels_remove() %>%
      filter(PPTESTCD == pptestcd) %>%
      pull(PPSTRES)
    result_vals <- reshaped_res %>%
      formatters::var_labels_remove() %>%
      pull(param_col)

    have_similar_vals <- all(round(result_vals, 3) %in% round(original_vals, 3))
    have_same_length <- length(result_vals) == length(original_vals)
    if (!(have_similar_vals && have_same_length)) {
      stop(paste("Mismatch in values for parameter:", pptestcd))
    }
  }

  # If all checks pass, return TRUE
  TRUE
}


describe("pivot_wider_pknca_results", {

  it("produces a data.frame with expected format when only reshaping main intervals", {
    pwres_only_main <- TEST_PKNCA_RES
    pwres_only_main$result <- TEST_PKNCA_RES$result  %>%
      filter(type_interval == "main")
    result_only_main <- pivot_wider_pknca_results(pwres_only_main)
    expect_s3_class(result, "data.frame")

    # Check that the result contains expected columns
    expected_columns <- c(
      "USUBJID", "start", "end", "DOSNO", "AFRLT", "ARRLT",
      "NFRLT", "NRRLT", "ADOSE", "route", "duration",
      "CMAX[ng/mL]", "TMAX[hr]", "TLST[hr]", "LAMZ[1/hr]",
      "R2", "R2ADJ", "LAMZLL[hr]",
      "LAMZNPT[count]", "CLSTP[ng/mL]", "LAMZHL[hr]",
      "LAMZSPN", "LAMZIX",
      "LAMZMTD", "Exclude"
    )
    expect_true(all(expected_columns %in% colnames(result_only_main)))

    # Check number of rows is the expected
    expect_true(nrow(result_only_main) == nrow(pwres_only_main$data$dose$data))

    # Check values
    expect_true(.validate_pknca_params(pwres_only_main, result_only_main))
  })

  it("reshapes PKNCA results correctly when also considering AUC intervals", {
    # Check that the result is a data frame
    expect_s3_class(pwres, "data.frame")

    # Check that the result contains expected columns
    expected_columns <- c(
      "USUBJID", "start", "end", "DOSNO", "AFRLT", "ARRLT",
      "NFRLT", "NRRLT", "ADOSE", "route", "duration",
      "CMAX[ng/mL]", "TMAX[hr]", "TLST[hr]", "LAMZ[1/hr]",
      "R2", "R2ADJ", "LAMZLL[hr]",
      "LAMZNPT[count]", "CLSTP[ng/mL]", "LAMZHL[hr]",
      "LAMZSPN", "LAMZIX", "LAMZMTD", "Exclude",
      "AUCINT_0-2", "AUCINT_2-4"
    )
    expect_true(all(expected_columns %in% colnames(pwres)))

    expect_true(.validate_pknca_params(TEST_PKNCA_RES, pwres))
  })

  it("rounds numeric values to three decimals", {
    expected_num_param_cols <- c(
      "CMAX[ng/mL]", "TMAX[hr]", "TLST[hr]", "LAMZ[1/hr]",
      "R2", "R2ADJ", "LAMZLL[hr]",
      "LAMZNPT[count]", "CLSTP[ng/mL]", "LAMZHL[hr]",
      "LAMZSPN", "AUCINT_0-5", "AUCINT_5-10"
    )
    rounded_values <- pwres[, expected_num_param_cols]
    expect_true(all(apply(rounded_values, 2,
                          function(x) all(na.omit(abs(x - round(x, 3))) < 1e-9))))
  })

  it("adds appropriate labels to columns", {
    labels <- formatters::var_labels(pwres)
    expected_labels <- c(
      ID = NA, start = NA, end = NA, DOSNO = NA, AFRLT = NA, ARRLT = NA,
      NFRLT = NA, NRRLT = NA, DOSE = NA, route = NA, duration = NA,
      `CMAX[ng/mL]` = "Max Conc", `TMAX[hr]` = "Time of CMAX",
      `TLST[hr]` = "Time of Last Nonzero Conc", `LAMZ[1/hr]` = "Lambda z",
      `R2` = "R Squared", `R2ADJ` = "R Squared Adjusted",
      `LAMZLL[hr]` = "Lambda z Lower Limit", `LAMZNPT[count]` = "Number of Points for Lambda z",
      `CLSTP[ng/mL]` = "Clast pred", `LAMZHL[hr]` = "Half-Life Lambda z",
      `LAMZSPN` = "Lambda z Span", `AUCINT_0-5` = NA, `AUCINT_5-10` = NA,
      `LAMZIX` = NA, `LAMZMTD` = NA, `Exclude` = NA
    )
    expect_equal(labels, expected_labels)
  })

  it("handles exclude values correctly", {
    # Modify TEST_PKNCA_RES$result to include exclude values
    pwres_with_exclude <- TEST_PKNCA_RES
    pwres_with_exclude$result <- pwres_with_exclude$result %>%
      mutate(
        exclude = ifelse(ID == 1 & DOSNO == 1, "Reason 1; Reason 2", NA_character_)
      )

    # Apply pivot_wider_pknca_results
    result <- pivot_wider_pknca_results(pwres_with_exclude)

    # Check that the Exclude column combines and deduplicates exclude values
    exclude_values <- result %>% filter(ID == 1 & DOSNO == 1) %>% pull(Exclude)
    expect_equal(exclude_values, "Reason 1, Reason 2")

    # Check that rows without exclude values have NA in the Exclude column
    exclude_values_na <- result %>% filter(ID == 2 & DOSNO == 2) %>% pull(Exclude)
    expect_true(is.na(exclude_values_na))
  })
})
