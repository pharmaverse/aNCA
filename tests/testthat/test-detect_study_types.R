describe("detect_study_types", {

  # Create a base data frame.
  base_data <- data.frame(
    STUDYID = "STUDY01",
    DRUG = "DRUG01",
    USUBJID = "SUBJ01",
    PCSPEC = "PLASMA",
    DOSNOA = 1,
    ROUTE = "IV",
    VOL = 0,
    TAU = NA_real_
  )

  # --- Test each study type classification ---

  it("correctly identifies a 'Single IV Dose' study", {
    test_data <- base_data
    result <- detect_study_types(test_data, "ROUTE", "VOL")

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$Type, "Single IV Dose")
    expect_equal(result$USUBJID_Count, 1)
  })

  it("correctly identifies a 'Single Extravascular Dose' study", {
    test_data <- base_data %>%
      mutate(ROUTE = "extravascular")
    result <- detect_study_types(test_data, "ROUTE", "VOL")

    expect_equal(nrow(result), 1)
    expect_equal(result$Type, "Single Extravascular Dose")
  })

  it("correctly identifies a 'Multiple IV Doses' study via multiple DOSNOA", {
    test_data <- bind_rows(
      base_data,
      base_data %>% mutate(DOSNOA = 2)
    )
    result <- detect_study_types(test_data, "ROUTE", "VOL")

    expect_equal(nrow(result), 1)
    expect_equal(result$Type, "Multiple IV Doses")
  })

  it("correctly identifies a 'Multiple Extravascular Doses' study via a non-NA TAU", {
    test_data <- base_data %>%
      mutate(ROUTE = "extravascular", TAU = 24)
    result <- detect_study_types(test_data, "ROUTE", "VOL")

    expect_equal(nrow(result), 1)
    expect_equal(result$Type, "Multiple Extravascular Doses")
  })

  it("correctly identifies 'Excretion Data', which takes precedence over other types", {
    # This data would otherwise be classified as a "Single IV Dose"
    test_data <- base_data %>%
      mutate(VOL = 15)
    result <- detect_study_types(test_data, "ROUTE", "VOL")

    expect_equal(nrow(result), 1)
    expect_equal(result$Type, "Excretion Data")
  })

  it("handles data frames without a TAU column correctly", {
    # Test for single dose without TAU column
    test_data_single <- base_data %>% select(-TAU)
    result_single <- detect_study_types(test_data_single, "ROUTE", "VOL")
    expect_equal(result_single$Type, "Single IV Dose")

    # Test for multiple doses without TAU column
    test_data_multi <- bind_rows(
      base_data,
      base_data %>% mutate(DOSNOA = 2)
    ) %>%
      select(-TAU)
    result_multi <- detect_study_types(test_data_multi, "ROUTE", "VOL")
    expect_equal(result_multi$Type, "Multiple IV Doses")
  })

  it("handles data frames without a volume column correctly", {
    # Test for single dose without volume column
    test_data_single <- base_data %>% select(-VOL)
    result_single <- detect_study_types(test_data_single, "ROUTE")
    expect_equal(result_single$Type, "Single IV Dose")

  })

  it("correctly summarizes a complex dataset with multiple study types and subjects", {
    combined_data <- bind_rows(
      # USUBJID: Subj-1, Type: Single IV
      base_data %>% mutate(USUBJID = "SUBJ01"),
      base_data %>% mutate(USUBJID = "SUBJ01", VOL = 15),
      # USUBJID: Subj-2, Type: Single Extravascular
      base_data %>% mutate(USUBJID = "SUBJ02", ROUTE = "extravascular"),
      # USUBJID: Subj-3, Type: Multiple IV (via DOSNOA)
      base_data %>% mutate(USUBJID = "SUBJ03"),
      base_data %>% mutate(USUBJID = "SUBJ03", DOSNOA = 2),
      # USUBJID: Subj-4, Type: Multiple Extravascular (via TAU)
      base_data %>% mutate(USUBJID = "SUBJ04", ROUTE = "extravascular", TAU = 12)
    )

    result <- detect_study_types(combined_data, "ROUTE", "VOL")

    # Expect 5 rows in the summary, one for each unique type detected
    expect_equal(nrow(result), 5)

    # Check that all expected types are present in the 'Type' column
    expected_types <- c(
      "Single IV Dose",
      "Single Extravascular Dose",
      "Multiple IV Doses",
      "Multiple Extravascular Doses",
      "Excretion Data"
    )
    expect_setequal(result$Type, expected_types)

    # Verify each type has a count of 1 subject
    expect_true(all(result$USUBJID_Count == 1))
  })
})
