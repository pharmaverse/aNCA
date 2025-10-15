describe("detect_study_types", {

  # Create a base data frame.
  base_data <- data.frame(
    STUDYID = "STUDY01",
    TRT = "DRUG01",
    ANALYTE = "DRUG01",
    METABFL = "N",
    USUBJID = "SUBJ01",
    PCSPEC = "PLASMA",
    DOSNOA = 1,
    ROUTE = "IV",
    VOL = 0,
    TRTRINT = NA_real_,
    ADOSEDUR = 0
  )

  groups <- c("TRT", "ANALYTE", "USUBJID", "PCSPEC")
  # --- Test each study type classification ---

  it("correctly identifies a 'Single IV Bolus Dose' study", {
    test_data <- base_data
    result <- detect_study_types(test_data, groups, "METABFL", "ROUTE", "VOL")

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$type, "Single IV Bolus")
  })

  it("correctly identifies a 'Single Extravascular Dose' study", {
    test_data <- base_data %>%
      mutate(ROUTE = "extravascular")
    result <- detect_study_types(test_data, groups, "METABFL", "ROUTE", "VOL")

    expect_equal(nrow(result), 1)
    expect_equal(result$type, "Single Extravascular")
  })

  it("correctly identifies a `Single IV Infusion Dose` study", {
    test_data <- base_data %>%
      mutate(ADOSEDUR = 2)

    result <- detect_study_types(test_data, groups, "METABFL", "ROUTE", "VOL")

    expect_equal(nrow(result), 1)
    expect_equal(result$type, "Single IV Infusion")
  })

  it("correctly identifies a 'Multiple IV Bolus Dose' study via multiple DOSNOA", {
    test_data <- bind_rows(
      base_data,
      base_data %>% mutate(DOSNOA = 2)
    )
    result <- detect_study_types(test_data, groups, "METABFL", "ROUTE", "VOL")

    expect_equal(nrow(result), 1)
    expect_equal(result$type, "Multiple IV Bolus")
  })

  it("correctly identifies a 'Multiple Extravascular Dose' study via a non-NA TRTRINT", {
    test_data <- base_data %>%
      mutate(ROUTE = "extravascular", TRTRINT = 24)
    result <- detect_study_types(test_data, groups, "METABFL", "ROUTE", "VOL")

    expect_equal(nrow(result), 1)
    expect_equal(result$type, "Multiple Extravascular")
  })

  it("correctly identifies a 'Multiple IV Infusion Doses' study via multiple DOSNOA", {
    test_data <- bind_rows(
      base_data %>% mutate(ADOSEDUR = 2),
      base_data %>% mutate(DOSNOA = 2, ADOSEDUR = 2)
    )
    result <- detect_study_types(test_data, groups, "METABFL", "ROUTE", "VOL")

    expect_equal(nrow(result), 1)
    expect_equal(result$type, "Multiple IV Infusion")
  })

  # --- Test Metabolite Types ---

  it("correctly identifies a 'Single IV Bolus Dose Metabolite' study", {
    test_data <- base_data %>% mutate(METABFL = "Y")
    result <- detect_study_types(test_data, groups, "METABFL", "ROUTE", "VOL")
    expect_equal(result$type, "Single IV Bolus (Metabolite)")
  })

  it("correctly identifies a 'Single Extravascular Dose Metabolite' study", {
    test_data <- base_data %>% mutate(ROUTE = "extravascular", METABFL = "Y")
    result <- detect_study_types(test_data, groups, "METABFL", "ROUTE", "VOL")
    expect_equal(result$type, "Single Extravascular (Metabolite)")
  })

  it("correctly identifies a 'Single IV Infusion Dose Metabolite' study", {
    test_data <- base_data %>% mutate(ADOSEDUR = 2, METABFL = "Y")
    result <- detect_study_types(test_data, groups, "METABFL", "ROUTE", "VOL")
    expect_equal(result$type, "Single IV Infusion (Metabolite)")
  })

  it("correctly identifies a 'Multiple IV Bolus Doses Metabolite' study", {
    test_data <- bind_rows(
      base_data %>% mutate(METABFL = "Y"),
      base_data %>% mutate(DOSNOA = 2, METABFL = "Y")
    )
    result <- detect_study_types(test_data, groups, "METABFL", "ROUTE", "VOL")
    expect_equal(result$type, "Multiple IV Bolus (Metabolite)")
  })

  it("correctly identifies 'Excretion Data', which takes precedence over other types", {
    # This data would otherwise be classified as a "Single IV Dose"
    test_data <- base_data %>%
      mutate(VOL = 15)
    result <- detect_study_types(test_data, groups, "METABFL", "ROUTE", "VOL")

    expect_equal(nrow(result), 1)
    expect_equal(result$type, "Excretion Data")
  })

  it("handles data frames without a TRTRINT column correctly", {
    # Test for single dose without TRTRINT column
    test_data_single <- base_data %>% select(-TRTRINT)
    result_single <- detect_study_types(test_data_single, groups, "METABFL", "ROUTE", "VOL")
    expect_equal(result_single$type, "Single IV Bolus")

    # Test for multiple doses without TRTRINT column
    test_data_multi <- bind_rows(
      base_data,
      base_data %>% mutate(DOSNOA = 2)
    ) %>%
      select(-TRTRINT)
    result_multi <- detect_study_types(test_data_multi, groups, "METABFL", "ROUTE", "VOL")
    expect_equal(result_multi$type, "Multiple IV Bolus")
  })

  it("handles data frames without a volume column correctly", {
    # Test for single dose without volume column
    test_data_single <- base_data %>% select(-VOL)
    result_single <- detect_study_types(test_data_single, groups, "METABFL", "ROUTE")
    expect_equal(result_single$type, "Single IV Bolus")

  })

  it("correctly summarizes a complex dataset with multiple study types and subjects", {
    combined_data <- bind_rows(
      # USUBJID: Subj-1, Type: Single IV
      base_data %>% mutate(USUBJID = "SUBJ01"),
      base_data %>% mutate(USUBJID = "SUBJ01", VOL = 15),
      # USUBJID: Subj-2, Type: Single Extravascular
      base_data %>% mutate(USUBJID = "SUBJ02", ROUTE = "extravascular"),
      # USUBJID: Subj-3, Type: Multiple IV (via DOSNOA)
      base_data %>% mutate(USUBJID = "SUBJ03", ADOSEDUR = 2),
      base_data %>% mutate(USUBJID = "SUBJ03", DOSNOA = 2, ADOSEDUR = 2),
      # USUBJID: Subj-4, Type: Multiple Extravascular (via TRTRINT)
      base_data %>% mutate(USUBJID = "SUBJ04", ROUTE = "extravascular", TRTRINT = 12)
    )

    result <- detect_study_types(combined_data, groups, "METABFL", "ROUTE", "VOL")

    # Expect 5 rows in the summary, one for each unique type detected
    expect_equal(nrow(result), 5)

    # Check that all expected types are present in the 'type' column
    expected_types <- c(
      "Single IV Bolus",
      "Single Extravascular",
      "Multiple IV Infusion",
      "Multiple Extravascular",
      "Excretion Data"
    )
    expect_setequal(result$type, expected_types)
  })
})
