# Shared data setup for all tests
ADNCA <- data.frame(
  STUDYID = rep(1, 20),
  USUBJID = rep(1:2, each = 10),
  PCSPEC = rep("Plasma", 20),
  DRUG = rep("DrugA", 20),
  PARAM = rep("Analyte1", 20),
  AFRLT = rep(seq(0, 9), 2),
  ARRLT = rep(seq(0, 4), 4),
  NFRLT = rep(seq(0, 9), 2),
  NCA_PROFILE = rep(1, 20),
  DOSEA = rep(c(5, 10), each = 10),
  ROUTE = rep(c("intravascular", "extravascular"), each = 10),
  ADOSEDUR = rep(c(0, 0), each = 10),
  AVAL = runif(20)
)

describe("format_pkncadata_intervals", {
  multi_analyte_adnca <- ADNCA %>% mutate(PARAM = rep(c("Analyte1", "Metabolite1"), each = 10))
  df_conc <- format_pkncaconc_data(multi_analyte_adnca,
                                   group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                     "DRUG", "PARAM"),
                                   time_column = "AFRLT")

  df_dose <- format_pkncadose_data(df_conc,
                                   group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                     "DRUG"))

  pknca_conc <- PKNCA::PKNCAconc(
    df_conc,
    formula = AVAL ~ AFRLT | STUDYID + PCSPEC + DRUG + USUBJID / PARAM,
    exclude_half.life = "exclude_half.life",
    time.nominal = "NFRLT"
  )

  pknca_dose <- PKNCA::PKNCAdose(
    data = df_dose,
    formula = DOSEA ~ AFRLT | STUDYID + DRUG + USUBJID
  )

  it("handles multiple analytes with metabolites", {
    result <- format_pkncadata_intervals(pknca_conc, pknca_dose)

    expect_equal(result$start[1], 0)
    expect_equal(result$end[1], 5)
    expect_equal(unique(result$PARAM), c("Analyte1", "Metabolite1"))
    expect_equal(result$cmax[1], FALSE)
    expect_equal(result$tmax[1], FALSE)
    expect_equal(result$half.life[1], FALSE)
    expect_equal(result$cl.obs[1], FALSE)

  })

  it("handles incorrect input type", {
    expect_error(format_pkncadata_intervals(pknca_conc = df_conc, data.frame()),
                 regexp = "Input pknca_conc must be a PKNCAconc object from the PKNCA package.")

    expect_error(format_pkncadata_intervals(pknca_conc = pknca_conc, data.frame()),
                 regexp = "Input pknca_dose must be a PKNCAdose object from the PKNCA package.")
  })

  it("handles missing columns", {
    missing_col_pknca_dose <- pknca_dose
    missing_col_pknca_dose$data <- select(missing_col_pknca_dose$data, -DRUG)
    expect_error(
      format_pkncadata_intervals(
        pknca_conc,
        missing_col_pknca_dose
      ),
      regexp = "Missing required columns: DRUG"
    )
  })

  it("correctly uses tau if column is available", {
    df_conc_tau <- df_conc %>%
      mutate(TAU = 5)  # Add a tau column for testing

    pknca_conc_tau <- PKNCA::PKNCAconc(
      df_conc_tau,
      formula = AVAL ~ AFRLT | STUDYID + PCSPEC + DRUG + USUBJID / PARAM,
      exclude_half.life = "exclude_half.life",
      time.nominal = "NFRLT"
    )

    result_tau <- format_pkncadata_intervals(pknca_conc_tau, pknca_dose)

    expect_equal(result_tau$end[4], 10)
  })

  it("sets last time to end AFRLT if no TAU available", {
    result <- format_pkncadata_intervals(pknca_conc, pknca_dose)
    expect_equal(result$end[4], 9)
  })

  it("sets end to Inf if no TAU and single dose", {
    single_dose_pknca_conc <- pknca_conc
    single_dose_pknca_conc$data <- single_dose_pknca_conc$data %>%
      filter(DOSNOA == 1)  # Filter to a single dose

    single_dose_pknca_dose <- pknca_dose
    single_dose_pknca_dose$data <- single_dose_pknca_dose$data %>%
      filter(DOSNOA == 1)  # Filter to a single dose

    result_single_dose <- format_pkncadata_intervals(single_dose_pknca_conc,
                                                     single_dose_pknca_dose)
    expect_true(all(is.infinite(result_single_dose$end)))
  })

  it("sets end to Inf if TAU= NA and single dose", {
    single_dose_pknca_conc <- pknca_conc
    single_dose_pknca_conc$data <- single_dose_pknca_conc$data %>%
      mutate(TAU = NA) %>%  # Set TAU to NA
      filter(DOSNOA == 1)  # Filter to a single dose

    single_dose_pknca_dose <- pknca_dose
    single_dose_pknca_dose$data <- single_dose_pknca_dose$data %>%
      mutate(TAU = NA) %>%  # Set TAU to NA
      filter(DOSNOA == 1)  # Filter to a single dose

    result_single_dose <- format_pkncadata_intervals(single_dose_pknca_conc,
                                                     single_dose_pknca_dose)
    expect_true(all(is.infinite(result_single_dose$end)))
  })

  it("sets end to last time point if TAU= NA and multiple dose", {
    pknca_conc_na_tau <- pknca_conc
    pknca_conc_na_tau$data <- pknca_conc$data %>%
      mutate(TAU = NA) # Set TAU to NA

    pknca_dose_na_tau <- pknca_dose
    pknca_dose_na_tau$data <- pknca_dose$data %>%
      mutate(TAU = NA)  # Set TAU to NA

    result_single_dose <- format_pkncadata_intervals(pknca_conc_na_tau,
                                                     pknca_dose_na_tau)
    expect_equal(result_single_dose$end[4], max(pknca_conc_na_tau$data$AFRLT, na.rm = TRUE))
  })

  it("uses ARRLT for start when start_from_last_dose is FALSE", {
    result <- format_pkncadata_intervals(
      pknca_conc = pknca_conc,
      pknca_dose = pknca_dose,
      start_from_last_dose = FALSE
    )
    expect_true(all(result$start >= 0))
  })

})

describe("update_main_intervals", {

  all_pknca_params <- setdiff(names(PKNCA::get.interval.cols()),
                              c("start", "end"))
  # setup data using FIXTURES
  data <- FIXTURE_PKNCA_DATA
  data$intervals <- data$intervals %>%
    mutate(!!!setNames(rep(FALSE, length(all_pknca_params)), all_pknca_params),
           PCSPEC = "SERUM",
           STUDYID = "S1") %>%
    filter(type_interval == "main") %>%
    select(-impute)

  #create study types df
  study_types_df <- tribble(
    ~STUDYID, ~DRUG, ~USUBJID, ~PCSPEC, ~ROUTE,         ~type,
    "S1",      "A",           1, "SERUM",  "extravascular", "Single Extravascular Dose",
    "S1",      "A",           2, "SERUM",  "extravascular", "Multiple Extravascular Doses",
    "S1",      "A",           3, "SERUM",  "intravascular", "Multiple IV Doses",
    "S1",      "A",           4, "SERUM",  "intravascular", "Single IV Dose",
    "S1",      "A",           5, "SERUM",  "intravascular", "Single IV Dose",
    "S1",      "A",           6, "SERUM",  "intravascular", "Single IV Dose",
    "S1",      "A",           7, "SERUM",  "intravascular", "Single IV Dose",
    "S1",      "A",           8, "SERUM",  "extravascular", "Single Extravascular Dose",
    "S1",      "A",           8, "SERUM",  "intravascular", "Multiple IV Doses"
  )

  #Create parameter list for each study type (one different per type)
  parameters <- list(
    `Single Extravascular Dose`    = c("cmax", "tmax", "auclast"),
    `Multiple Extravascular Doses` = c("cmax", "tmax", "half.life"),
    `Multiple IV Doses`            = c("cmax", "auclast", "half.life"),
    `Single IV Dose`               = c("tmax", "auclast", "half.life")
  )

  auc_data <- tibble(start_auc = rep(NA_real_, 2), end_auc = rep(NA_real_, 2))

  it("correctly updates parameter flags based on study type", {
    result <- update_main_intervals(data, parameters, study_types_df,
                                    auc_data, impute = FALSE)

    # Check a specific profile: USUBJID 1 is 'Single Extravascular Dose'
    profile_1 <- result$intervals %>% filter(USUBJID == 1)
    expected_true <- parameters$`Single Extravascular Dose`
    expected_false <- setdiff(all_pknca_params, expected_true)

    expect_true(all(profile_1[expected_true] == TRUE))
    expect_true(all(profile_1[expected_false] == FALSE))

    # Check another profile: USUBJID 3 is 'Multiple IV Doses'
    profile_3 <- result$intervals %>% filter(USUBJID == 3)
    expected_true_3 <- parameters$`Multiple IV Doses`
    expected_false_3 <- setdiff(all_pknca_params, expected_true_3)

    expect_true(all(profile_3[expected_true_3] == TRUE))
    expect_true(all(profile_3[expected_false_3] == FALSE))
  })

  it("handles partial AUCs (auc_data) creating proper intervals for each", {

    auc_data <- data.frame(
      start_auc = c(0, 1, 2),
      end_auc = c(1, 2, 3)
    )
    result <- update_main_intervals(data, parameters, study_types_df, auc_data, impute = FALSE)

    manual_intervals <- result$intervals %>% filter(type_interval == "manual")
    expect_true(all(manual_intervals$aucint.last == TRUE))

    new_interval_check <- manual_intervals %>%
      filter(USUBJID == 1, start == 1)

    expect_equal(nrow(new_interval_check), 1)
    expect_equal(new_interval_check$end, 2)
  })

  it("does not impute C0 when not requested", {
    result <- update_main_intervals(data, parameters, study_types_df, auc_data, impute = FALSE)
    expect_true("impute" %in% names(result))
    expect_true(all(is.na(result$intervals$impute)))
  })

  it("imputes c0 when requested", {
    result <- update_main_intervals(data, parameters, study_types_df, auc_data, impute = TRUE)
    expect_true("impute" %in% names(result))
    expect_false(all(is.na(result$intervals$impute)))
  })

  it("handles empty parameter selections and empty AUC data", {
    # Test with empty parameter list
    result_no_params <- update_main_intervals(data, list(), study_types_df,
                                              auc_data, impute = FALSE)
    param_flags <- result_no_params$intervals %>% select(all_of(all_pknca_params))
    expect_true(all(param_flags == FALSE))

    # Test with empty auc_data
    result_no_auc <- update_main_intervals(data, parameters, study_types_df,
                                           auc_data, impute = FALSE)
    expect_equal(nrow(result_no_auc$intervals), nrow(data$intervals))
  })

  it("filters out invalid AUC ranges before creating intervals", {
    invalid_auc_data <- data.frame(
      start_auc = c(0,  5, -1, 2, NA), # valid, start > end, negative, start > end, NA
      end_auc   = c(4,  2,  1, 2, 4)
    ) # Only first row is valid

    original_rows <- nrow(data$intervals)
    result <- update_main_intervals(data, parameters, study_types_df,
                                    invalid_auc_data, impute = FALSE)

    # Expect one new set of intervals for the single valid AUC range
    expect_equal(nrow(result$intervals), original_rows * 2)
  })

  it("handles missing columns correctly", {
    # remove PCSPEC column from intervals
    data$intervals$PCSPEC <- NULL

    expect_error(update_main_intervals(data, parameters, study_types_df,
                                       auc_data, impute = FALSE),
                 "Missing required columns: PCSPEC")
  })

})
