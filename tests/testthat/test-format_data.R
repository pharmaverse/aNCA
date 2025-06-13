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

describe("format_pkncaconc_data", {
  it("generates a valid dataset with required columns", {
    df_conc <- format_pkncaconc_data(ADNCA,
                                     group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                       "DRUG", "PARAM"),
                                     time_column = "AFRLT",
                                     rrlt_column = "ARRLT")

    expect_s3_class(df_conc, "data.frame")
    expect_setequal(
      c(names(ADNCA), "std_route", "conc_groups", "TIME", "TIME_DOSE", "DOSNOA"),
      colnames(df_conc)
    )
    expect_equal(df_conc$TIME, df_conc$AFRLT)
    expect_no_error(
      PKNCA::PKNCAconc(
        df_conc,
        formula = AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / PARAM,
        exclude_half.life = "exclude_half.life",
        time.nominal = "NFRLT"
      )
    )
  })

  it("filters EVID if column is present", {
    # Create base data with EVID = 0
    ADNCA <- ADNCA %>%
      mutate(EVID = 0)

    # Create one dosing row per USUBJID
    dosing_rows <- ADNCA %>%
      distinct(STUDYID) %>%
      mutate(
        USUBJID = 1,
        PCSPEC = "Plasma",
        DRUG = "DrugA",
        PARAM = "Analyte1",
        AFRLT = 0,
        ARRLT = 0,
        NFRLT = 0,
        NCA_PROFILE = 1,
        DOSEA = 5,
        ROUTE = "intravascular",
        ADOSEDUR = 0,
        AVAL = 10,
        EVID = 1
      )

    # Append the dosing rows to the original data
    ADNCA <- bind_rows(ADNCA, dosing_rows)
    df_conc <- format_pkncaconc_data(ADNCA,
                                     group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                       "DRUG", "PARAM"),
                                     time_column = "AFRLT",
                                     rrlt_column = "ARRLT")

    expect_true(all(df_conc$EVID == 0))
  })

  it("filters out rows where PARAMCD contains DOSE", {
    data_with_paramcd <- ADNCA %>%
      mutate(PARAMCD = c(rep("DOSE1", 10), rep("PARAM", 10)))

    result <- format_pkncaconc_data(
      data_with_paramcd,
      group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "PARAM"),
      time_column = "AFRLT"
    )
    expect_true(all(!grepl("DOSE", result$PARAMCD, ignore.case = TRUE)))
  })

  it("returns an error for empty input dataframe", {
    empty_adnca <- data.frame()
    expect_error(format_pkncaconc_data(empty_adnca,
                                       group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                         "DRUG", "PARAM"),
                                       time_column = "AFRLT"),
                 regexp = "Input dataframe is empty. Please provide a valid ADNCA dataframe.")
  })

  it("returns an error for missing required columns", {
    incomplete_adnca <- ADNCA %>% select(-PARAM)
    expect_error(
      format_pkncaconc_data(
        incomplete_adnca,
        group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "PARAM"),
        time_column = "AFRLT"
      ),
      regexp = "Missing required columns: PARAM"
    )
  })

  it("processes multiple analytes correctly", {
    multi_analyte_adnca <- ADNCA %>% mutate(PARAM = rep(c("Analyte1", "Analyte2"), each = 10))
    df_conc <- format_pkncaconc_data(multi_analyte_adnca,
                                     group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                       "DRUG", "PARAM"),
                                     time_column = "AFRLT")
    expect_equal(nrow(df_conc), 20)
    expect_equal(length(unique(df_conc$PARAM)), 2)
  })
})

describe("format_pkncadose_data", {
  df_conc <- format_pkncaconc_data(ADNCA,
                                   group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                     "DRUG", "PARAM"))

  it("generates with no errors", {
    df_dose <- format_pkncadose_data(df_conc,
                                     group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                       "DRUG", "PARAM"))

    expect_s3_class(df_dose, "data.frame")
    expect_setequal(
      c(names(ADNCA), "conc_groups", "TIME", "TIME_DOSE", "std_route", "DOSNOA"),
      colnames(df_dose)
    )

    # Test if myintervals can be used with PKNCAdata by testing its output
    expect_no_error(
      PKNCA::PKNCAdose(
        data = df_dose,
        formula = DOSEA ~ TIME_DOSE | STUDYID + DRUG + USUBJID,
        route = "ROUTE",
        time.nominal = "NFRLT",
        duration = "ADOSEDUR"
      )
    )
    dose_counts <- df_dose %>% group_by(USUBJID) %>% summarise(n = n())
    expect_true(all(dose_counts$n >= 2))
  })

  it("handles empty input", {
    empty_df_conc <- data.frame()
    expect_error(
      format_pkncadose_data(
        empty_df_conc,
        group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG")
      ),
      regexp = "Input dataframe is empty. Please provide a valid concentration dataframe."
    )
  })

  it("handles missing columns", {
    missing_col_df_conc <- df_conc %>% ungroup() %>% select(-DRUG)
    expect_error(
      format_pkncadose_data(
        missing_col_df_conc,
        group_columns = c("STUDYID", "USUBJID", "DRUG")
      ),
      regexp = "Missing required columns: DRUG"
    )
  })

  it("handles negative time values", {
    negative_time_adnca <- ADNCA %>%
      add_row(
        ADNCA[1, ] %>%
          mutate(
            AFRLT = -1,
            ARRLT = -1
          )
      )
    df_conc <- format_pkncaconc_data(negative_time_adnca,
                                     group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                       "DRUG", "PARAM"),
                                     time_column = "AFRLT")
    df_dose <- format_pkncadose_data(df_conc,
                                     group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                       "DRUG", "PARAM"))
    expect_true(all(df_dose$TIME_DOSE >= 0))
  })
})

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
    formula = AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / PARAM,
    exclude_half.life = "exclude_half.life",
    time.nominal = "NFRLT"
  )

  pknca_dose <- PKNCA::PKNCAdose(
    data = df_dose,
    formula = DOSEA ~ TIME_DOSE | STUDYID + DRUG + USUBJID
  )

  params <- c("cmax", "tmax", "half.life", "cl.obs")
  it("handles multiple analytes with metabolites", {
    result <- format_pkncadata_intervals(pknca_conc, pknca_dose, params = params)

    expect_equal(result$start[1], 0)
    expect_equal(result$end[1], 5)
    expect_equal(unique(result$PARAM), c("Analyte1", "Metabolite1"))
    expect_equal(result$cmax[1], TRUE)
    expect_equal(result$tmax[1], TRUE)
    expect_equal(result$half.life[1], TRUE)
    expect_equal(result$cl.obs[1], TRUE)

    expect_no_error(
      PKNCA::PKNCAdata(
        data.conc = pknca_conc,
        data.dose = pknca_dose,
        intervals = result,
        options = list(
          keep_interval_cols = c("TIME_DOSE", "NCA_PROFILE", "DOSNOA", "type_interval")
        ),
        units = PKNCA::pknca_units_table(
          concu = "ng/mL",
          doseu = "mg",
          amountu = "ng",
          timeu = "h"
        )
      )
    )
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
      formula = AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / PARAM,
      exclude_half.life = "exclude_half.life",
      time.nominal = "NFRLT"
    )

    result_tau <- format_pkncadata_intervals(pknca_conc_tau, pknca_dose, params = params)

    expect_equal(result_tau$end[4], 10)
  })

  it("sets last time to end AFRLT if no TAU available", {
    result <- format_pkncadata_intervals(pknca_conc, pknca_dose, params = params)
    expect_equal(result$end[4], 9)
  })

  it("uses ARRLT for start when start_from_last_dose is FALSE", {
    result <- format_pkncadata_intervals(
      pknca_conc = pknca_conc,
      pknca_dose = pknca_dose,
      params = c("cmax"),
      start_from_last_dose = FALSE
    )
    expect_true(all(result$start >= 0))
  })
  
  it("sets correct parameters if VOLUME present", {
    df_conc_volume <- df_conc %>%
      mutate(VOLUME = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

    pknca_conc_volume <- PKNCA::PKNCAconc(
      df_conc_volume,
      formula = AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / PARAM,
      exclude_half.life = "exclude_half.life",
      time.nominal = "NFRLT",
      volume = "VOLUME"
    )

    result_volume <- format_pkncadata_intervals(pknca_conc_volume, pknca_dose, params = c("ae", "fe", "cmax", "tmax", "half.life", "cl.obs"))

    expect_false(all(result_volume$cmax))
    expect_false(all(result_volume$tmax))
    expect_false(all(result_volume$half.life))
    expect_false(all(result_volume$cl.obs))
    expect_true(all(result_volume$ae))
    expect_true(all(result_volume$fe))
  })
  
  it("sets correct parameters if VOLUME not present",
     {
    pknca_conc_no_volume <- PKNCA::PKNCAconc(
      df_conc,
      formula = AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / PARAM,
      exclude_half.life = "exclude_half.life",
      time.nominal = "NFRLT"
    )

    result_no_volume <- format_pkncadata_intervals(pknca_conc_no_volume, pknca_dose, params =c("ae", "fe", "cmax", "tmax", "half.life"))

    expect_false(all(result_no_volume$ae))
    expect_false(all(result_no_volume$fe))
    expect_true(all(result_no_volume$cmax))
    expect_true(all(result_no_volume$tmax))
    expect_true(all(result_no_volume$half.life))
  })

})
