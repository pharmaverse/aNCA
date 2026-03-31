# Shared data setup for all tests
ADNCA <- data.frame(
  STUDYID = rep(1, 20),
  USUBJID = rep(1:2, each = 10),
  PCSPEC = rep("Plasma", 20),
  DOSETRT = rep("DrugA", 20),
  PARAM = rep("Analyte1", 20),
  AFRLT = rep(seq(0, 9), 2),
  ARRLT = rep(seq(0, 4), 4),
  NFRLT = rep(seq(0, 9), 2),
  ATPTREF = rep(1, 20),
  DOSEA = rep(c(5, 10), each = 10),
  ROUTE = rep(c("intravascular", "extravascular"), each = 10),
  ADOSEDUR = rep(c(0, 0), each = 10),
  AVAL = runif(20)
)

describe("format_pkncaconc_data", {
  it("generates a valid dataset with required columns", {
    df_conc <- format_pkncaconc_data(ADNCA,
                                     group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                       "DOSETRT", "PARAM"),
                                     time_column = "AFRLT",
                                     rrlt_column = "ARRLT",
                                     dose_group_columns = c("STUDYID", "USUBJID", "DOSETRT"))

    expect_s3_class(df_conc, "data.frame")
    expect_setequal(
      c(names(ADNCA), "std_route", "DOSNOA", "nca_exclude"),
      colnames(df_conc)
    )
    expect_no_error(
      PKNCA::PKNCAconc(
        df_conc,
        formula = AVAL ~ AFRLT | STUDYID + PCSPEC + DOSETRT + USUBJID / PARAM,
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
        DOSETRT = "DrugA",
        PARAM = "Analyte1",
        AFRLT = 0,
        ARRLT = 0,
        NFRLT = 0,
        ATPTREF = 1,
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
                                                       "DOSETRT", "PARAM"),
                                     time_column = "AFRLT",
                                     rrlt_column = "ARRLT",
                                     dose_group_columns = c("STUDYID", "USUBJID", "DOSETRT"))

    expect_true(all(df_conc$EVID == 0))
  })

  it("filters out rows where PARAMCD contains DOSE", {
    data_with_paramcd <- ADNCA %>%
      mutate(PARAMCD = c(rep("DOSE1", 10), rep("PARAM", 10)))

    result <- format_pkncaconc_data(
      data_with_paramcd,
      group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DOSETRT", "PARAM"),
      time_column = "AFRLT",
      dose_group_columns = c("STUDYID", "USUBJID", "DOSETRT")
    )
    expect_true(all(!grepl("DOSE", result$PARAMCD, ignore.case = TRUE)))
  })

  it("returns an error for empty input dataframe", {
    empty_adnca <- data.frame()
    expect_error(format_pkncaconc_data(empty_adnca,
                                       group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                         "DOSETRT", "PARAM"),
                                       time_column = "AFRLT",
                                       dose_group_columns = c("STUDYID", "USUBJID", "DOSETRT")),
                 regexp = "Input dataframe is empty. Please provide a valid ADNCA dataframe.")
  })

  it("returns an error for missing required columns", {
    incomplete_adnca <- ADNCA %>% select(-PARAM)
    expect_error(
      format_pkncaconc_data(
        incomplete_adnca,
        group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DOSETRT", "PARAM"),
        time_column = "AFRLT",
        dose_group_columns = c("STUDYID", "USUBJID", "DOSETRT")
      ),
      regexp = "Missing required columns: PARAM"
    )
  })

  it("handles time_end_column (AEFRLT) correctly, if present", {
    subj1 <- unique(ADNCA$USUBJID)[1]
    adnca <- ADNCA %>%
      # For some cases (subj1) add a collection duration time, for the rest no
      mutate(
        AEFRLT = ifelse(
          USUBJID %in% subj1,
          AFRLT + 0.5,
          AFRLT
        )
      )
    df_conc <- format_pkncaconc_data(adnca,
                                     group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                       "DOSETRT", "PARAM"),
                                     time_end_column = "AEFRLT",
                                     time_column = "AFRLT",
                                     rrlt_column = "ARRLT",
                                     dose_group_columns = c("STUDYID", "USUBJID", "DOSETRT"))

    # CONCDUR is created correctly based on the time end column
    expect_equal(df_conc$CONCDUR, ifelse(df_conc$USUBJID %in% subj1, 0.5, 0))

    # CONCDUR cna be used as a sample collection duration column with PKNCA
    expect_no_error(
      PKNCA::PKNCAconc(
        df_conc,
        formula = AVAL ~ AFRLT | STUDYID + PCSPEC + DOSETRT + USUBJID / PARAM,
        exclude_half.life = "exclude_half.life",
        time.nominal = "NFRLT",
        duration = "CONCDUR"
      )
    )
  })

  test_that("using nca_exclude_reason_columns concatenates exclusion reasons correctly", {
    test_df <- ADNCA
    nrows <- nrow(ADNCA)
    test_df$NCA1XRS <- c(rep("Contaminated sample", 2), rep(NA_character_, nrows - 2))
    test_df$NCA2XRS <- c("Wrongly labelled", rep(NA_character_, nrows - 2), "Wrong labelled")

    exp_nca_exclude <- c(
      "Contaminated sample; Wrongly labelled",
      "Contaminated sample",
      rep("", nrows - 3),
      "Wrong labelled"
    )

    res <- format_pkncaconc_data(
      test_df,
      group_columns = "USUBJID",
      nca_exclude_reason_columns = c("NCA1XRS", "NCA2XRS"),
      dose_group_columns = "USUBJID"
    )

    expect_equal(res$nca_exclude, exp_nca_exclude)
  })

  it("computes DOSNOA consistently across specimen types", {
    # Plasma has Day 1 (dose 1) and Day 10 (dose 2), Urine only has Day 10
    plasma_day1 <- data.frame(
      STUDYID = 1, USUBJID = c(1, 1), PCSPEC = "PLASMA",
      DOSETRT = "DrugA", PARAM = "Analyte1",
      AFRLT = c(0, 1), ARRLT = c(0, 1), NFRLT = c(0, 1),
      ATPTREF = "Day 1", DOSEA = 100,
      ROUTE = "extravascular", ADOSEDUR = 0, AVAL = c(0.5, 1.2)
    )
    plasma_day10 <- data.frame(
      STUDYID = 1, USUBJID = c(1, 1), PCSPEC = "PLASMA",
      DOSETRT = "DrugA", PARAM = "Analyte1",
      AFRLT = c(216, 217), ARRLT = c(0, 1), NFRLT = c(216, 217),
      ATPTREF = "Day 10", DOSEA = 100,
      ROUTE = "extravascular", ADOSEDUR = 0, AVAL = c(0.8, 1.5)
    )
    urine_day10 <- data.frame(
      STUDYID = 1, USUBJID = c(1, 1), PCSPEC = "URINE",
      DOSETRT = "DrugA", PARAM = "Analyte1",
      AFRLT = c(216, 228), ARRLT = c(0, 12), NFRLT = c(216, 228),
      ATPTREF = "Day 10", DOSEA = 100,
      ROUTE = "extravascular", ADOSEDUR = 0, AVAL = c(50, 80)
    )
    mixed_data <- rbind(plasma_day1, plasma_day10, urine_day10)

    df_conc <- format_pkncaconc_data(
      mixed_data,
      group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DOSETRT", "PARAM"),
      time_column = "AFRLT",
      rrlt_column = "ARRLT",
      dose_group_columns = c("STUDYID", "USUBJID", "DOSETRT")
    )

    # Plasma Day 1 should be DOSNOA = 1
    plasma_d1 <- df_conc[df_conc$PCSPEC == "PLASMA" & df_conc$ATPTREF == "Day 1", ]
    expect_true(all(plasma_d1$DOSNOA == 1))

    # Plasma Day 10 should be DOSNOA = 2
    plasma_d10 <- df_conc[df_conc$PCSPEC == "PLASMA" & df_conc$ATPTREF == "Day 10", ]
    expect_true(all(plasma_d10$DOSNOA == 2))

    # Urine Day 10 should also be DOSNOA = 2 (same dose as Plasma Day 10)
    urine_d10 <- df_conc[df_conc$PCSPEC == "URINE" & df_conc$ATPTREF == "Day 10", ]
    expect_true(all(urine_d10$DOSNOA == 2))
  })

  it("processes multiple analytes correctly", {
    multi_analyte_adnca <- ADNCA %>% mutate(PARAM = rep(c("Analyte1", "Analyte2"), each = 10))
    df_conc <- format_pkncaconc_data(multi_analyte_adnca,
                                     group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                       "DOSETRT", "PARAM"),
                                     time_column = "AFRLT",
                                     dose_group_columns = c("STUDYID", "USUBJID", "DOSETRT"))
    expect_equal(nrow(df_conc), 20)
    expect_equal(length(unique(df_conc$PARAM)), 2)
  })
})

describe("format_pkncadose_data", {
  df_conc <- format_pkncaconc_data(ADNCA,
                                   group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                     "DOSETRT", "PARAM"),
                                   dose_group_columns = c("STUDYID", "USUBJID", "DOSETRT"))

  it("generates with no errors", {
    df_dose <- format_pkncadose_data(df_conc,
                                     group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                       "DOSETRT", "PARAM"))

    expect_s3_class(df_dose, "data.frame")
    expect_setequal(
      c(names(ADNCA), "std_route", "DOSNOA", "nca_exclude"),
      colnames(df_dose)
    )

    # Test if myintervals can be used with PKNCAdata by testing its output
    expect_no_error(
      PKNCA::PKNCAdose(
        data = df_dose,
        formula = DOSEA ~ AFRLT | STUDYID + DOSETRT + USUBJID,
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
        group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DOSETRT")
      ),
      regexp = "Input dataframe is empty. Please provide a valid concentration dataframe."
    )
  })

  it("handles missing columns", {
    missing_col_df_conc <- df_conc %>% ungroup() %>% select(-DOSETRT)
    expect_error(
      format_pkncadose_data(
        missing_col_df_conc,
        group_columns = c("STUDYID", "USUBJID", "DOSETRT")
      ),
      regexp = "Missing required columns: DOSETRT"
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
                                                       "DOSETRT", "PARAM"),
                                     time_column = "AFRLT",
                                     dose_group_columns = c("STUDYID", "USUBJID", "DOSETRT"))
    df_dose <- format_pkncadose_data(df_conc,
                                     group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                       "DOSETRT", "PARAM"))
    expect_true(all(df_dose$AFRLT >= 0))
  })
})
