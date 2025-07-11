CDISC_COLS <- metadata_variables %>%
  filter(Dataset %in% c("ADPC", "ADPP", "PP")) %>%
  arrange(Order) %>%
  group_by(Dataset) %>%
  group_split() %>%
  setNames(c("ADPC", "ADPP", "PP"))

# Subset results just to simplify the testing
test_pknca_res <- FIXTURE_PKNCA_RES %>%
  mutate(PPTESTCD = translate_terms(
    PPTESTCD, "PPTESTCD", "PKNCA"
  )) %>%
  filter(
    USUBJID %in% unique(USUBJID)[1:2],
    PPTESTCD %in% c("cmax", "aucint.last", "r.squared")
  )

# Add special columns for testing in the concentration data
test_pknca_res$data$conc$data <- test_pknca_res$data$conc$data %>%
  mutate(
    ATPT = "Post-dose",
    ATPTN = 1,
    PCTPT = "Post-dose",
    PCTPTNUM = 1,
    PCTPTREF = "Most Recent Dose",
    AVISIT = paste0("VISIT ", as.integer(NFRLT %% 24)),
    VISIT = AVISIT,
    AVISITN = as.integer(NFRLT %% 24),
    VISITNUM = AVISITN,
    EPOCH = "OBSERVATION"
  )
test_pknca_res$data$dose$data <- test_pknca_res$data$dose$data %>%
  mutate(
    PCRFTDTC = case_when(
      NCA_PROFILE == 1 ~ "2023-01-01T00:00",
      NCA_PROFILE == 2 ~ "2023-01-02T00:00",
      TRUE ~ NA_character_
    ),
    PCRFTDTM = case_when(
      NCA_PROFILE == 1 ~ "01-01-2023 00:00",
      NCA_PROFILE == 2 ~ "02-01-2023 00:00",
      TRUE ~ NA_character_
    )
  )

describe("export_cdisc", {
  it("exports CDISC-compliant datasets (PP, ADPP, ADPC)", {
    result <- export_cdisc(test_pknca_res)

    # Check that the result contains the expected components
    expect_type(result, "list")
    expect_named(result, c("pp", "adpp", "adpc"))

    # Check the PP
    pp <- result$pp
    expect_s3_class(pp, "data.frame")
    expect_true(all(names(pp) %in% CDISC_COLS$PP$Variable))
    expect_equal(nrow(pp), 9)
    expect_equal(
      unname(formatters::var_labels(pp)),
      translate_terms(names(pp), "Variable", "Label", metadata_variables)
    )

    # Check the ADPP
    adpp <- result$adpp
    expect_s3_class(adpp, "data.frame")
    expect_true(all(names(adpp) %in% CDISC_COLS$ADPP$Variable))
    expect_equal(nrow(adpp), 9) # Ensure all variables are included
    expect_equal(
      unname(formatters::var_labels(adpp)),
      translate_terms(names(adpp), "Variable", "Label", metadata_variables)
    )

    # Check the ADPC
    adpc <- result$adpc
    expect_s3_class(adpc, "data.frame")
    expect_true(all(names(adpc) %in% CDISC_COLS$ADPC$Variable))
    expect_equal(nrow(adpc), nrow(test_pknca_res$data$conc$data))
    expect_equal(
      unname(formatters::var_labels(adpc)),
      translate_terms(names(adpc), "Variable", "Label", metadata_variables)
    )
  })

  it("derives when possible ATPT, ATPTN, ATPTREF (PCTPT, PCTPTNUM, PCTPTREF)", {
    test_pknca_res_no_atpt <- test_pknca_res
    test_pknca_res_no_atpt$data$conc$data <- test_pknca_res_no_atpt$data$conc$data %>%
      select(-ATPT, -ATPTN)

    res_no_atpt_vars <- export_cdisc(test_pknca_res_no_atpt)

    # Check derivation when PCTPT, PCTPTNUM, PCTPTREF are present
    expect_equal(
      unique(res_no_atpt_vars$adpc$ATPT),
      unique(test_pknca_res_no_atpt$data$conc$data$PCTPT)
    )
    expect_equal(
      unique(res_no_atpt_vars$adpc$ATPTN),
      unique(test_pknca_res_no_atpt$data$conc$data$PCTPTNUM)
    )
    expect_equal(
      unique(res_no_atpt_vars$adpc$ATPTREF),
      unique(test_pknca_res_no_atpt$data$conc$data$PCTPTREF)
    )

    # Check is NA when PCTPT, PCTPTNUM, PCTPTREF are not present
    test_pknca_res_no_pctpt <- test_pknca_res_no_atpt
    test_pknca_res_no_pctpt$data$conc$data <- test_pknca_res_no_atpt$data$conc$data %>%
      select(-PCTPT, -PCTPTNUM, -PCTPTREF)
    res_no_atpt_pctpt_vars <- export_cdisc(test_pknca_res_no_pctpt)

    expect_equal(
      unique(res_no_atpt_pctpt_vars$adpc$ATPT),
      NA_character_
    )
    expect_equal(
      unique(res_no_atpt_pctpt_vars$adpc$ATPTN),
      NA
    )
    expect_equal(
      unique(res_no_atpt_pctpt_vars$adpc$ATPTREF),
      NA_character_
    )
  })

  it("derives PPSTINT and PPENINT for partial AUC intervals", {
    modified_test_pknca_res <- test_pknca_res
    modified_test_pknca_res$result <- modified_test_pknca_res$result %>%
      mutate(PPTESTCD = ifelse(PPTESTCD == "AUCINT", "AUCINT", PPTESTCD))

    result <- export_cdisc(modified_test_pknca_res)

    # Check that PPSTINT and PPENINT are derived correctly
    expect_true("PPSTINT" %in% names(result$pp))
    expect_true("PPENINT" %in% names(result$pp))
    expect_equal(
      result$pp$PPSTINT[which(result$pp$PPTESTCD == "AUCINT")],
      rep(c("PT0H", "PT2H"), times = 3)
    )
    expect_equal(
      result$pp$PPENINT[which(result$pp$PPTESTCD == "AUCINT")],
      rep(c("PT2H", "PT4H"), times = 3)
    )
  })

  it("derives PPREASND & PPSTAT correctly in all situations", {
    modified_test_pknca_res <- test_pknca_res
    modified_test_pknca_res$result <- modified_test_pknca_res$result %>%
      mutate(
        # Case NA without reason
        PPSTRES = ifelse(PPTESTCD == "aucint.last", NA, PPSTRES),
        exclude = ifelse(PPTESTCD == "cmax", NA, exclude),
        # Case NA with reason
        PPSTRES = ifelse(PPTESTCD == "cmax" & USUBJID == unique(USUBJID)[1], NA, PPSTRES),
        exclude = ifelse(
          PPTESTCD == "cmax" & USUBJID == unique(USUBJID)[1],
          "Excluded due to protocol deviation",
          exclude
        ),
        # Reason with > 200 characters
        PPSTRES = ifelse(PPTESTCD == "cmax" & USUBJID == unique(USUBJID)[2], NA, PPSTRES),
        exclude = ifelse(
          PPTESTCD == "cmax" & USUBJID == unique(USUBJID)[2],
          paste(rep("A", 201), collapse = ""),
          exclude
        ),
      )

    result <- export_cdisc(modified_test_pknca_res)

    expect_equal(filter(result$pp, PPTESTCD == "AUCINT") %>%
                   pull(PPREASND) %>%
                   unique(),
                 "NOT DERIVED")
    expect_equal(filter(result$pp, PPTESTCD == "CMAX" & USUBJID == unique(USUBJID)[1]) %>%
                   pull(PPREASND) %>%
                   unique(),
                 "Excluded due to protocol deviation")
    expect_equal(filter(result$pp, PPTESTCD == "CMAX" & USUBJID == unique(USUBJID)[2]) %>%
                   pull(PPREASND) %>%
                   unique(),
                 paste0(rep("A", 200), collapse = ""))
  })

  it("derives correctly PPRFTDTC (if either PCRFTDTC or PCRFTDTM are present)", {
    test_no_pcrftdtc <- test_pknca_res
    test_no_pcrftdtm <- test_pknca_res
    test_nothing <- test_pknca_res

    res <- export_cdisc(test_pknca_res)
    test_no_pcrftdtc$data$dose$data <- test_no_pcrftdtc$data$dose$data %>%
      select(-PCRFTDTC)
    res_no_pcrftdtc <- export_cdisc(test_no_pcrftdtc)
    test_no_pcrftdtm$data$dose$data <- test_no_pcrftdtm$data$dose$data %>%
      select(-PCRFTDTM)
    res_no_pcrftdtm <- export_cdisc(test_no_pcrftdtm)
    test_nothing$data$dose$data <- test_nothing$data$dose$data %>%
      select(-PCRFTDTC, -PCRFTDTM)
    res_nothing <- export_cdisc(test_nothing)


    # Check that PPRFTDTC is derived correctly (specifying also seconds)
    expected_val <- paste0(unique(test_pknca_res$data$dose$data$PCRFTDTC), ":00")

    expect_equal(unique(res$pp$PPRFTDTC), expected_val)
    expect_equal(unique(res_no_pcrftdtc$pp$PPRFTDTC), expected_val)
    expect_equal(unique(res_no_pcrftdtm$pp$PPRFTDTC), expected_val)
    expect_equal(unique(res_nothing$pp$PPRFTDTC), NA_character_)
  })

  it("derives PPGRPID correctly, using AVISIT, VISIT and/or PARAM, PCSPEC. NCA_PROFILE", {
    test_no_avisit <- test_pknca_res
    test_no_avisit_visit <- test_pknca_res
    test_nothing <- test_pknca_res

    test_no_avisit$data$conc$data <- test_no_avisit$data$conc$data %>%
      select(-AVISIT)
    test_no_avisit_visit$data$conc$data <- test_no_avisit_visit$data$conc$data %>%
      select(-AVISIT, -VISIT)
    test_nothing$data$conc$data <- test_nothing$data$conc$data %>%
      select(-AVISIT, -VISIT)
    test_nothing$result <- test_nothing$result %>% select(-NCA_PROFILE)

    res <- export_cdisc(test_pknca_res)
    res_no_avisit <- export_cdisc(test_no_avisit)
    res_no_avisit_visit <- export_cdisc(test_no_avisit_visit)

    # Check that PPGRPID is derived correctly
    conc_group_cols <- c(group_vars(test_pknca_res$data$conc), "NCA_PROFILE")
    group_dose_cols <- group_vars(test_pknca_res$data$dose)
    exp_grpid <- as.data.frame(test_pknca_res, filter_requested = TRUE) %>%
      left_join(test_pknca_res$data$conc$data %>%
                  select(all_of(c(conc_group_cols, "AVISIT", "VISIT"))) %>%
                  group_by(!!!syms(conc_group_cols)) %>%
                  slice(1),
                by = conc_group_cols) %>%
      # Expected derivations
      mutate(
        GRPID_VISIT = paste0(PARAM, "-", PCSPEC, "-", VISIT),
        GRPID_AVISIT = paste0(PARAM, "-", PCSPEC, "-", AVISIT),
        GRPID_NCAPROFILE = paste0(PARAM, "-", PCSPEC, "-", NCA_PROFILE)
      ) %>%
      # Arrange in same order as results
      arrange(!!!syms(c(group_dose_cols, "start", "end", "PPTESTCD")))

    expect_equal(res$pp$PPGRPID, exp_grpid$GRPID_AVISIT, ignore_attr = TRUE)
    expect_equal(res_no_avisit$pp$PPGRPID, exp_grpid$GRPID_VISIT, ignore_attr = TRUE)
    expect_equal(res_no_avisit_visit$pp$PPGRPID, exp_grpid$GRPID_NCAPROFILE, ignore_attr = TRUE)
  })

  it("does not derive SUBJID if not present with USUBJID, STUDYID", {
    test_with_subjid <- test_pknca_res
    test_with_subjid$data$conc$data <- test_pknca_res$data$dose$data %>%
      mutate(
        STUDYID = "S1",
        SUBJID = ifelse(USUBJID == unique(USUBJID)[1], 1, 2),
        USUBJID = paste0(STUDYID, "-", SUBJID)
      )
    test_with_subjid$data$dose$data <- test_pknca_res$data$dose$data %>%
      mutate(
        STUDYID = "S1",
        SUBJID = ifelse(USUBJID == unique(USUBJID)[1], 1, 2),
        USUBJID = paste0(STUDYID, "-", SUBJID)
      )
    test_with_subjid$data$intervals <- test_with_subjid$data$intervals %>%
      mutate(
        STUDYID = "S1",
        SUBJID = ifelse(USUBJID == unique(USUBJID)[1], 1, 2),
        USUBJID = paste0(STUDYID, "-", SUBJID)
      )
    test_with_subjid$result <- test_with_subjid$result %>%
      mutate(
        STUDYID = "S1",
        SUBJID = ifelse(USUBJID == unique(USUBJID)[1], 1, 2),
        USUBJID = paste0(STUDYID, "-", SUBJID)
      )

    test_no_subjid <- test_with_subjid
    test_no_subjid$data$dose$data <- test_with_subjid$data$dose$data %>%
      select(-SUBJID)

    test_no_subjid_no_studyid <- test_with_subjid
    test_no_subjid_no_studyid$data$dose$data <- test_with_subjid$data$dose$data %>%
      select(-SUBJID, -STUDYID)

    res_with_subjid <- export_cdisc(test_with_subjid)
    res_no_subjid <- export_cdisc(test_no_subjid)
    res_no_subjid_no_studyid <- export_cdisc(test_no_subjid_no_studyid)

    # Check that SUBJID is derived correctly
    expected_vals <- as.character(test_with_subjid$data$dose$data$SUBJID)
    expect_equal(res_with_subjid$adpc$SUBJID, expected_vals, ignore_attr = TRUE)
    expect_equal(res_no_subjid_no_studyid$adpc$SUBJID, expected_vals, ignore_attr = TRUE)
  })

  it("derives PPFAST from EXFAST, PCFAST, or FEDSTATE as appropriate", {
    # Case 1: EXFAST present
    test_exfast <- test_pknca_res
    test_exfast$data$dose$data$EXFAST <- ifelse(
      test_exfast$data$dose$data$USUBJID == unique(test_exfast$data$dose$data$USUBJID)[1],
      "Y",
      "N"
    )
    res_exfast <- export_cdisc(test_exfast)
    expect_equal(unique(res_exfast$pp$PPFAST), c("Y", "N"))

    # Case 2: PCFAST present
    test_pcfast <- test_exfast
    test_pcfast$data$dose$data <- test_pcfast$data$dose$data %>%
      rename(PCFAST = EXFAST)
    res_pcfast <- export_cdisc(test_pcfast)
    expect_equal(unique(res_pcfast$pp$PPFAST), c("Y", "N"))

    # Case 3: FEDSTATE present
    test_fedstate <- test_exfast
    test_fedstate$data$dose$data <- test_fedstate$data$dose$data %>%
      rename(FEDSTATE = EXFAST)
    res_fedstate <- export_cdisc(test_fedstate)
    expect_equal(unique(res_fedstate$pp$PPFAST), c("Y", "N"))

    # Case 4: No variable to derive PPFAST
    test_no_fast <- test_pknca_res
    res_no_fast <- export_cdisc(test_no_fast)
    expect_true(!"PPFAST" %in% names(res_no_fast$pp))
  })

  it("derives PARAMCD (analyte) if PARAMCD or PCTESTCD are present for ADPC", {
    # Case 1: PARAMCD present
    test_paramcd <- test_pknca_res
    analyte_codes <- paste0(test_paramcd$data$conc$data$PARAM, 1)
    test_paramcd$data$conc$data$PARAMCD <- analyte_codes
    res_paramcd <- export_cdisc(test_paramcd)
    expect_equal(unique(res_paramcd$adpc$PARAMCD), unique(analyte_codes))

    # Case 2: PCTESTCD present
    test_pctestcd <- test_pknca_res
    test_pctestcd$data$conc$data$PCTESTCD <- analyte_codes
    res_pctestcd <- export_cdisc(test_pctestcd)
    expect_equal(unique(res_pctestcd$adpc$PARAMCD), unique(analyte_codes))

    # Case 3: Neither PARAMCD nor PCTESTCD present, then is NA
    test_no_paramcd_pctestcd <- test_pknca_res
    res_no_paramcd_pctestcd <- export_cdisc(test_no_paramcd_pctestcd)
    expect_true(all(is.na(res_no_paramcd_pctestcd$adpc$PARAMCD)))
  })

  # Performs correctly the one-to-many mappings between PKNCA and PPTESTCD
  it("derives the one-to-many mappings for mrt.xxx to distinguish extravascular (EV) and infusion (IC) denominations", {
    test_mrt_data <- FIXTURE_PKNCA_DATA
    test_mrt_data$intervals <- test_mrt_data$intervals %>%
      filter(USUBJID %in% unique(USUBJID)[c(5, 7)]) %>%
        mutate(
          mrt.last = TRUE,
          mrt.obs = TRUE,
          mrt.pred = TRUE
        )
    test_mrt_result <- PKNCA::pk.nca(test_mrt_data)
    test_mrt_result$result <- test_mrt_result$result %>%
      filter(PPTESTCD %in% c("mrt.last", "mrt.obs", "mrt.pred")) %>%
      mutate(
        PPSTRES = PPORRES,
        PPSTRESU = PPORRESU
      )

    res <- export_cdisc(test_mrt_result)
    
    res_of_infusion_subj <- res$pp %>% filter(USUBJID == unique(USUBJID)[1])
    res_of_ev_subj <- res$pp %>% filter(USUBJID == unique(USUBJID)[2])

    expect_true(all(res_of_infusion_subj$PPTESTCD %in% c("MRTICIFO", "MRTICIFP", "MRTICLST")))
    expect_true(all(
      res_of_infusion_subj$PPTEST %in% c(
        "MRT IV Cont Inf Infinity Obs",
        "MRT IV Cont Inf Infinity Pred",
        "MRT IV Cont Inf to Last Nonzero Conc"
    )))
    expect_true(all(res_of_ev_subj$PPTESTCD %in% c("MRTEVIFO", "MRTEVIFP", "MRTEVLST")))
    expect_true(all(res_of_ev_subj$PPTEST %in% c(
      "MRT Extravasc Infinity Obs",
      "MRT Extravasc Infinity Pred",
      "MRT Extravasc to Last Nonzero Conc"
    )))
  })
})

describe(".get_subjid", {
  it("returns SUBJID when present", {
    data <- data.frame(SUBJID = c("1", "2"))
    result <- get_subjid(data)
    expect_equal(result, c("1", "2"))
  })

  it("derives SUBJID when USUBJID & STUDYID are present", {
    data <- data.frame(USUBJID = c("S1-1", "S1-2"), STUDYID = "S1")
    result <- get_subjid(data)
    expect_equal(result, c("1", "2"))
  })

  it("returns NA when neither USUBJID nor SUBJID are present", {
    data <- data.frame()
    result <- get_subjid(data)
    expect_true(all(is.na(result)))
  })
})
