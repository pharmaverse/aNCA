CDISC_COLS <- metadata_nca_variables %>%
  filter(Dataset %in% c("ADPC", "ADPP", "PP")) %>%
  arrange(Order) %>%
  split(.[["Dataset"]])

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
    VISIT = paste0("VISIT ", ATPTREF),
    ATPTREFN = as.integer(factor(ATPTREF)),
    VISITNUM = ATPTREFN,
    EPOCH = "OBSERVATION"
  )
test_pknca_res$data$dose$data <- test_pknca_res$data$dose$data %>%
  mutate(
    PCRFTDTC = case_when(
      ATPTREF == 1 ~ "2023-01-01T00:00",
      ATPTREF == 2 ~ "2023-01-02T00:00",
      TRUE ~ NA_character_
    ),
    PCRFTDTM = case_when(
      ATPTREF == 1 ~ "2023-01-01 00:00",
      ATPTREF == 2 ~ "2023-01-02 00:00",
      TRUE ~ NA_character_
    )
  )

describe("metadata_nca_variables is consistent with what is expected", {
  pp_var <- CDISC_COLS$PP %>%
    filter(Variable == "PTAETORD")
  expect_equal(
    pp_var$Label,
    "Planned Order of Element within Arm"
  )
  adpp_var <- CDISC_COLS$ADPP %>%
    filter(Variable == "PPFAST")
  expect_equal(
    adpp_var$Label,
    "Fasting Status"
  )
  adpc_var <- CDISC_COLS$ADPC %>%
    filter(Variable == "ROUTE")
  expect_equal(
    adpc_var$Label,
    "Route of Administration"
  )
})

describe("export_cdisc", {
  it("exports a list with CDISC objects", {
    result <- export_cdisc(test_pknca_res)
    expect_type(result, "list")
    expect_named(result, c("pp", "adpp", "adpc"))
  })

  it("exports a PP dataset with CDISC labels", {
    result <- export_cdisc(test_pknca_res)
    pp <- result$pp
    expect_s3_class(pp, "data.frame")
    expect_true(all(names(pp) %in% CDISC_COLS$PP$Variable))
    expect_equal(nrow(pp), 12)
    expect_equal(
      unname(formatters::var_labels(pp)),
      translate_terms(names(pp), "Variable", "Label", metadata_nca_variables)
    )
  })

  it("exports a ADPP dataset with CDISC labels", {
    result <- export_cdisc(test_pknca_res)
    adpp <- result$adpp
    expect_s3_class(adpp, "data.frame")
    expect_true(all(names(adpp) %in% CDISC_COLS$ADPP$Variable))
    expect_equal(nrow(adpp), 12)
    expect_equal(
      unname(formatters::var_labels(adpp)),
      translate_terms(names(adpp), "Variable", "Label", metadata_nca_variables)
    )
  })

  it("exports a ADPC dataset with CDISC labels", {
    result <- export_cdisc(test_pknca_res)
    adpc <- result$adpc
    expect_s3_class(adpc, "data.frame")
    expect_true(all(names(adpc) %in% CDISC_COLS$ADPC$Variable))
    expect_equal(nrow(adpc), nrow(test_pknca_res$data$conc$data))
    expect_equal(
      unname(formatters::var_labels(adpc)),
      translate_terms(names(adpc), "Variable", "Label", metadata_nca_variables)
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

  it("derives PPSTINT and PPENINT for interval (INT) parameters", {
    for (int_param in c("AUCINT", "CAVGINT", "AUCINTD", "AUCINTPD", "RCAMINT")) {
      modified_test_pknca_res <- test_pknca_res
      modified_test_pknca_res$result <- modified_test_pknca_res$result %>%
        mutate(PPTESTCD = translate_terms(PPTESTCD)) %>%
        mutate(PPTESTCD = ifelse(PPTESTCD == "AUCINT", int_param, PPTESTCD))

      result <- export_cdisc(modified_test_pknca_res)

      # Check that PPSTINT and PPENINT are derived correctly
      expect_true("PPSTINT" %in% names(result$pp))
      expect_true("PPENINT" %in% names(result$pp))
      expect_equal(
        result$pp$PPSTINT[which(result$pp$PPTESTCD == int_param)],
        rep(c("PT0H", "PT2H"), times = 3)
      )
      expect_equal(
        result$pp$PPENINT[which(result$pp$PPTESTCD == int_param)],
        rep(c("PT2H", "PT4H"), times = 3)
      )
    }
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
    expected_val <- paste0(unique(test_pknca_res$data$dose$data$PCRFTDTC), ":00") %>%
      as.character()

    expect_equal(unique(res$pp$PPRFTDTC), expected_val)
    expect_equal(unique(res_no_pcrftdtc$pp$PPRFTDTC), expected_val)
    expect_equal(unique(res_no_pcrftdtm$pp$PPRFTDTC), expected_val)
    expect_equal(unique(res_nothing$pp$PPRFTDTC), NA_character_)
  })

  it("derives PPGRPID correctly, using ATPTREF and/or PARAM, PCSPEC.", {
    test_no_atptref <- test_pknca_res
    test_no_param_pcspec <- test_pknca_res

    test_no_atptref$data$conc$data <- test_no_atptref$data$conc$data %>%
      select(-any_of("ATPTREF"))
    test_no_atptref$data$dose$data <- test_no_atptref$data$dose$data %>%
      select(-any_of("ATPTREF"))
    test_no_atptref$result <- test_no_atptref$result %>%
      select(-any_of("ATPTREF"))

    test_no_param_pcspec$data$conc$data <- test_no_param_pcspec$data$conc$data %>%
      select(-any_of(c("PCSPEC")))
    test_no_param_pcspec$data$dose$data <- test_no_param_pcspec$data$dose$data %>%
      select(-any_of(c("PCSPEC")))
    test_no_param_pcspec$result <- test_no_atptref$result %>%
      select(-any_of(c("PCSPEC")))
    test_no_param_pcspec$data$conc$columns$groups$group_vars <- "USUBJID"

    res <- export_cdisc(test_pknca_res)
    res_no_atptref <- export_cdisc(test_no_atptref)
    res_no_param_pcspec <- export_cdisc(test_no_param_pcspec)

    # Check that PPGRPID is derived correctly
    conc_group_cols <- c(group_vars(test_pknca_res$data$conc), "ATPTREF")
    group_dose_cols <- group_vars(test_pknca_res$data$dose)
    exp_grpid <- as.data.frame(test_pknca_res) %>%
      left_join(test_pknca_res$data$conc$data %>%
                  select(all_of(c(conc_group_cols, "ATPTREF", "VISIT"))) %>%
                  group_by(!!!syms(conc_group_cols)) %>%
                  slice(1),
                by = conc_group_cols) %>%
      # Expected derivations
      mutate(
        GRPID_REGULAR = paste0(PARAM, "-", PCSPEC, "-", ATPTREF),
        GRPID_NO_ATPTREF = paste0(PARAM, "-", PCSPEC)
      ) %>%
      # Arrange in same order as results
      arrange(!!!syms(c(group_dose_cols, "start", "end", "PPTESTCD")))

    expect_equal(res$pp$PPGRPID, exp_grpid$GRPID_REGULAR, ignore_attr = TRUE)
    expect_equal(res_no_atptref$pp$PPGRPID, exp_grpid$GRPID_NO_ATPTREF, ignore_attr = TRUE)
  })

  it("does not derive SUBJID if not present with USUBJID, STUDYID", {
    test_with_subjid <- test_pknca_res
    test_with_subjid$data$conc$data <- test_pknca_res$data$conc$data %>%
      filter(USUBJID %in% unique(USUBJID)[1:2]) %>%
      mutate(
        STUDYID = "S1",
        SUBJID = ifelse(USUBJID == unique(USUBJID)[1], 1, 2),
        USUBJID = paste0(STUDYID, "-", SUBJID)
      )
    test_with_subjid$data$dose$data <- test_pknca_res$data$dose$data %>%
      filter(USUBJID %in% unique(USUBJID)[1:2]) %>%
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
    expected_vals <- as.character(test_with_subjid$data$conc$data$SUBJID)
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
  it("differentiates cl.xxx for extravascular (bioavailability, F) and intravascular", {
    test_cl_data <- FIXTURE_PKNCA_DATA
    test_cl_data$intervals <- test_cl_data$intervals %>%
      filter(USUBJID %in% unique(USUBJID)[c(5, 7)]) %>%
      mutate(
        cl.last = TRUE,
        cl.obs = TRUE,
        cl.pred = TRUE,
        cl.all = TRUE
      )
    test_cl_result <- suppressWarnings(PKNCA::pk.nca(test_cl_data))
    test_cl_result$result <- test_cl_result$result %>%
      filter(PPTESTCD %in% c("cl.last", "cl.obs", "cl.pred")) %>%
      mutate(
        PPSTRES = PPORRES,
        PPSTRESU = PPORRESU
      )

    res <- export_cdisc(test_cl_result)

    res_of_infusion_subj <- res$pp %>% filter(USUBJID == unique(USUBJID)[1])
    res_of_ev_subj <- res$pp %>% filter(USUBJID == unique(USUBJID)[2])

    expect_true(all(res_of_infusion_subj$PPTESTCD %in% c("CLO", "CLP", "CLALL", "CLLST")))
    expect_true(all(
      res_of_infusion_subj$PPTEST %in% c(
        "Total CL Obs",
        "Total CL Pred",
        "CL (based on AUCall)",
        "CL (based on AUClast)"
      )
    ))
    expect_true(all(res_of_ev_subj$PPTESTCD %in% c("CLFO", "CLFP", "CLFLST")))
    expect_true(all(
      res_of_ev_subj$PPTEST %in% c(
        "Total CL Obs by F",
        "Total CL Pred by F",
        "CL (based on AUCall) by F",
        "CL (based on AUClast) by F"
      )
    ))
  })

  it("differentiates vz.xxx for extravascular (bioavailability, F) and intravascular", {
    test_vz_data <- FIXTURE_PKNCA_DATA
    test_vz_data$intervals <- test_vz_data$intervals %>%
      filter(USUBJID %in% unique(USUBJID)[c(5, 7)]) %>%
      mutate(
        vz.last = TRUE,
        vz.obs = TRUE,
        vz.pred = TRUE
      )
    test_vz_result <- suppressWarnings(PKNCA::pk.nca(test_vz_data))
    test_vz_result$result <- test_vz_result$result %>%
      filter(PPTESTCD %in% c("vz.last", "vz.obs", "vz.pred")) %>%
      mutate(
        PPSTRES = PPORRES,
        PPSTRESU = PPORRESU
      )

    res <- export_cdisc(test_vz_result)

    res_of_infusion_subj <- res$pp %>% filter(USUBJID == unique(USUBJID)[1])
    res_of_ev_subj <- res$pp %>% filter(USUBJID == unique(USUBJID)[2])

    expect_true(all(res_of_infusion_subj$PPTESTCD %in% c("VZO", "VZP")))
    expect_true(all(
      res_of_infusion_subj$PPTEST %in% c(
        "Vz Obs",
        "Vz Pred"
      )
    ))
    expect_true(all(res_of_ev_subj$PPTESTCD %in% c("VZFO", "VZFP")))
    expect_true(all(res_of_ev_subj$PPTEST %in% c(
      "Vz Obs by F",
      "Vz Pred by F"
    )))
  })

  it("differentiates mrt.xxx for extravascular (EV), infusion (IC) and bolus (IB)", {
    test_mrt_data <- FIXTURE_PKNCA_DATA
    test_mrt_data$intervals <- test_mrt_data$intervals %>%
      filter(USUBJID %in% unique(USUBJID)[c(5, 6, 7)]) %>%
      mutate(
        mrt.last = TRUE,
        mrt.obs = TRUE,
        mrt.pred = TRUE
      )
    test_mrt_result <- suppressWarnings(PKNCA::pk.nca(test_mrt_data))
    test_mrt_result$result <- test_mrt_result$result %>%
      filter(PPTESTCD %in% c("mrt.last", "mrt.obs", "mrt.pred")) %>%
      mutate(
        PPSTRES = PPORRES,
        PPSTRESU = PPORRESU
      )

    res <- export_cdisc(test_mrt_result)

    res_of_infusion_subj <- res$pp %>% filter(USUBJID == unique(USUBJID)[1])
    res_of_bolus_subj <- res$pp %>% filter(USUBJID == unique(USUBJID)[2])
    res_of_ev_subj <- res$pp %>% filter(USUBJID == unique(USUBJID)[3])

    expect_true(all(res_of_infusion_subj$PPTESTCD %in% c("MRTICIFO", "MRTICIFP", "MRTICLST")))
    expect_true(all(
      res_of_infusion_subj$PPTEST %in% c(
        "MRT IV Cont Inf Infinity Obs",
        "MRT IV Cont Inf Infinity Pred",
        "MRT IV Cont Inf to Last Nonzero Conc"
      )
    ))
    expect_true(all(res_of_bolus_subj$PPTESTCD %in% c("MRTIBIFO", "MRTIBIFP", "MRTIBLST")))
    expect_true(all(
      res_of_bolus_subj$PPTEST %in% c(
        "MRT IV Bolus Infinity Obs",
        "MRT IV Bolus Infinity Pred",
        "MRT IV Bolus to Last Nonzero Conc"
      )
    ))
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
