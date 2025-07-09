CDISC_COLS <- metadata_variables %>%
  filter(Dataset %in% c("ADPC", "ADPP", "PP")) %>%
  arrange(Order) %>%
  group_by(Dataset) %>%
  group_split() %>%
  setNames(c("ADPC", "ADPP", "PP"))

# Subset results just to simplify the testing
test_pknca_res <- FIXTURE_PKNCA_RES %>%
  filter(
    USUBJID %in% unique(USUBJID)[1:2],
    PPTESTCD %in% c("CMAX", "AUCINT", "R2")
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

    # Check the ADPP
    adpp <- result$adpp
    expect_s3_class(adpp, "data.frame")
    expect_true(all(names(adpp) %in% CDISC_COLS$ADPP$Variable))
    expect_equal(nrow(adpp), 9) # Ensure all variables are included

    # Check the ADPC
    adpc <- result$adpc
    expect_s3_class(adpc, "data.frame")
    expect_true(all(names(adpc) %in% CDISC_COLS$ADPC$Variable))
    expect_equal(nrow(adpc), nrow(test_pknca_res$data$conc$data))
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
    expect_equal(result$pp$PPSTINT[which(result$pp$PPTESTCD == "AUCINT")], "PT2H")
    expect_equal(result$pp$PPENINT[which(result$pp$PPTESTCD == "AUCINT")], "PT4H")
  })

  it("derives PPREASND & PPSTAT correctly in all situations", {
    modified_test_pknca_res <- test_pknca_res
    modified_test_pknca_res$result <- modified_test_pknca_res$result %>%
      mutate(
        # Case NA without reason
        PPSTRES = ifelse(PPTESTCD == "AUCINT", NA, PPSTRES),
        exclude = ifelse(PPTESTCD == "AUCINT", NA, exclude),
        # Case NA with reason
        PPSTRES = ifelse(PPTESTCD == "AUCINF", NA, PPSTRES),
        exclude = ifelse(PPTESTCD == "AUCINF", "Excluded due to protocol deviation", exclude),
        # Reason with > 200 characters
        exclude = ifelse(PPTESTCD == "R2", paste(rep("A", 201), collapse = ""), exclude),
        PPSTRES = ifelse(PPTESTCD == "R2", NA, PPSTRES)
      )

    result <- export_cdisc(modified_test_pknca_res)

    expect_equal(filter(result$pp, PPTESTCD == "AUCINT") %>%
                   pull(PPREASND) %>%
                   unique(),
                 "NOT DERIVED")
    expect_equal(filter(result$pp, PPTESTCD == "AUCINF") %>%
                   pull(PPREASND) %>%
                   unique(),
                 "Excluded due to protocol deviation")
    expect_equal(filter(result$pp, PPTESTCD == "R2") %>%
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


    # Check that PPRFTDTC is derived correctly
    expected_val <- unique(test_pknca_res$data$dose$data$PCRFTDTC)

    expect_equal(unique(res$pp$PPRFTDTC), expected_val)
    expect_equal(unique(res_no_pcrftdtc$pp$PPRFTDTC), expected_val)
    expect_equal(unique(res_no_pcrftdtm$pp$PPRFTDTC), expected_val)
    expect_equal(unique(res_nothing$pp$PPRFTDTC), NA_character_)
  })

  it("derives PPGRPID correctly, using AVISIT, VISIT and/or PARAM, PCSPEC. NCA_PROFILE", {
    test_no_avisit <- test_pknca_res
    test_no_avisit_visit <- test_pknca_res
    test_nothing <- test_pknca_res

    test_no_avisit$data$dose$data <- test_no_avisit$data$dose$data %>%
      select(-AVISIT)
    test_no_avisit_visit$data$dose$data <- test_no_avisit_visit$data$dose$data %>%
      select(-AVISIT, -VISIT)
    test_nothing$data$dose$data <- test_nothing$data$dose$data %>%
      select(-AVISIT, -VISIT)
    test_nothing$result <- test_nothing$result %>% select(-NCA_PROFILE)

    res <- export_cdisc(test_pknca_res)
    res_no_avisit <- export_cdisc(test_no_avisit)
    res_no_avisit_visit <- export_cdisc(test_no_avisit_visit)

    # Check that PPGRPID is derived correctly
    data <- left_join(test_pknca_res$result, test_pknca_res$data$dose$data,
                      by = intersect(names(test_pknca_res$result),
                                     names(test_pknca_res$data$dose$data)))
    expected_val_all <- data %>%
      mutate(PPGRPID = paste0(PARAM, "-", PCSPEC, "-", AVISIT)) %>%
      pull(PPGRPID) %>%
      unique()
    expected_val_no_avisit <- data %>%
      mutate(PPGRPID = paste0(PARAM, "-", PCSPEC, "-", VISIT)) %>%
      pull(PPGRPID) %>%
      unique()
    expected_val_no_avisit_visit <- data  %>%
      mutate(PPGRPID = paste0(PARAM, "-", PCSPEC, "-", NCA_PROFILE)) %>%
      pull(PPGRPID) %>%
      unique()

    expect_equal(unique(res$pp$PPGRPID), unique(expected_val_all))
    expect_equal(unique(res_no_avisit$pp$PPGRPID), unique(expected_val_no_avisit))
    expect_equal(unique(res_no_avisit_visit$pp$PPGRPID), unique(expected_val_no_avisit_visit))
  })

  it("does not derive SUBJID if not present with USUBJID, STUDYID", {

    test_with_subjid <- test_pknca_res
    test_with_subjid$data$dose$data <- test_pknca_res$data$dose$data %>%
      mutate(SUBJID = as.character(1:2))

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
    expected_vals <- unique(test_with_subjid$data$dose$data$SUBJID)
    expect_equal(unique(res_with_subjid$adpc$SUBJID), expected_vals)
    expect_equal(unique(res_no_subjid_no_studyid$adpc$SUBJID), expected_vals)
  })

  it("derives PPFAST from EXFAST, PCFAST, or FEDSTATE as appropriate", {
    # Case 1: EXFAST present
    test_exfast <- test_pknca_res
    test_exfast$data$dose$data$EXFAST <- c("Y", "N")
    res_exfast <- export_cdisc(test_exfast)
    expect_equal(unique(res_exfast$pp$PPFAST), c("Y", "N"))

    # Case 2: PCFAST present
    test_pcfast <- test_pknca_res
    test_pcfast$data$dose$data$PCFAST <- c("Y", "N")
    res_pcfast <- export_cdisc(test_pcfast)
    expect_equal(unique(res_pcfast$pp$PPFAST), c("Y", "N"))

    # Case 3: FEDSTATE present
    test_fedstate <- test_pknca_res
    test_fedstate$data$dose$data$FEDSTATE <- c("Y", "N")
    res_fedstate <- export_cdisc(test_fedstate)
    expect_equal(unique(res_fedstate$pp$PPFAST), c("Y", "N"))

    # Case 4: No variable to derive PPFAST
    test_no_fast <- test_pknca_res
    res_no_fast <- export_cdisc(test_no_fast)
    expect_true(!"PPFAST" %in% names(res_no_fast$pp))
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
