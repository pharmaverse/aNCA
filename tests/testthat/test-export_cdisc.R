
# Simplified test_pknca_res object for testing with additional variables
test_pknca_res <- list(
  result = data.frame(
    USUBJID = rep(c("S1-1", "S1-2"), each = 3),
    NCA_PROFILE = c(1, 1, 1, 1, 1, 1),
    PPTESTCD = c("AUCINF", "AUCIFO", "R2", "AUCINF", "AUCINT", "R2"),
    PPORRES = c(100, 95, 0.98, NA, 115, 0.96),
    PPSTRES = c(100, 95, 0.98, NA, 115, 0.96),
    PPSTRESU = c("ng*h/mL", "ng*h/mL", "", "ng*h/mL", "ng*h/mL", ""),
    PPORRESU = c("ng*h/mL", "ng*h/mL", "", "ng*h/mL", "ng*h/mL", ""),
    exclude = c(NA, NA, NA, "Excluded due to protocol deviation", NA, NA),
    PARAM = c("Analyte1", "Analyte1", "Analyte1", "Analyte1", "Analyte1", "Analyte1"),
    PCSPEC = c("Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma"),
    start = c(0, 0, 0, 0, 2, 2),  # Start times for intervals
    end = c(5, 5, 5, 5, 4, 4)     # End times for intervals
  ),
  data = list(
    dose = list(
      data = data.frame(
        USUBJID = c("S1-1", "S1-2"),
        STUDYID = "S1",
        NCA_PROFILE = c(1, 1),
        ROUTE = c("Oral", "Oral"),
        RRLTU = c("h", "h"),
        DOSEA = c(100, 200),
        DOSEU = c("mg", "mg"),
        # Extra variables only added if present (PP / ADPP)
        SEX = c("M", "F"),
        RACE = c("White", "Black"),
        AGE = c(30, 25),
        AGEU = c("years", "years"),
        SITEID = c("Site1", "Site2"),
        ACTARM = c("Treatment A", "Treatment B"),
        TRT01P = c("Drug A", "Drug B"),
        TRT01A = c("Drug A", "Drug B"),
        PCRFTDTC = "2020-01-01T08:30:00",
        PCRFTDTM = "2020-01-01 08:30:00",
        # Just to derive PPGRPID
        AVISIT = c("Visit 1", "Visit 1"),
        VISIT = c("Visit 1", "Visit 1"),
        VISITNUM = c(1, 1)
      ),
      columns = list(
        groups = c("USUBJID", "NCA_PROFILE"),
        route = "ROUTE"
      )
    ),
    conc = list(
      data = data.frame(
        USUBJID = rep(c("S1-1", "S1-2"), each = 5),
        STUDYID = "S1",
        PARAM = rep("Analyte1", 10),
        AVAL = c(0, 10, 20, 30, 40, 0, 15, 25, 35, 45),
        AFRLT = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
        ARRLT = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
        NFRLT = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
        NRRLT = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
        PCSPEC = rep("Plasma", 10),
        AVALU = rep("ng/mL", 10),
        is.excluded.hl = rep(FALSE, 10),
        # Extra variables only added if present (ADPC)
        SEX = c("M", "F", "M", "F", "M", "M", "F", "M", "F", "M"),
        RACE = c("White", "Black", "White", "Black", "White",
                 "White", "Black", "White", "Black", "White"),
        AGE = c(30, 25, 30, 25, 30, 30, 25, 30, 25, 30),
        AGEU = rep("years", 10),
        ACTARM = rep(c("Treatment A", "Treatment B"), each = 5),
        STUDYID = rep("S1", 10),
        ATPT = rep(c("Pre-dose", "Post-dose"), each = 5),
        ATPTN = rep(c(0, 1), each = 5),
        ANL01FL = rep("Y", 10),
        SITEID = rep(c("Site1", "Site2"), each = 5),
        # These variables are only added if present but could be derived
        PCSTRESC = c("BLQ", "10", "20", "30", "40", "BLQ", "15", "25", "35", "45"),
        PCSTRESN = c(0, 10, 20, 30, 40, 0, 15, 25, 35, 45),
        PCSTRESU = rep("ng/mL", 10),
        PCORRES = c(0, 10, 20, 30, 40, 0, 15, 25, 35, 45),
        PCORRESU = rep("ng/mL", 10),
        PCTPT = rep(c("Pre-dose", "Post-dose"), each = 5),
        PCTPTNUM = rep(c(0, 1), each = 5),
        PCTPTREF = "Most Recent Dose",
        AVISIT = c("Visit 1", "Visit 2", "Visit 3", "Visit 4", "Visit 5",
                   "Visit 1", "Visit 2", "Visit 3", "Visit 4", "Visit 5"),
        VISIT = c("Visit 1", "Visit 2", "Visit 3", "Visit 4", "Visit 5",
                  "Visit 1", "Visit 2", "Visit 3", "Visit 4", "Visit 5"),
        AVISITN = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
        VISITNUM = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
      ),
      columns = list(
        groups = c("USUBJID", "PARAM")
      )
    )
  )
)

describe("export_cdisc", {
  it("exports CDISC-compliant datasets (PP, ADPP, ADPC)", {
    result <- export_cdisc(test_pknca_res)

    # Check that the result contains the expected components
    expect_type(result, "list")
    expect_named(result, c("pp", "adpp", "adpc", "studyid"))

    # Check the PP
    pp <- result$pp
    expect_s3_class(pp, "data.frame")
    expect_true(all(CDISC_COLS$PP %in% names(pp)))
    expect_equal(nrow(pp), 6) # Ensure all variables are included

    # Check the ADPP
    adpp <- result$adpp
    expect_s3_class(adpp, "data.frame")
    expect_true(all(CDISC_COLS$ADPP %in% names(adpp)))
    expect_equal(nrow(adpp), 6) # Ensure all variables are included

    # Check the ADPC
    adpc <- result$adpc
    expect_s3_class(adpc, "data.frame")
    expect_true(all(CDISC_COLS$ADPC %in% names(adpc)))
    expect_equal(nrow(adpc), 10) # Ensure all concentration data is included

    # Check the studyid
    expect_equal(result$studyid, "S1")
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
