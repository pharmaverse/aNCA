describe("std_dtc_to_rdate", {
  it("parses full ISO 8601 datetime", {
    result <- std_dtc_to_rdate("2024-01-15T08:00:00")
    expect_s3_class(result, "POSIXct")
    expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2024-01-15 08:00:00")
  })

  it("parses datetime without seconds", {
    result <- std_dtc_to_rdate("2024-01-15T08:00")
    expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2024-01-15 08:00:00")
  })

  it("parses date-only format", {
    result <- std_dtc_to_rdate("2024-01-15")
    expect_equal(format(result, "%Y-%m-%d"), "2024-01-15")
  })

  it("handles mixed precision in a vector", {
    input <- c("2024-01-15T08:00:00", "2024-01-16", "2024-01-17T12:30")
    result <- std_dtc_to_rdate(input)
    expect_length(result, 3)
    expect_false(any(is.na(result)))
  })

  it("returns NA for unparseable strings", {
    result <- std_dtc_to_rdate("not-a-date")
    expect_true(is.na(result))
  })
})


describe("parse_iso8601_duration", {
  it("parses hours", {
    expect_equal(parse_iso8601_duration("PT2H"), 2)
  })

  it("parses minutes", {
    expect_equal(parse_iso8601_duration("PT90M"), 1.5)
  })

  it("parses seconds", {
    expect_equal(parse_iso8601_duration("PT3600S"), 1)
  })

  it("parses combined H and M", {
    expect_equal(parse_iso8601_duration("PT1H30M"), 1.5)
  })

  it("parses negative durations", {
    expect_equal(parse_iso8601_duration("PT-0.083H"), -0.083)
  })

  it("returns NA for non-duration strings", {
    expect_true(is.na(parse_iso8601_duration("not-a-duration")))
  })

  it("returns NA for NA input", {
    expect_true(is.na(parse_iso8601_duration(NA)))
  })

  it("handles decimal hours", {
    expect_equal(parse_iso8601_duration("PT2.889H"), 2.889)
  })
})


describe("route_cdisc_to_pknca", {
  it("maps IV routes to intravascular", {
    iv_routes <- c("INTRAVENOUS DRIP", "IV BOLUS", "INTRAVENOUS INFUSION",
                   "INTRAVASCULAR")
    result <- route_cdisc_to_pknca(iv_routes)
    expect_true(all(result == "intravascular"))
  })

  it("maps oral/other routes to extravascular", {
    ev_routes <- c("ORAL", "SUBCUTANEOUS", "INTRAMUSCULAR", "TOPICAL")
    result <- route_cdisc_to_pknca(ev_routes)
    expect_true(all(result == "extravascular"))
  })
})


describe(".prepare_dose_table", {
  # Minimal EX data for testing
  ex_mini <- data.frame(
    STUDYID = "S1",
    USUBJID = rep("S1-01", 3),
    EXTRT = "DrugA",
    EXDOSE = 5,
    EXDOSU = "mg",
    EXROUTE = "INTRAVENOUS DRIP",
    EXSTDTC = c("2024-01-15T08:00:00", "2024-01-16T08:00:00",
                "2024-01-17T08:00:00"),
    EXENDTC = c("2024-01-15T10:00:00", "2024-01-16T10:00:00",
                "2024-01-17T10:00:00"),
    EXDUR = c("PT2H", "PT2H", "PT2H"),
    EXELTM = c("PT0H", "PT24H", "PT48H"),
    stringsAsFactors = FALSE
  )

  it("produces correct number of rows", {
    result <- .prepare_dose_table(ex_mini)
    expect_equal(nrow(result), 3)
  })

  it("derives AFRLT correctly", {
    result <- .prepare_dose_table(ex_mini)
    expect_equal(result$AFRLT, c(0, 24, 48))
  })

  it("derives NFRLT from EXELTM", {
    result <- .prepare_dose_table(ex_mini)
    expect_equal(result$NFRLT, c(0, 24, 48))
  })

  it("assigns sequential DOSNOA", {
    result <- .prepare_dose_table(ex_mini)
    expect_equal(result$DOSNOA, 1:3)
  })

  it("parses ADOSEDUR from EXDUR", {
    result <- .prepare_dose_table(ex_mini)
    expect_equal(result$ADOSEDUR, c(2, 2, 2))
  })

  it("maps route to std_route", {
    result <- .prepare_dose_table(ex_mini)
    expect_true(all(result$std_route == "intravascular"))
  })

  it("renames EXTRT to DOSETRT", {
    result <- .prepare_dose_table(ex_mini)
    expect_true("DOSETRT" %in% names(result))
    expect_equal(unique(result$DOSETRT), "DrugA")
  })

  it("derives ADOSEDUR from EXENDTC when EXDUR is absent", {
    ex_no_dur <- ex_mini[, setdiff(names(ex_mini), "EXDUR")]
    result <- .prepare_dose_table(ex_no_dur)
    expect_equal(result$ADOSEDUR, c(2, 2, 2))
  })

  it("defaults ADOSEDUR to 0 when both EXDUR and EXENDTC are absent", {
    ex_no_dur <- ex_mini[, setdiff(names(ex_mini), c("EXDUR", "EXENDTC"))]
    result <- .prepare_dose_table(ex_no_dur)
    expect_equal(result$ADOSEDUR, c(0, 0, 0))
  })

  it("handles multiple subjects", {
    ex_multi <- rbind(
      ex_mini,
      data.frame(
        STUDYID = "S1", USUBJID = "S1-02", EXTRT = "DrugA",
        EXDOSE = 10, EXDOSU = "mg", EXROUTE = "ORAL",
        EXSTDTC = "2024-01-18T08:00:00", EXENDTC = NA,
        EXDUR = "PT0H", EXELTM = "PT0H",
        stringsAsFactors = FALSE
      )
    )
    result <- .prepare_dose_table(ex_multi)
    expect_equal(nrow(result), 4)
    # S1-02 should have DOSNOA = 1
    expect_equal(result$DOSNOA[result$USUBJID == "S1-02"], 1)
    # S1-02 should have AFRLT = 0
    expect_equal(result$AFRLT[result$USUBJID == "S1-02"], 0)
  })
})


describe(".check_required_cols", {
  it("passes when all columns present", {
    df <- data.frame(A = 1, B = 2, C = 3)
    expect_silent(.check_required_cols(df, c("A", "B"), "test"))
  })

  it("errors when columns are missing", {
    df <- data.frame(A = 1)
    expect_error(
      .check_required_cols(df, c("A", "B", "C"), "test"),
      "Missing required columns in test: B, C"
    )
  })
})


describe("ex_to_PKNCAdose", {
  ex_mini <- data.frame(
    STUDYID = "S1",
    USUBJID = rep("S1-01", 3),
    EXTRT = "DrugA",
    EXDOSE = c(5, 5, 5),
    EXDOSU = "mg",
    EXROUTE = "INTRAVENOUS DRIP",
    EXSTDTC = c("2024-01-15T08:00:00", "2024-01-16T08:00:00",
                "2024-01-17T08:00:00"),
    EXENDTC = c("2024-01-15T10:00:00", "2024-01-16T10:00:00",
                "2024-01-17T10:00:00"),
    EXDUR = c("PT2H", "PT2H", "PT2H"),
    EXELTM = c("PT0H", "PT24H", "PT48H"),
    stringsAsFactors = FALSE
  )

  it("returns a PKNCAdose object", {
    result <- ex_to_PKNCAdose(ex_mini)
    expect_s3_class(result, "PKNCAdose")
  })

  it("has correct formula groups", {
    result <- ex_to_PKNCAdose(ex_mini)
    groups <- dplyr::group_vars(result)
    expect_true(all(c("STUDYID", "DOSETRT", "USUBJID") %in% groups))
  })

  it("has DOSNOA in dose data", {
    result <- ex_to_PKNCAdose(ex_mini)
    expect_true("DOSNOA" %in% names(result$data))
    expect_equal(result$data$DOSNOA, 1:3)
  })

  it("has ADOSEDUR in dose data", {
    result <- ex_to_PKNCAdose(ex_mini)
    expect_true("ADOSEDUR" %in% names(result$data))
    expect_equal(result$data$ADOSEDUR, c(2, 2, 2))
  })

  it("has std_route as route column", {
    result <- ex_to_PKNCAdose(ex_mini)
    expect_equal(result$columns$route, "std_route")
    expect_true(all(result$data$std_route == "intravascular"))
  })

  it("errors on missing required columns", {
    bad_ex <- ex_mini[, c("STUDYID", "USUBJID")]
    expect_error(ex_to_PKNCAdose(bad_ex), "Missing required columns")
  })
})


describe("pc_to_PKNCAconc", {
  # Build minimal PC and EX data
  ex_mini <- data.frame(
    STUDYID = "S1",
    USUBJID = rep("S1-01", 2),
    EXTRT = "DrugA",
    EXDOSE = 5,
    EXDOSU = "mg",
    EXROUTE = "INTRAVENOUS DRIP",
    EXSTDTC = c("2024-01-15T08:00:00", "2024-01-16T08:00:00"),
    EXENDTC = c("2024-01-15T10:00:00", "2024-01-16T10:00:00"),
    EXDUR = c("PT2H", "PT2H"),
    EXELTM = c("PT0H", "PT24H"),
    stringsAsFactors = FALSE
  )

  pc_mini <- data.frame(
    STUDYID = "S1",
    USUBJID = rep("S1-01", 5),
    PCTEST = "DrugA",
    PCSPEC = "SERUM",
    PCSTRESN = c(0.1, 2.0, 1.0, 0.5, 1.5),
    PCSTRESU = "ug/mL",
    PCDTC = c(
      "2024-01-15T07:55:00",   # pre-first-dose
      "2024-01-15T09:00:00",   # 1h post dose 1
      "2024-01-15T14:00:00",   # 6h post dose 1
      "2024-01-15T20:00:00",   # 12h post dose 1
      "2024-01-16T09:00:00"    # 1h post dose 2
    ),
    PCELTM = c("PT-0.083H", "PT1H", "PT6H", "PT12H", "PT1H"),
    stringsAsFactors = FALSE
  )

  it("returns a PKNCAconc object", {
    result <- pc_to_PKNCAconc(pc_mini, ex_mini)
    expect_s3_class(result, "PKNCAconc")
  })

  it("has correct formula groups", {
    result <- pc_to_PKNCAconc(pc_mini, ex_mini)
    groups <- dplyr::group_vars(result)
    expect_true(all(c("STUDYID", "PCSPEC", "DOSETRT", "USUBJID") %in% groups))
  })

  it("derives AFRLT correctly", {
    result <- pc_to_PKNCAconc(pc_mini, ex_mini)
    afrlt <- result$data$AFRLT
    # First sample is 5 min before first dose = -5/60 hours
    expect_equal(afrlt[1], -5 / 60, tolerance = 0.001)
    # Second sample is 1h after first dose
    expect_equal(afrlt[2], 1, tolerance = 0.01)
    # Last sample is 25h after first dose
    expect_equal(afrlt[5], 25, tolerance = 0.01)
  })

  it("derives ARRLT using most recent prior dose", {
    result <- pc_to_PKNCAconc(pc_mini, ex_mini)
    arrlt <- result$data$ARRLT
    # Pre-first-dose sample: assigned to dose 1, ARRLT = PCDTC - dose1_time < 0
    expect_true(arrlt[1] < 0)
    # 1h post dose 1
    expect_equal(arrlt[2], 1, tolerance = 0.01)
    # 12h post dose 1
    expect_equal(arrlt[4], 12, tolerance = 0.01)
    # 1h post dose 2
    expect_equal(arrlt[5], 1, tolerance = 0.01)
  })

  it("assigns DOSNOA correctly", {
    result <- pc_to_PKNCAconc(pc_mini, ex_mini)
    dosnoa <- result$data$DOSNOA
    # First 4 samples assigned to dose 1, last to dose 2
    expect_equal(dosnoa, c(1, 1, 1, 1, 2))
  })

  it("derives ATPTREF from DOSNOA", {
    result <- pc_to_PKNCAconc(pc_mini, ex_mini)
    expect_equal(result$data$ATPTREF, c("DOSE 1", "DOSE 1", "DOSE 1",
                                         "DOSE 1", "DOSE 2"))
  })

  it("parses NRRLT from PCELTM", {
    result <- pc_to_PKNCAconc(pc_mini, ex_mini)
    nrrlt <- result$data$NRRLT
    expect_equal(nrrlt, c(-0.083, 1, 6, 12, 1), tolerance = 0.001)
  })

  it("derives NFRLT = NRRLT + nominal dose time", {
    result <- pc_to_PKNCAconc(pc_mini, ex_mini)
    nfrlt <- result$data$NFRLT
    # Dose 1 nominal time = 0, dose 2 nominal time = 24
    expect_equal(nfrlt, c(-0.083, 1, 6, 12, 25), tolerance = 0.001)
  })

  it("maps PCTEST to PARAM", {
    result <- pc_to_PKNCAconc(pc_mini, ex_mini)
    expect_equal(unique(result$data$PARAM), "DrugA")
  })

  it("maps PCSTRESN to AVAL", {
    result <- pc_to_PKNCAconc(pc_mini, ex_mini)
    expect_equal(result$data$AVAL, pc_mini$PCSTRESN)
  })

  it("initialises aNCA-specific columns", {
    result <- pc_to_PKNCAconc(pc_mini, ex_mini)
    d <- result$data
    expect_true(all(d$nca_exclude == ""))
    expect_true(all(d$is.excluded.hl == FALSE))
    expect_true(all(d$exclude_half.life == FALSE))
    expect_true(all(d$REASON == ""))
    expect_true(all(d$METABFL == ""))
    expect_equal(unique(d$RRLTU), "hour")
    expect_true(all(d$CONCDUR == 0))
  })

  it("sets METABFL when metabolites are specified", {
    pc_metab <- rbind(
      pc_mini,
      data.frame(
        STUDYID = "S1", USUBJID = "S1-01", PCTEST = "MetabA",
        PCSPEC = "SERUM", PCSTRESN = 0.5, PCSTRESU = "ug/mL",
        PCDTC = "2024-01-15T09:00:00", PCELTM = "PT1H",
        stringsAsFactors = FALSE
      )
    )
    result <- pc_to_PKNCAconc(pc_metab, ex_mini, metabolites = "MetabA")
    metab_rows <- result$data$PARAM == "MetabA"
    expect_true(all(result$data$METABFL[metab_rows] == "Y"))
    expect_true(all(result$data$METABFL[!metab_rows] == ""))
  })

  it("joins DM demographics when provided", {
    dm_mini <- data.frame(
      USUBJID = "S1-01",
      AGE = 30, AGEU = "Years", SEX = "M", RACE = "WHITE",
      ARM = "DrugA 5mg", ACTARM = "DrugA 5mg",
      stringsAsFactors = FALSE
    )
    result <- pc_to_PKNCAconc(pc_mini, ex_mini, dm = dm_mini)
    expect_true("AGE" %in% names(result$data))
    expect_true("SEX" %in% names(result$data))
    expect_equal(unique(result$data$AGE), 30)
  })

  it("works without PCELTM (falls back to ARRLT for NRRLT)", {
    pc_no_eltm <- pc_mini[, setdiff(names(pc_mini), "PCELTM")]
    result <- pc_to_PKNCAconc(pc_no_eltm, ex_mini)
    expect_equal(result$data$NRRLT, result$data$ARRLT)
  })

  it("defaults PCSPEC to UNKNOWN when absent", {
    pc_no_spec <- pc_mini[, setdiff(names(pc_mini), "PCSPEC")]
    # Add PCSPEC as required by the formula but test the default
    result <- pc_to_PKNCAconc(pc_no_spec, ex_mini)
    expect_equal(unique(result$data$PCSPEC), "UNKNOWN")
  })

  it("errors on missing required PC columns", {
    bad_pc <- pc_mini[, c("STUDYID", "USUBJID")]
    expect_error(pc_to_PKNCAconc(bad_pc, ex_mini), "Missing required columns")
  })

  it("errors on missing required EX columns", {
    bad_ex <- ex_mini[, c("STUDYID", "USUBJID")]
    expect_error(pc_to_PKNCAconc(pc_mini, bad_ex), "Missing required columns")
  })
})


describe("sdtm_to_PKNCAdata", {
  ex_mini <- data.frame(
    STUDYID = "S1",
    USUBJID = rep("S1-01", 2),
    EXTRT = "DrugA",
    EXDOSE = 5,
    EXDOSU = "mg",
    EXROUTE = "INTRAVENOUS DRIP",
    EXSTDTC = c("2024-01-15T08:00:00", "2024-01-16T08:00:00"),
    EXENDTC = c("2024-01-15T10:00:00", "2024-01-16T10:00:00"),
    EXDUR = c("PT2H", "PT2H"),
    EXELTM = c("PT0H", "PT24H"),
    stringsAsFactors = FALSE
  )

  pc_mini <- data.frame(
    STUDYID = "S1",
    USUBJID = rep("S1-01", 4),
    PCTEST = "DrugA",
    PCSPEC = "SERUM",
    PCSTRESN = c(2.0, 1.0, 0.5, 1.5),
    PCSTRESU = "ug/mL",
    PCDTC = c(
      "2024-01-15T09:00:00",
      "2024-01-15T14:00:00",
      "2024-01-15T20:00:00",
      "2024-01-16T09:00:00"
    ),
    PCELTM = c("PT1H", "PT6H", "PT12H", "PT1H"),
    stringsAsFactors = FALSE
  )

  it("returns a PKNCAdata object", {
    result <- sdtm_to_PKNCAdata(pc_mini, ex_mini)
    expect_s3_class(result, "PKNCAdata")
  })

  it("has both conc and dose components", {
    result <- sdtm_to_PKNCAdata(pc_mini, ex_mini)
    expect_s3_class(result$conc, "PKNCAconc")
    expect_s3_class(result$dose, "PKNCAdose")
  })

  it("has a units table", {
    result <- sdtm_to_PKNCAdata(pc_mini, ex_mini)
    expect_true(!is.null(result$units))
    expect_true(nrow(result$units) > 0)
  })

  it("has intervals", {
    result <- sdtm_to_PKNCAdata(pc_mini, ex_mini)
    expect_true(nrow(result$intervals) > 0)
  })

  it("conc data has all columns needed by downstream pipeline", {
    result <- sdtm_to_PKNCAdata(pc_mini, ex_mini)
    required_cols <- c(
      "AVAL", "AFRLT", "ARRLT", "NFRLT", "NRRLT",
      "PARAM", "PCSPEC", "DOSETRT", "DOSEA", "DOSEU",
      "ROUTE", "std_route", "ADOSEDUR", "DOSNOA", "ATPTREF",
      "AVALU", "RRLTU", "CONCDUR", "METABFL",
      "nca_exclude", "is.excluded.hl", "is.included.hl",
      "exclude_half.life", "REASON"
    )
    missing <- setdiff(required_cols, names(result$conc$data))
    expect_equal(length(missing), 0,
                 info = paste("Missing columns:", paste(missing, collapse = ", ")))
  })

  it("dose data has all columns needed by downstream pipeline", {
    result <- sdtm_to_PKNCAdata(pc_mini, ex_mini)
    required_cols <- c(
      "DOSEA", "AFRLT", "NFRLT", "DOSNOA", "ADOSEDUR",
      "DOSEU", "std_route", "STUDYID", "DOSETRT", "USUBJID"
    )
    missing <- setdiff(required_cols, names(result$dose$data))
    expect_equal(length(missing), 0,
                 info = paste("Missing columns:", paste(missing, collapse = ", ")))
  })

  it("passes DM and metabolites through", {
    dm_mini <- data.frame(
      USUBJID = "S1-01", AGE = 30, AGEU = "Years",
      SEX = "M", RACE = "WHITE",
      stringsAsFactors = FALSE
    )
    result <- sdtm_to_PKNCAdata(pc_mini, ex_mini, dm = dm_mini)
    expect_true("AGE" %in% names(result$conc$data))
  })
})


describe("sdtm_to_PKNCAdata with example datasets", {
  skip_if_not(
    file.exists(system.file("data", "pc_example.rda", package = "aNCA")),
    message = "SDTM example .rda files not yet generated"
  )

  data("pc_example", package = "aNCA", envir = environment())
  data("ex_example", package = "aNCA", envir = environment())
  data("dm_example", package = "aNCA", envir = environment())

  it("works end-to-end with the package example datasets", {
    result <- sdtm_to_PKNCAdata(pc_example, ex_example, dm = dm_example)
    expect_s3_class(result, "PKNCAdata")
    expect_true(nrow(result$conc$data) > 0)
    expect_true(nrow(result$dose$data) > 0)
  })

  it("produces correct number of concentration rows", {
    result <- sdtm_to_PKNCAdata(pc_example, ex_example, dm = dm_example)
    expect_equal(nrow(result$conc$data), nrow(pc_example))
  })

  it("has demographics from DM in conc data", {
    result <- sdtm_to_PKNCAdata(pc_example, ex_example, dm = dm_example)
    expect_true("AGE" %in% names(result$conc$data))
    expect_true("SEX" %in% names(result$conc$data))
    expect_true("RACE" %in% names(result$conc$data))
  })
})


describe("NCA round-trip: SDTM vs ADNCA produce equivalent results", {
  skip_if_not(
    file.exists(system.file("data", "pc_example.rda", package = "aNCA")),
    message = "SDTM example .rda files not yet generated"
  )

  data("adnca_example", package = "aNCA", envir = environment())
  data("pc_example", package = "aNCA", envir = environment())
  data("ex_example", package = "aNCA", envir = environment())
  data("dm_example", package = "aNCA", envir = environment())


  # Identify metabolites from the ADNCA data so the SDTM path can set METABFL
  metabolite_params <- unique(
    adnca_example$PARAM[adnca_example$METABFL == "Y"]
  )

  # Build both PKNCAdata objects
  pknca_adnca <- PKNCA_create_data_object(adnca_example)
  pknca_sdtm  <- sdtm_to_PKNCAdata(
    pc_example, ex_example, dm_example,
    metabolites = metabolite_params
  )

  it("produces identical formula groups", {
    expect_equal(
      dplyr::group_vars(pknca_sdtm$conc),
      dplyr::group_vars(pknca_adnca$conc)
    )
    expect_equal(
      dplyr::group_vars(pknca_sdtm$dose),
      dplyr::group_vars(pknca_adnca$dose)
    )
  })

  it("produces same number of concentration rows", {
    expect_equal(nrow(pknca_sdtm$conc$data), nrow(pknca_adnca$conc$data))
  })

  it("produces identical AVAL values", {
    # Sort by a stable key to avoid tie-breaking issues
    adnca_sorted <- pknca_adnca$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)
    sdtm_sorted <- pknca_sdtm$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)
    # NA-safe comparison
    expect_true(
      all(adnca_sorted$AVAL == sdtm_sorted$AVAL, na.rm = TRUE) &&
        all(is.na(adnca_sorted$AVAL) == is.na(sdtm_sorted$AVAL))
    )
  })

  it("produces AFRLT within 1 second of ADNCA", {
    adnca_sorted <- pknca_adnca$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)
    sdtm_sorted <- pknca_sdtm$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)
    max_diff_hours <- max(abs(adnca_sorted$AFRLT - sdtm_sorted$AFRLT))
    # 1 second = 1/3600 hours
    expect_lt(max_diff_hours, 1 / 3600)
  })

  # --- PKNCA object structure ---------------------------------------------------

  it("produces identical units tables", {
    expect_equal(pknca_sdtm$units, pknca_adnca$units)
  })

  it("produces identical conc and dose formulas", {
    expect_equal(
      deparse(pknca_sdtm$conc$formula),
      deparse(pknca_adnca$conc$formula)
    )
    expect_equal(
      deparse(pknca_sdtm$dose$formula),
      deparse(pknca_adnca$dose$formula)
    )
  })

  it("produces identical PKNCA column mappings", {
    expect_equal(pknca_sdtm$conc$columns, pknca_adnca$conc$columns)
    expect_equal(pknca_sdtm$dose$columns, pknca_adnca$dose$columns)
  })

  # --- Conc data: all shared columns that should be identical -----------------
  # Columns that differ by design (pre-dose reference assignment) are tested
  # separately below. Columns related to exclusion settings (NCA1XRS, NCA2XRS)
  # are absent from SDTM and not compared.

  it("produces identical conc values for PKNCA-mapped columns", {
    # These are the columns referenced by PKNCAconc$columns — the ones PKNCA
    # actually uses for computation. They must match exactly.
    adnca_sorted <- pknca_adnca$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)
    sdtm_sorted <- pknca_sdtm$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)

    # concentration (AVAL) — already tested above, but included for completeness
    expect_equal(sdtm_sorted$AVAL, adnca_sorted$AVAL, label = "AVAL")
    # volume (VOLUME)
    expect_equal(sdtm_sorted$VOLUME, adnca_sorted$VOLUME, label = "VOLUME")
    # time.nominal (NFRLT) differs for pre-dose samples; skip here
    # duration (CONCDUR) differs for urine; skip here
    # exclude (nca_exclude) — initialized column
    expect_equal(sdtm_sorted$nca_exclude, adnca_sorted$nca_exclude,
                 label = "nca_exclude")
    # exclude_half.life, include_half.life — initialized columns
    expect_equal(sdtm_sorted$exclude_half.life,
                 adnca_sorted$exclude_half.life,
                 label = "exclude_half.life")
    expect_equal(sdtm_sorted$include_half.life,
                 adnca_sorted$include_half.life,
                 label = "include_half.life")
    # unit columns (AVALU, RRLTU)
    expect_equal(sdtm_sorted$AVALU, adnca_sorted$AVALU, label = "AVALU")
    expect_equal(sdtm_sorted$RRLTU, adnca_sorted$RRLTU, label = "RRLTU")
  })

  it("produces identical conc grouping and demographic columns", {
    adnca_sorted <- pknca_adnca$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)
    sdtm_sorted <- pknca_sdtm$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)

    # Grouping columns from the formula
    for (col in c("STUDYID", "PCSPEC", "DOSETRT", "USUBJID", "PARAM")) {
      expect_equal(sdtm_sorted[[col]], adnca_sorted[[col]],
                   label = paste("conc", col))
    }
    # Dosing columns carried into conc
    expect_equal(sdtm_sorted$DOSEA, adnca_sorted$DOSEA, label = "DOSEA")
    expect_equal(sdtm_sorted$DOSEU, adnca_sorted$DOSEU, label = "DOSEU")
    expect_equal(sdtm_sorted$ROUTE, adnca_sorted$ROUTE, label = "ROUTE")
    expect_equal(sdtm_sorted$std_route, adnca_sorted$std_route,
                 label = "std_route")
    # Demographics
    for (col in c("AGE", "AGEU", "RACE", "SEX")) {
      expect_equal(sdtm_sorted[[col]], adnca_sorted[[col]],
                   label = paste("conc", col))
    }
    # Other shared columns
    expect_equal(sdtm_sorted$VOLUMEU, adnca_sorted$VOLUMEU, label = "VOLUMEU")
    expect_equal(sdtm_sorted$REASON, adnca_sorted$REASON, label = "REASON")
    expect_equal(sdtm_sorted$is.excluded.hl, adnca_sorted$is.excluded.hl,
                 label = "is.excluded.hl")
    expect_equal(sdtm_sorted$is.included.hl, adnca_sorted$is.included.hl,
                 label = "is.included.hl")
  })

  it("produces identical NRRLT in conc data", {
    # NRRLT (nominal relative time to reference dose) should match because
    # both paths derive it from the same source (PCELTM / NRRLT column).
    adnca_sorted <- pknca_adnca$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)
    sdtm_sorted <- pknca_sdtm$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)
    expect_equal(sdtm_sorted$NRRLT, adnca_sorted$NRRLT, label = "NRRLT")
  })

  # --- Dose data: all shared columns -----------------------------------------

  it("produces same number of dose rows", {
    expect_equal(nrow(pknca_sdtm$dose$data), nrow(pknca_adnca$dose$data))
  })

  it("produces identical dose data for all shared columns", {
    adnca_dose <- pknca_adnca$dose$data %>%
      dplyr::arrange(STUDYID, USUBJID, DOSETRT, AFRLT)
    sdtm_dose <- pknca_sdtm$dose$data %>%
      dplyr::arrange(STUDYID, USUBJID, DOSETRT, AFRLT)

    # PKNCA-mapped dose columns
    expect_equal(sdtm_dose$DOSEA, adnca_dose$DOSEA, label = "dose DOSEA")
    expect_equal(sdtm_dose$std_route, adnca_dose$std_route,
                 label = "dose std_route")
    expect_equal(sdtm_dose$ADOSEDUR, adnca_dose$ADOSEDUR,
                 label = "dose ADOSEDUR")
    expect_equal(sdtm_dose$DOSNOA, adnca_dose$DOSNOA, label = "dose DOSNOA")
    expect_equal(sdtm_dose$DOSEU, adnca_dose$DOSEU, label = "dose DOSEU")

    # Grouping columns
    expect_equal(sdtm_dose$STUDYID, adnca_dose$STUDYID, label = "dose STUDYID")
    expect_equal(sdtm_dose$USUBJID, adnca_dose$USUBJID, label = "dose USUBJID")
    expect_equal(sdtm_dose$DOSETRT, adnca_dose$DOSETRT, label = "dose DOSETRT")
    expect_equal(sdtm_dose$ROUTE, adnca_dose$ROUTE, label = "dose ROUTE")

    # AFRLT within 1 second (datetime precision)
    max_dose_afrlt_diff <- max(abs(adnca_dose$AFRLT - sdtm_dose$AFRLT))
    expect_lt(max_dose_afrlt_diff, 1 / 3600, label = "dose AFRLT")
  })

  # --- Pre-dose normalization -------------------------------------------------
  # ADNCA assigns pre-dose samples (NRRLT < 0) to the upcoming dose, while
  # SDTM assigns them to the most recent previous dose. After normalizing both
  # to the same convention (most recent previous dose), ARRLT, NFRLT, DOSNOA,
  # ATPTREF, and ADOSEDUR should all match.

  it("produces matching ARRLT after normalizing pre-dose reference", {
    adnca_sorted <- pknca_adnca$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)
    sdtm_sorted <- pknca_sdtm$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)

    # Build dose time lookup from dose data
    dose_times <- pknca_adnca$dose$data %>%
      dplyr::arrange(STUDYID, USUBJID, DOSETRT, AFRLT) %>%
      dplyr::group_by(STUDYID, USUBJID, DOSETRT) %>%
      dplyr::mutate(dn = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::select(USUBJID, dn, dose_afrlt = AFRLT)

    # Normalize ADNCA: shift pre-dose from upcoming to previous dose
    adnca_norm <- adnca_sorted %>%
      dplyr::mutate(
        norm_dosnoa = ifelse(NRRLT < 0 & DOSNOA > 1, DOSNOA - 1L, DOSNOA)
      ) %>%
      dplyr::left_join(dose_times, by = c("USUBJID", "norm_dosnoa" = "dn")) %>%
      dplyr::mutate(norm_arrlt = AFRLT - dose_afrlt)

    # SDTM: already references previous dose
    sdtm_norm <- sdtm_sorted %>%
      dplyr::mutate(norm_dosnoa = DOSNOA) %>%
      dplyr::left_join(dose_times, by = c("USUBJID", "norm_dosnoa" = "dn")) %>%
      dplyr::mutate(norm_arrlt = AFRLT - dose_afrlt)

    # Normalized DOSNOA should be identical
    expect_equal(adnca_norm$norm_dosnoa, sdtm_norm$norm_dosnoa,
                 label = "normalized DOSNOA")

    # Normalized ARRLT within 1 second
    max_diff <- max(abs(adnca_norm$norm_arrlt - sdtm_norm$norm_arrlt),
                    na.rm = TRUE)
    expect_lt(max_diff, 1 / 3600, label = "normalized ARRLT")

    # Normalized NFRLT (= TRTRINT * (norm_dosnoa - 1) + NRRLT) should match
    trtrint <- if ("TRTRINT" %in% names(adnca_sorted)) {
      adnca_sorted$TRTRINT[1]
    } else {
      24
    }
    adnca_nfrlt <- trtrint * (adnca_norm$norm_dosnoa - 1) + adnca_sorted$NRRLT
    sdtm_nfrlt  <- trtrint * (sdtm_norm$norm_dosnoa - 1) + sdtm_sorted$NRRLT
    expect_equal(adnca_nfrlt, sdtm_nfrlt, label = "normalized NFRLT")
  })

  it("produces matching ADOSEDUR after normalizing pre-dose reference", {
    adnca_sorted <- pknca_adnca$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)
    sdtm_sorted <- pknca_sdtm$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)

    # Dose info lookup
    dose_info <- pknca_adnca$dose$data %>%
      dplyr::arrange(STUDYID, USUBJID, DOSETRT, AFRLT) %>%
      dplyr::group_by(STUDYID, USUBJID, DOSETRT) %>%
      dplyr::mutate(dn = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::select(USUBJID, dn, dose_adosedur = ADOSEDUR)

    adnca_norm <- adnca_sorted %>%
      dplyr::mutate(
        norm_dosnoa = ifelse(NRRLT < 0 & DOSNOA > 1, DOSNOA - 1L, DOSNOA)
      ) %>%
      dplyr::left_join(dose_info, by = c("USUBJID", "norm_dosnoa" = "dn"))

    sdtm_norm <- sdtm_sorted %>%
      dplyr::mutate(norm_dosnoa = DOSNOA) %>%
      dplyr::left_join(dose_info, by = c("USUBJID", "norm_dosnoa" = "dn"))

    expect_equal(adnca_norm$dose_adosedur, sdtm_norm$dose_adosedur,
                 label = "normalized ADOSEDUR")
  })

  # --- CONCDUR and METABFL ---------------------------------------------------

  it("produces identical CONCDUR for serum samples", {
    adnca_sorted <- pknca_adnca$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)
    sdtm_sorted <- pknca_sdtm$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)

    serum_idx <- adnca_sorted$PCSPEC != "URINE"
    expect_equal(sdtm_sorted$CONCDUR[serum_idx],
                 adnca_sorted$CONCDUR[serum_idx],
                 label = "CONCDUR (serum)")
  })

  it("produces identical METABFL when metabolites are specified", {
    adnca_sorted <- pknca_adnca$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)
    sdtm_sorted <- pknca_sdtm$conc$data %>%
      dplyr::arrange(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT)

    expect_equal(sdtm_sorted$METABFL, adnca_sorted$METABFL,
                 label = "METABFL")
  })

  it("produces identical intervals", {
    adnca_int <- format_pkncadata_intervals(pknca_adnca$conc, pknca_adnca$dose)
    sdtm_int  <- format_pkncadata_intervals(pknca_sdtm$conc, pknca_sdtm$dose)

    key_cols <- c("PARAM", "PCSPEC", "DOSETRT", "USUBJID", "ATPTREF")
    adnca_key <- adnca_int %>%
      dplyr::select(dplyr::any_of(c("start", "end", key_cols))) %>%
      dplyr::arrange(dplyr::across(dplyr::everything()))
    sdtm_key <- sdtm_int %>%
      dplyr::select(dplyr::any_of(c("start", "end", key_cols))) %>%
      dplyr::arrange(dplyr::across(dplyr::everything()))

    expect_equal(nrow(adnca_key), nrow(sdtm_key))
    # Start/end should match within 1 second
    expect_true(all(abs(adnca_key$start - sdtm_key$start) < 1 / 3600))
    expect_true(all(abs(adnca_key$end - sdtm_key$end) < 1 / 3600))
  })

  it("produces NCA results within 1% of ADNCA path", {
    nca_params <- c("cmax", "tmax", "half.life", "lambda.z")

    # Set up intervals
    adnca_int <- format_pkncadata_intervals(pknca_adnca$conc, pknca_adnca$dose)
    sdtm_int  <- format_pkncadata_intervals(pknca_sdtm$conc, pknca_sdtm$dose)
    for (p in nca_params) {
      if (p %in% names(adnca_int)) adnca_int[[p]] <- TRUE
      if (p %in% names(sdtm_int))  sdtm_int[[p]]  <- TRUE
    }
    pknca_adnca$intervals <- adnca_int
    pknca_sdtm$intervals  <- sdtm_int
    pknca_adnca$options <- list(progress = FALSE, allow_partial_missing_units = TRUE)
    pknca_sdtm$options  <- list(progress = FALSE, allow_partial_missing_units = TRUE)

    results_adnca <- PKNCA::pk.nca(pknca_adnca)
    results_sdtm  <- PKNCA::pk.nca(pknca_sdtm)

    # Join by full key
    res_a <- as.data.frame(results_adnca$result) %>%
      dplyr::filter(PPTESTCD %in% nca_params) %>%
      dplyr::select(STUDYID, USUBJID, PARAM, PCSPEC, DOSETRT,
                    PPTESTCD, PPSTRES, start, end)
    res_s <- as.data.frame(results_sdtm$result) %>%
      dplyr::filter(PPTESTCD %in% nca_params) %>%
      dplyr::select(STUDYID, USUBJID, PARAM, PCSPEC, DOSETRT,
                    PPTESTCD, PPSTRES, start, end)

    comp <- dplyr::inner_join(
      res_a, res_s,
      by = c("STUDYID", "USUBJID", "PARAM", "PCSPEC", "DOSETRT",
             "PPTESTCD", "start", "end"),
      suffix = c("_a", "_s")
    )

    expect_true(nrow(comp) > 0, info = "Should have matched results")

    # For numeric comparisons, check < 1% relative difference
    numeric_comp <- comp %>%
      dplyr::filter(!is.na(PPSTRES_a) & !is.na(PPSTRES_s) & PPSTRES_a != 0)

    if (nrow(numeric_comp) > 0) {
      rel_diffs <- abs(numeric_comp$PPSTRES_a - numeric_comp$PPSTRES_s) /
        abs(numeric_comp$PPSTRES_a) * 100
      expect_true(
        all(rel_diffs < 1),
        info = paste("Max relative diff:", max(rel_diffs), "%")
      )
    }

    # NA consistency: if one is NA, the other should be too
    na_comp <- comp %>%
      dplyr::filter(is.na(PPSTRES_a) | is.na(PPSTRES_s))
    if (nrow(na_comp) > 0) {
      expect_true(
        all(is.na(na_comp$PPSTRES_a) & is.na(na_comp$PPSTRES_s)),
        info = "NA results should be consistent between paths"
      )
    }
  })
})
