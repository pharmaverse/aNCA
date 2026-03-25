describe("SDTM example datasets", {

  # Skip all tests if the .rda files have not been generated yet
  skip_if_not(
    file.exists(system.file("data", "dm_example.rda", package = "aNCA")),
    message = "SDTM example .rda files not yet generated — run data-raw/sdtm_example.R first"
  )

  data("adnca_example", package = "aNCA", envir = environment())
  data("dm_example", package = "aNCA", envir = environment())
  data("pc_example", package = "aNCA", envir = environment())
  data("ex_example", package = "aNCA", envir = environment())

  describe("dm_example", {
    it("has one row per unique subject in adnca_example", {
      expect_equal(nrow(dm_example), length(unique(adnca_example$USUBJID)))
    })

    it("contains all expected columns", {
      expected_cols <- c(
        "STUDYID", "USUBJID", "AGE", "AGEU", "SEX", "RACE",
        "ARM", "ACTARM", "RFXSTDTC"
      )
      expect_true(all(expected_cols %in% names(dm_example)))
    })

    it("has the same subjects as adnca_example", {
      expect_setequal(dm_example$USUBJID, unique(adnca_example$USUBJID))
    })

    it("has unique RFXSTDTC per subject", {
      expect_equal(length(unique(dm_example$RFXSTDTC)), nrow(dm_example))
    })

    it("has valid ISO 8601 datetimes in RFXSTDTC", {
      parsed <- as.POSIXct(dm_example$RFXSTDTC, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      expect_false(any(is.na(parsed)))
    })
  })

  describe("pc_example", {
    it("has the same number of rows as adnca_example", {
      expect_equal(nrow(pc_example), nrow(adnca_example))
    })

    it("contains all expected columns", {
      expected_cols <- c(
        "STUDYID", "USUBJID", "PCTEST", "PCSPEC", "PCSTRESN", "PCSTRESU",
        "PCDTC", "PCRFTDTC", "PCELTM", "VOLUME", "VOLUMEU"
      )
      expect_true(all(expected_cols %in% names(pc_example)))
    })

    it("has concentration values matching adnca_example$AVAL", {
      expect_equal(pc_example$PCSTRESN, adnca_example$AVAL)
    })

    it("has analyte names matching adnca_example$PARAM", {
      expect_equal(pc_example$PCTEST, adnca_example$PARAM)
    })

    it("has valid ISO 8601 datetimes in PCDTC", {
      parsed <- as.POSIXct(pc_example$PCDTC, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      expect_false(any(is.na(parsed)))
    })

    it("has subject-specific reference dates from dm_example", {
      pc_subjects <- unique(pc_example$USUBJID)
      for (subj in pc_subjects) {
        pc_rft <- unique(pc_example$PCRFTDTC[pc_example$USUBJID == subj])
        dm_rft <- dm_example$RFXSTDTC[dm_example$USUBJID == subj]
        # The earliest PCRFTDTC for a subject should match their RFXSTDTC
        expect_equal(min(pc_rft), dm_rft)
      }
    })
  })

  describe("ex_example", {
    it("has fewer rows than adnca_example (deduplicated to dose events)", {
      expect_lt(nrow(ex_example), nrow(adnca_example))
    })

    it("contains all expected columns", {
      expected_cols <- c(
        "STUDYID", "USUBJID", "EXTRT", "EXDOSE", "EXDOSU",
        "EXROUTE", "EXSTDTC", "EXENDTC", "EXDUR", "EXELTM"
      )
      expect_true(all(expected_cols %in% names(ex_example)))
    })

    it("has the same subjects as adnca_example", {
      expect_setequal(unique(ex_example$USUBJID), unique(adnca_example$USUBJID))
    })

    it("has valid ISO 8601 datetimes in EXSTDTC", {
      parsed <- as.POSIXct(ex_example$EXSTDTC, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      expect_false(any(is.na(parsed)))
    })

    it("has EXENDTC >= EXSTDTC for all rows", {
      start <- as.POSIXct(ex_example$EXSTDTC, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      end   <- as.POSIXct(ex_example$EXENDTC, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      expect_false(any(is.na(end)))
      expect_true(all(end >= start))
    })

    it("has EXELTM starting at PT0H for each subject's first dose", {
      first_eltm <- tapply(ex_example$EXELTM, ex_example$USUBJID, function(x) x[1])
      expect_true(all(first_eltm == "PT0H"))
    })

    it("has no duplicate dose events", {
      keys <- paste(ex_example$STUDYID, ex_example$USUBJID,
                    ex_example$EXTRT, ex_example$EXSTDTC, sep = "|")
      expect_equal(length(keys), length(unique(keys)))
    })

    it("has treatment names matching adnca_example$DOSETRT", {
      expect_setequal(unique(ex_example$EXTRT), unique(adnca_example$DOSETRT))
    })
  })
})
