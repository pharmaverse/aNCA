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

  describe("pc_example timing consistency with adnca_example", {
    it("preserves inter-sample time intervals per subject", {
      # AFRLT differences between consecutive samples should match
      # PCDTC differences (in hours) for each subject+analyte
      for (subj in unique(adnca_example$USUBJID)) {
        for (param in unique(adnca_example$PARAM)) {
          idx <- adnca_example$USUBJID == subj & adnca_example$PARAM == param
          if (sum(idx) < 2) next
          adnca_afrlt <- sort(adnca_example$AFRLT[idx])
          adnca_diffs <- diff(adnca_afrlt)

          pcdtc <- as.POSIXct(pc_example$PCDTC[idx],
                              format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
          pcdtc_sorted <- sort(pcdtc)
          pcdtc_diffs <- as.numeric(diff(pcdtc_sorted), units = "hours")

          expect_equal(pcdtc_diffs, adnca_diffs, tolerance = 1 / 3600,
                       label = paste(subj, param, "time intervals"))
        }
      }
    })

    it("preserves dose amounts from adnca_example", {
      # Each unique (USUBJID, DOSETRT, ATPTREF) in adnca has a DOSEA.
      # The corresponding EX record should have the same EXDOSE.
      adnca_doses <- unique(adnca_example[, c("USUBJID", "DOSETRT", "DOSEA")])
      ex_doses <- unique(ex_example[, c("USUBJID", "EXTRT", "EXDOSE")])
      names(ex_doses) <- c("USUBJID", "DOSETRT", "DOSEA")
      # Every dose in ADNCA should appear in EX
      merged <- merge(adnca_doses, ex_doses, by = c("USUBJID", "DOSETRT", "DOSEA"))
      expect_equal(nrow(merged), nrow(adnca_doses))
    })

    it("preserves dose durations from adnca_example", {
      # ADOSEDUR varies per dose event. Compare the set of unique durations.
      adnca_dur_set <- sort(unique(adnca_example$ADOSEDUR))
      ex_dur_parsed <- sort(unique(parse_iso8601_duration(ex_example$EXDUR)))
      for (d in adnca_dur_set) {
        expect_true(
          any(abs(ex_dur_parsed - d) < 1e-3),
          label = paste("ADOSEDUR", d, "found in EX")
        )
      }
    })

    it("preserves inter-dose time intervals per subject", {
      # EX dose times (from EXSTDTC) should match the unique dose times
      # derived from ADNCA. Use ATPTREF to get one dose time per dose
      # period, taking the minimum AFRLT - ARRLT (dose start, not end).
      for (subj in unique(ex_example$USUBJID)) {
        ex_subj <- ex_example[ex_example$USUBJID == subj, ]
        ex_times <- sort(as.POSIXct(ex_subj$EXSTDTC,
                                     format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
        if (length(ex_times) < 2) next
        ex_diffs <- as.numeric(diff(ex_times), units = "hours")

        adnca_subj <- adnca_example[adnca_example$USUBJID == subj &
                                      adnca_example$ARRLT >= 0, ]
        dose_times <- tapply(
          adnca_subj$AFRLT - adnca_subj$ARRLT,
          adnca_subj$ATPTREF,
          min
        )
        dose_times <- sort(unname(dose_times))
        if (length(dose_times) < 2) next
        adnca_diffs <- diff(dose_times)

        expect_equal(ex_diffs, adnca_diffs, tolerance = 0.005,
                     label = paste(subj, "inter-dose intervals"))
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
