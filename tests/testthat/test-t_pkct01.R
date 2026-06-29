# Shared fixture: minimal ADNCA-like data frame
pkct01_data <- data.frame(
  PARAM   = rep(c("Drug A", "Drug B"), each = 12),
  PCSPEC  = rep("PLASMA", 24),
  TRT01A  = rep(rep(c("10mg", "50mg"), each = 6), 2),
  DOSEA   = rep(rep(c(10, 50), each = 6), 2),
  ATPTREF = rep("Day 1", 24),
  NFRLT   = rep(rep(c(0, 1, 2), each = 2), 4),
  NRRLT   = rep(rep(c(0, 1, 2), each = 2), 4),
  AVAL    = c(0, 0, 5, 6, 3, 4,   # Drug A / 10mg
              0, 0, 10, 11, 7, 8,  # Drug A / 50mg
              0, 0, 2, 3, 1, 2,    # Drug B / 10mg
              0, 0, 4, 5, 3, 3),   # Drug B / 50mg
  AVALC   = c(rep("BLQ", 2), "5", "6", "3", "4",
              rep("BLQ", 2), "10", "11", "7", "8",
              rep("BLQ", 2), "2", "3", "1", "2",
              rep("BLQ", 2), "4", "5", "3", "3"),
  stringsAsFactors = FALSE
)

describe("t_pkct01", {
  it("returns a named list with one entry per PARAM/PCSPEC combination", {
    result <- t_pkct01(pkct01_data)
    expect_type(result, "list")
    expect_equal(length(result), 2)
    expect_true(all(grepl("PLASMA", names(result))))
  })

  it("each element is a data frame", {
    result <- t_pkct01(pkct01_data)
    purrr::walk(result, ~ expect_s3_class(.x, "data.frame"))
  })

  it("output contains expected statistic columns", {
    result <- t_pkct01(pkct01_data)[[1]]
    expected_cols <- c("TRT01A", "ATPTREF", "NFRLT",
                       "n", "n_blq", "Mean", "SD", "CV_pct",
                       "Median", "GeoMean", "Min", "Max")
    expect_true(all(expected_cols %in% names(result)))
  })

  it("BLQ rows produce NA for numeric stats and non-zero n_blq", {
    result <- t_pkct01(pkct01_data)[[1]]
    blq_rows <- result[result$NFRLT == 0, ]
    expect_true(all(is.na(blq_rows$Mean)))
    expect_true(all(blq_rows$n_blq > 0))
  })

  it("non-BLQ rows have correct n and numeric stats", {
    result <- t_pkct01(pkct01_data)[[1]]
    row_t1 <- result[result$TRT01A == "10mg" & result$NFRLT == 1, ]
    expect_equal(row_t1$n, 2)
    expect_equal(row_t1$n_blq, 0)
    expect_equal(row_t1$Mean, round(mean(c(5, 6)), 3))
    expect_equal(row_t1$Min, 5)
    expect_equal(row_t1$Max, 6)
  })

  it("stops with informative error when required columns are missing", {
    bad <- pkct01_data[, setdiff(names(pkct01_data), "AVAL")]
    expect_error(t_pkct01(bad), "missing required columns")
  })

  it("returns single list entry when list_vars not present in data", {
    data_no_pcspec <- pkct01_data[, setdiff(names(pkct01_data), "PCSPEC")]
    result <- t_pkct01(data_no_pcspec, list_vars = c("PARAM", "PCSPEC"))
    expect_equal(length(result), 2)  # PARAM still present
  })

  it("falls back to AVAL==0 for BLQ detection when blq_var column is absent", {
    data_no_avalc <- pkct01_data[, setdiff(names(pkct01_data), "AVALC")]
    result <- t_pkct01(data_no_avalc)[[1]]
    blq_rows <- result[result$NFRLT == 0, ]
    # AVAL==0 rows should still be counted as BLQ and excluded from stats
    expect_true(all(is.na(blq_rows$Mean)))
    expect_true(all(blq_rows$n_blq > 0))
  })

  it("AVAL==0 fallback: n_blq matches count of zero-AVAL rows per group", {
    data_no_avalc <- pkct01_data[, setdiff(names(pkct01_data), "AVALC")]
    result <- t_pkct01(data_no_avalc)[[1]]
    row <- result[result$TRT01A == "10mg" & result$NFRLT == 0, ]
    # 10mg arm at NFRLT=0 has 2 rows with AVAL=0
    expect_equal(row$n_blq, 2L)
    expect_equal(row$n, 2L)
  })

  it("orders rows by stratum then by numeric (not lexical) time", {
    # Two arms; NFRLT includes 10 to catch a lexical sort that would place
    # "10" before "2".  Input order deliberately interleaves arms/times.
    d <- data.frame(
      PARAM   = "Drug A",
      PCSPEC  = "PLASMA",
      TRT01A  = rep(c("B_arm", "A_arm"), each = 4),
      ATPTREF = "Day 1",
      NFRLT   = rep(c(2, 10, 2, 10), 2),
      AVAL    = 1:8,
      AVALC   = as.character(1:8),
      stringsAsFactors = FALSE
    )
    result <- t_pkct01(d)[[1]]
    # Each arm's rows are contiguous: an arm value never reappears after a switch
    expect_equal(anyDuplicated(rle(as.character(result$TRT01A))$values), 0L)
    # Within each arm, time is ascending and numeric (2 before 10)
    purrr::walk(split(result$NFRLT, result$TRT01A), ~ expect_false(is.unsorted(.x)))
  })

  it("orders treatment arms and visits naturally (10 mg before 100, DOSE 2 before DOSE 10)", {
    d <- data.frame(
      PARAM   = "Drug A",
      PCSPEC  = "PLASMA",
      TRT01A  = rep(c("100 mg", "10 mg"), each = 4),
      ATPTREF = rep(c("DOSE 10", "DOSE 2"), times = 4),
      NFRLT   = 1,
      AVAL    = 1:8,
      AVALC   = as.character(1:8),
      stringsAsFactors = FALSE
    )
    result <- t_pkct01(d)[[1]]
    # Arms natural-sorted: every "10 mg" row precedes every "100 mg" row
    expect_true(max(which(result$TRT01A == "10 mg")) <
                  min(which(result$TRT01A == "100 mg")))
    # Within an arm, "DOSE 2" precedes "DOSE 10"
    arm <- result[result$TRT01A == "10 mg", ]
    expect_equal(arm$ATPTREF, c("DOSE 2", "DOSE 10"))
  })

  it("attaches readable labels to statistic columns", {
    result <- t_pkct01(pkct01_data)[[1]]
    expect_equal(attr(result$GeoMean, "label"), "Geometric Mean")
    expect_equal(attr(result$GeoCV_pct, "label"), "Geometric CV%")
    expect_equal(attr(result$CV_pct, "label"), "CV%")
    expect_equal(attr(result$n_blq, "label"), "Number BLQ")
  })
})

describe("t_pkct01_dose", {
  it("stratifies by DOSEA instead of TRT01A", {
    result <- t_pkct01_dose(pkct01_data)[[1]]
    expect_true("DOSEA" %in% names(result))
    expect_false("TRT01A" %in% names(result))
  })
})

describe("t_pkct01_tad", {
  it("uses NRRLT as time variable instead of NFRLT", {
    result <- t_pkct01_tad(pkct01_data)[[1]]
    expect_true("NRRLT" %in% names(result))
    expect_false("NFRLT" %in% names(result))
  })
})

describe("t_pkct01_dose_tad", {
  it("uses DOSEA for stratification and NRRLT for time", {
    result <- t_pkct01_dose_tad(pkct01_data)[[1]]
    expect_true("DOSEA" %in% names(result))
    expect_true("NRRLT" %in% names(result))
  })
})
