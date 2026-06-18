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
