describe("is_iqr_outlier: Tukey rule within groups", {
  it("flags a high outlier within a single group", {
    d <- data.frame(v = c(1, 2, 3, 4, 5, 100))
    out <- is_iqr_outlier(d, "v")
    expect_equal(which(out), 6L)
  })

  it("computes outliers independently per group", {
    d <- data.frame(
      g = rep(c("a", "b"), each = 6),
      v = c(1, 2, 3, 4, 5, 100, 10, 11, 12, 13, 14, 15)
    )
    out <- is_iqr_outlier(d, "v", "g")
    # 100 is an outlier in group a; group b has none
    expect_equal(which(out), 6L)
  })

  it("returns all FALSE for groups with fewer than 4 values", {
    d <- data.frame(v = c(1, 2, 1000))
    expect_false(any(is_iqr_outlier(d, "v")))
  })

  it("ignores NA values", {
    d <- data.frame(v = c(1, 2, 3, 4, NA, 100))
    out <- is_iqr_outlier(d, "v")
    expect_false(out[5])
    expect_true(out[6])
  })

  it("ignores group columns not present in the data", {
    d <- data.frame(v = c(1, 2, 3, 4, 5, 100))
    expect_equal(
      is_iqr_outlier(d, "v", c("missing_col")),
      is_iqr_outlier(d, "v")
    )
  })

  it("returns empty logical for empty data", {
    expect_equal(is_iqr_outlier(data.frame(v = numeric(0)), "v"), logical(0))
  })
})

describe("params_with_outliers: parameter-level detection", {
  make_res <- function(result) list(result = result)

  it("returns parameters with at least one non-excluded outlier", {
    result <- data.frame(
      PPTESTCD = c(rep("CMAX", 6), rep("AUCLST", 6)),
      PPSTRES = c(1, 2, 3, 4, 5, 100, 10, 11, 12, 13, 14, 15),
      exclude = "",
      stringsAsFactors = FALSE
    )
    res <- make_res(result)
    expect_equal(params_with_outliers(res), "CMAX")
  })

  it("excludes flagged records from the outlier calculation", {
    result <- data.frame(
      PPTESTCD = rep("CMAX", 6),
      PPSTRES = c(1, 2, 3, 4, 5, 100),
      exclude = c("", "", "", "", "", "flagged"),
      stringsAsFactors = FALSE
    )
    res <- make_res(result)
    # The only outlier is flag-excluded, so CMAX should not qualify
    expect_equal(params_with_outliers(res), character(0))
  })

  it("excludes manual (.pp_excl) records from the calculation", {
    result <- data.frame(
      PPTESTCD = rep("CMAX", 6),
      PPSTRES = c(1, 2, 3, 4, 5, 100),
      exclude = "",
      .pp_excl = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
      stringsAsFactors = FALSE
    )
    res <- make_res(result)
    expect_equal(params_with_outliers(res), character(0))
  })

  it("returns empty for empty results", {
    expect_equal(
      params_with_outliers(list(result = data.frame())),
      character(0)
    )
  })
})
