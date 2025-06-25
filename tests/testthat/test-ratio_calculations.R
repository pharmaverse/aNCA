data <- data.frame(
  USUBJID = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A"),
  TIME = c(0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2),
  MATRIX = c(
    "BLOOD", "BLOOD", "BLOOD", "PLASMA", "PLASMA", "PLASMA",
    "BRAIN", "BRAIN", "BRAIN", "LIVER", "LIVER", "LIVER"
  ),
  CONC = c(10, 20, 15, 25, 30, 40, 5, 10, 8, 12, 18, 16),
  UNITS = rep("ng/mL", 12)
)

describe("multiple_matrix_ratios function", {
  it("computes correct ratios", {
    result <- multiple_matrix_ratios(
      data, "MATRIX", "CONC", "UNITS",
      c("TIME", "USUBJID"), c("BLOOD", "BRAIN"),
      c("PLASMA", "LIVER")
    )

    expected_ratios <- c(
      10 / 25, 20 / 30, 15 / 40, 10 / 12, 20 / 18, 15 / 16,
      5 / 25, 10 / 30, 8 / 40, 5 / 12, 10 / 18, 8 / 16
    )
    expect_setequal(result$Ratio, signif(expected_ratios, 3))
  })

  it("handles missing data correctly", {
    data <- data.frame(
      USUBJID = rep("A", 16),
      TIME = rep(c(0, 1, 2, 3), each = 4),
      MATRIX = c(
        "BLOOD", "PLASMA", "BRAIN", "LIVER", "BLOOD", "PLASMA", "BRAIN", "LIVER",
        "BLOOD", "PLASMA", "BRAIN", "LIVER", "BLOOD", "PLASMA", "BRAIN", "LIVER"
      ),
      CONC = c(10, 25, NA, 12, 20, NA, 10, 18, 15, 40, 8, NA, 22, 44, 12, 20),
      UNITS = rep("ng/mL", 16)
    )

    result <- multiple_matrix_ratios(
      data, "MATRIX", "CONC", "UNITS",
      c("TIME", "USUBJID"), c("BLOOD", "BRAIN"),
      c("PLASMA", "LIVER")
    )

    expect_true(nrow(result) == 10)
  })

  it("handles non-matching time points correctly", {
    data <- data.frame(
      USUBJID = rep("A", 12),
      TIME = c(0, 1, 2, 3, 0, 1, 2, 4, 0, 1, 3, 4),
      MATRIX = c(
        "BLOOD", "PLASMA", "BRAIN", "LIVER", "BLOOD", "PLASMA", "BRAIN", "LIVER",
        "BLOOD", "PLASMA", "BRAIN", "LIVER"
      ),
      CONC = c(10, 25, 5, 12, 20, 30, 10, 18, 15, 40, 8, 16),
      UNITS = rep("ng/mL", 12)
    )

    result <- multiple_matrix_ratios(
      data, "MATRIX", "CONC", "UNITS",
      c("TIME", "USUBJID"), c("BLOOD", "BRAIN"),
      c("PLASMA", "LIVER")
    )

    expect_true(nrow(result) < 8)
  })
})

describe("calculate_ratios function", {

  res <- FIXTURE_PKNCA_RES
  res$result$PPTEST <- translate_terms(res$result$PPTESTCD, "PPTESTCD", "PPTEST")
  test_groups <- data.frame(PARAM = "B")
  ref_groups <- data.frame(PARAM = "A")


  # Make a simple input version that has same units and only 1 subject
  res_simple <- res
  res_simple$result <- res$result %>%
    filter(USUBJID == 8) %>%
    mutate(
      PPORRESU = "ng/mL",
      PPSTRESU = "ng/mL"
    )

  it("computes correct ratios for simple case (data.frame)", {

    ratios <- calculate_ratios(
      res_simple$result,
      parameter = "CMAX",
      match_cols = c("start", "end", "USUBJID"),
      ref_groups = ref_groups,
      test_groups = test_groups
    )

    expect_equal(ratios$PPSTRES, c(2 / 3, 4 / 5), tolerance = 1e-2)
    expect_true(all(grepl("RATIO", ratios$PPTESTCD)))
  })

  it("computes correct ratios for simple case (PKNCAresults)", {

    pknca_res_with_ratios <- calculate_ratios(
      res_simple,
      parameter = "CMAX",
      match_cols = c("start", "end", "USUBJID"),
      ref_groups = ref_groups,
      test_groups = test_groups
    )
    ratios <- pknca_res_with_ratios$result %>%
      filter(PPTESTCD == "CMAX_RATIO")

    expect_equal(nrow(pknca_res_with_ratios$result), nrow(res_simple$result) + 2)
    expect_equal(ratios$PPSTRES, c(2 / 3, 4 / 5), tolerance = 1e-2)
    expect_true(all(grepl("CMAX_RATIO", ratios$PPTESTCD)))
  })

  it("handles adjusting_factor", {

    ratios <- calculate_ratios(
      res_simple$result,
      parameter = "CMAX",
      match_cols = c("start", "end", "USUBJID"),
      ref_groups = ref_groups,
      test_groups = test_groups,
      adjusting_factor = 2
    )

    expect_equal(ratios$PPORRES, c(2 / 3, 4 / 5) * 2)
    expect_true(all(grepl("RATIO", ratios$PPTESTCD)))
  })

  it("handles unit conversions when needed and possible to convert", {
    res_with_diff_units <- res_simple
    res_with_diff_units$result <- res_simple$result %>%
      mutate(
        PPORRESU = ifelse(PARAM == "B", "ng/mL", "pg/mL"),
        PPSTRESU = ifelse(PARAM == "B", "ng/mL", "pg/mL")
      )

    ratios <- calculate_ratios(
      res_with_diff_units,
      parameter = "CMAX",
      match_cols = c("start"),
      ref_groups = ref_groups,
      test_groups = test_groups
    )
    ratios <- ratios$result %>%
      filter(PPTESTCD == "CMAX_RATIO")

    expect_equal(ratios$PPORRES, c(2 / 3, 4 / 5) * 1000)
    expect_equal(ratios$PPORRESU, rep("fraction", 2))
    expect_true(all(grepl("RATIO", ratios$PPTESTCD)))
  })

  it("handles when unit conversions are needed but not possible to convert", {
    res_with_diff_units <- res_simple
    res_with_diff_units$result <- res_simple$result %>%
      mutate(
        PPORRESU = ifelse(PARAM == "B", "ng/mL", "unknown_unit"),
        PPSTRESU = ifelse(PARAM == "B", "ng/mL", "unknown_unit")
      )

    ratios <- calculate_ratios(
      res_with_diff_units,
      parameter = "CMAX",
      match_cols = c("start"),
      ref_groups = ref_groups,
      test_groups = test_groups
    )
    ratios <- ratios$result %>%
      filter(PPTESTCD == "CMAX_RATIO")

    expect_equal(ratios$PPORRES, c(2 / 3, 4 / 5))
    expect_equal(ratios$PPORRESU, rep("ng/mL/unknown_unit", 2))
  })

  it("returns error when a non-group column is used for match_cols or ref_groups", {

    expect_error(
      calculate_ratios(
        res,
        parameter = "CMAX",
        match_cols = c("UNKNOWN_COL"),
        ref_groups = c(ref_groups, "UNKNOWN_COL"),
        test_groups = test_groups
      ),
      "match_cols and ref_groups must contain valid group column names in PKNCAres:"
    )

    expect_error(
      calculate_ratios(
        res,
        parameter = "CMAX",
        match_cols = c("start"),
        ref_groups = ref_groups,
        test_groups = data.frame(UNKNOWN_COL = "X")
      ),
      "match_cols and ref_groups must contain valid group column names in PKNCAres:"
    )
  })

  it("allows custom PPTESTCD and PPTEST", {

    ratios <- calculate_ratios(
      res_simple,
      parameter = "CMAX",
      match_cols = c("start"),
      ref_groups = ref_groups,
      test_groups = test_groups,
      custom.pptestcd = "MYRATIO",
      custom.pptest = "My Custom Ratio"
    )
    ratios <- ratios$result %>%
      filter(PPTESTCD == "MYRATIO")
    expect_equal(ratios$PPTESTCD, c("MYRATIO", "MYRATIO"))
    expect_equal(ratios$PPTEST, c("My Custom Ratio", "My Custom Ratio"))
  })
})
