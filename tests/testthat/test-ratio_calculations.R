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
  it("computes correct ratios for simple case", {

    res <- FIXTURE_PKNCA_RES
    numerator_groups <- data.frame(PARAM = "B")
    denominator_groups <- data.frame(PARAM = "A")

    ratios <- calculate_ratios(
      res,
      parameter = "AUCINT",
      match_cols = c("start"),
      denominator_groups = denominator_groups,
      numerator_groups = numerator_groups
    )

    expect_equal(ratios$PPORRES, c(2, 2, 2))
    expect_equal(ratios$PPSTRES, c(2, 2, 2))
    expect_true(all(grepl("RATIO", ratios$PPTESTCD)))
  })

  it("handles adjusting_factor", {
    res <- list(
      result = data.frame(
        USUBJID = c("A", "A"),
        VISIT = c(1, 1),
        TREATMENT = c("Drug", "Placebo"),
        PPTESTCD = c("AUCINF", "AUCINF"),
        PPTEST = c("AUCINF", "AUCINF"),
        PPORRES = c(100, 50),
        PPSTRES = c(100, 50),
        PPORRESU = c("ng/mL", "ng/mL"),
        PPSTRESU = c("ng/mL", "ng/mL"),
        stringsAsFactors = FALSE
      )
    )
    class(res) <- "PKNCAresults"

    numerator_groups <- data.frame(TREATMENT = "Drug")
    denominator_groups <- data.frame(TREATMENT = "Placebo")

    ratios <- calculate_ratios(
      res,
      parameter = "AUCINF",
      match_cols = c("USUBJID", "VISIT"),
      denominator_groups = denominator_groups,
      numerator_groups = numerator_groups,
      adjusting_factor = 2
    )

    expect_equal(ratios$PPORRES, 4)
    expect_equal(ratios$PPSTRES, 4)
  })

  it("returns empty data frame if no matches", {
    res <- list(
      result = data.frame(
        USUBJID = c("A", "A"),
        VISIT = c(1, 1),
        TREATMENT = c("Drug", "Placebo"),
        PPTESTCD = c("AUCINF", "AUCINF"),
        PPTEST = c("AUCINF", "AUCINF"),
        PPORRES = c(100, 50),
        PPSTRES = c(100, 50),
        PPORRESU = c("ng/mL", "ng/mL"),
        PPSTRESU = c("ng/mL", "ng/mL"),
        stringsAsFactors = FALSE
      )
    )
    class(res) <- "PKNCAresults"

    numerator_groups <- data.frame(TREATMENT = "Drug")
    denominator_groups <- data.frame(TREATMENT = "Other")

    ratios <- calculate_ratios(
      res,
      parameter = "AUCINF",
      match_cols = c("USUBJID", "VISIT"),
      denominator_groups = denominator_groups,
      numerator_groups = numerator_groups
    )

    expect_equal(nrow(ratios), 0)
  })

  it("allows custom PPTESTCD and PPTEST", {
    res <- list(
      result = data.frame(
        USUBJID = c("A", "A"),
        VISIT = c(1, 1),
        TREATMENT = c("Drug", "Placebo"),
        PPTESTCD = c("AUCINF", "AUCINF"),
        PPTEST = c("AUCINF", "AUCINF"),
        PPORRES = c(100, 50),
        PPSTRES = c(100, 50),
        PPORRESU = c("ng/mL", "ng/mL"),
        PPSTRESU = c("ng/mL", "ng/mL"),
        stringsAsFactors = FALSE
      )
    )
    class(res) <- "PKNCAresults"

    numerator_groups <- data.frame(TREATMENT = "Drug")
    denominator_groups <- data.frame(TREATMENT = "Placebo")

    ratios <- calculate_ratios(
      res,
      parameter = "AUCINF",
      match_cols = c("USUBJID", "VISIT"),
      denominator_groups = denominator_groups,
      numerator_groups = numerator_groups,
      custom.pptestcd = "MYRATIO",
      custom.pptest = "My Custom Ratio"
    )

    expect_equal(ratios$PPTESTCD, "MYRATIO")
    expect_equal(ratios$PPTEST, "My Custom Ratio")
  })
})
