data <- data.frame(
  USUBJID = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A"),
  TIME = c(0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2),
  MATRIX = c("BLOOD", "BLOOD", "BLOOD", "PLASMA", "PLASMA", "PLASMA",
             "BRAIN", "BRAIN", "BRAIN", "LIVER", "LIVER", "LIVER"),
  CONC = c(10, 20, 15, 25, 30, 40, 5, 10, 8, 12, 18, 16),
  UNITS = rep("ng/mL", 12)
)

describe("multiple_matrix_ratios function", {


  it("computes correct ratios", {
    result <- multiple_matrix_ratios(data, "MATRIX", "CONC", "UNITS",
                                     c("TIME", "USUBJID"), c("BLOOD", "BRAIN"),
                                     c("PLASMA", "LIVER"))

    expected_ratios <- c(10 / 25, 20 / 30, 15 / 40, 10 / 12, 20 / 18, 15 / 16,
                         5 / 25, 10 / 30, 8 / 40, 5 / 12, 10 / 18, 8 / 16)
    expect_setequal(result$Ratio, signif(expected_ratios, 3))
    expect_equal(result$Unit[1], "unitless")
  })

  it("handles missing data correctly", {
    data <- data.frame(
      USUBJID = rep("A", 16),
      TIME = rep(c(0, 1, 2, 3), each = 4),
      MATRIX = c("BLOOD", "PLASMA", "BRAIN", "LIVER", "BLOOD", "PLASMA", "BRAIN", "LIVER",
                 "BLOOD", "PLASMA", "BRAIN", "LIVER", "BLOOD", "PLASMA", "BRAIN", "LIVER"),
      CONC = c(10, 25, NA, 12, 20, NA, 10, 18, 15, 40, 8, NA, 22, 44, 12, 20),
      UNITS = rep("ng/mL", 16)
    )

    result <- multiple_matrix_ratios(data, "MATRIX", "CONC", "UNITS",
                                     c("TIME", "USUBJID"), c("BLOOD", "BRAIN"),
                                     c("PLASMA", "LIVER"))

    expect_true(nrow(result) == 10)
  })

  it("handles non-matching time points correctly", {
    data <- data.frame(
      USUBJID = rep("A", 12),
      TIME = c(0, 1, 2, 3, 0, 1, 2, 4, 0, 1, 3, 4),
      MATRIX = c("BLOOD", "PLASMA", "BRAIN", "LIVER", "BLOOD", "PLASMA", "BRAIN", "LIVER",
                 "BLOOD", "PLASMA", "BRAIN", "LIVER"),
      CONC = c(10, 25, 5, 12, 20, 30, 10, 18, 15, 40, 8, 16),
      UNITS = rep("ng/mL", 12)
    )

    result <- multiple_matrix_ratios(data, "MATRIX", "CONC", "UNITS",
                                     c("TIME", "USUBJID"), c("BLOOD", "BRAIN"),
                                     c("PLASMA", "LIVER"))

    expect_true(nrow(result) < 8)
  })

  it("calculates units correctly", {
    data <- data.frame(
      USUBJID = rep("A", 12),
      TIME = c(0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2),
      MATRIX = c("BLOOD", "BLOOD", "BLOOD", "PLASMA", "PLASMA", "PLASMA",
                 "BRAIN", "BRAIN", "BRAIN", "LIVER", "LIVER", "LIVER"),
      CONC = c(10, 20, 15, 25, 30, 40, 5, 10, 8, 12, 18, 16),
      UNITS = c("ng/mL", "ng/mL", "ng/mL", "mg/ng", "mg/ng", "mg/ng",
                "ng/mg", "ng/mg", "ng/mg", "mg/mL", "mg/mL", "mg/mL")
    )

    result <- multiple_matrix_ratios(data, "MATRIX", "CONC", "UNITS",
                                     c("TIME", "USUBJID"), "BLOOD", "PLASMA")

    expect_equal(result$Unit[1], "ng/mL")
  })
})
