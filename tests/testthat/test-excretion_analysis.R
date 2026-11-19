source(
  file.path(
    system.file("shiny", package = "aNCA"),
    "modules", "tab_nca", "excretion_analysis.R"
  ),
  local = TRUE
)

describe(".excretion_unit_value", {
  it("converts numeric-looking character values for unit calculations", {
    expect_equal(.excretion_unit_value(c("100", "12,5", "")), c(100, 12.5, NA))
  })
})

describe(".excretion_pooling_variables", {
  it("offers shared variables but excludes calculation and subject columns", {
    conc_data <- data.frame(
      GROUP = c("A", "B"),
      COHORT = c("1", "2"),
      USUBJID = c("01", "02"),
      AVAL = c(1, 2),
      stringsAsFactors = FALSE
    )
    dose_data <- data.frame(
      GROUP = c("A", "B"),
      COHORT = c("1", "2"),
      USUBJID = c("01", "02"),
      DOSEA = c(10, 10),
      stringsAsFactors = FALSE
    )

    expect_setequal(
      .excretion_pooling_variables(conc_data, dose_data),
      c("GROUP", "COHORT")
    )
  })
})

describe(".excretion_pool_doses", {
  it("uses a cumulative dose for each selected pool and dose occasion", {
    dose_data <- data.frame(
      STUDYID = rep("study-1", 5),
      DOSETRT = c("A", "A", "B", "B", "B"),
      USUBJID = sprintf("0%d", 1:5),
      GROUP = c("group-1", "group-1", "group-2", "group-2", "group-2"),
      DOSNOA = rep(1, 5),
      DOSEA = c(10, 10, 5, 5, 5)
    )

    result <- .excretion_pool_doses(
      dose_data = dose_data,
      dose_col = "DOSEA",
      dose_group_columns = c("STUDYID", "DOSETRT", "USUBJID"),
      pool_vars = "GROUP"
    )

    expect_equal(result$DOSEA, c(20, 20, 15, 15, 15))
  })

  it("keeps independently selected pooling variables separate", {
    dose_data <- data.frame(
      STUDYID = rep("study-1", 4),
      DOSETRT = rep("A", 4),
      USUBJID = sprintf("0%d", 1:4),
      GROUP = rep("group-1", 4),
      SEX = c("F", "F", "M", "M"),
      DOSNOA = rep(1, 4),
      DOSEA = c(10, 10, 5, 5)
    )

    result <- .excretion_pool_doses(
      dose_data = dose_data,
      dose_col = "DOSEA",
      dose_group_columns = c("STUDYID", "DOSETRT", "USUBJID"),
      pool_vars = c("GROUP", "SEX")
    )

    expect_equal(result$DOSEA, c(20, 20, 10, 10))
  })

  it("converts numeric-looking character doses before summing", {
    dose_data <- data.frame(
      STUDYID = rep("study-1", 2),
      DOSETRT = rep("A", 2),
      USUBJID = c("01", "02"),
      GROUP = rep("group-1", 2),
      DOSNOA = rep(1, 2),
      DOSEA = c("10", "10"),
      stringsAsFactors = FALSE
    )

    result <- .excretion_pool_doses(
      dose_data = dose_data,
      dose_col = "DOSEA",
      dose_group_columns = c("STUDYID", "DOSETRT", "USUBJID"),
      pool_vars = "GROUP"
    )

    expect_type(result$DOSEA, "double")
    expect_equal(result$DOSEA, c(20, 20))
  })

  it("requires a complete selection of pooling variables", {
    dose_data <- data.frame(
      STUDYID = "study-1",
      DOSETRT = "A",
      USUBJID = "01",
      DOSNOA = 1,
      DOSEA = 10
    )

    expect_error(
      .excretion_pool_doses(
        dose_data = dose_data,
        dose_col = "DOSEA",
        dose_group_columns = c("STUDYID", "DOSETRT", "USUBJID"),
        pool_vars = character(0)
      ),
      "Select at least one variable"
    )
  })
})
