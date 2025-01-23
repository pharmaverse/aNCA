DATA_FIXTURE <- list(
  conc = list(
    data = data.frame(
      STUDYID = 1,
      PCSPEC = 1,
      USUBJID = rep(1:4, each = 4),
      DOSNO = 1,
      IX = rep(1:4, times = 4),
      ANALYTE = rep("A", 16),
      is.included.hl = FALSE,
      is.excluded.hl = FALSE,
      exclude_half.life = FALSE,
      REASON = ""
    )
  )
)

PROFILES_FIXTURE <- data.frame(
  USUBJID = rep(1:4, each = 1),
  ANALYTE = rep("A", 4),
  PCSPEC = rep(1, 4),
  DOSNO = rep(1, 4)
)

describe(".filter_slopes", {
  it("should handle slope selection", {
    selection <- data.frame(
      TYPE = rep("Selection", 2),
      PATIENT = c(1, 3),
      PROFILE = c(1, 1),
      ANALYTE = c("A", "A"),
      PCSPEC = c(1, 1),
      IXrange = c("1:3", "2:4"),
      REASON = "Test selection"
    )

    res <- .filter_slopes(DATA_FIXTURE, selection, PROFILES_FIXTURE)
    expect_true(all(res$is.included.hl[c(1:3, 6:8)]))
    expect_true(all(res$REASON[c(1:3, 6:8)] == "Test selection"))
  })

  it("should handle slope exclusion", {
    exclusion <- data.frame(
      TYPE = rep("Exclusion", 2),
      PATIENT = c(2, 4),
      PROFILE = c(1, 1),
      ANALYTE = c("A", "A"),
      PCSPEC = c(1, 1),
      IXrange = c("1:2", "2:3"),
      REASON = "Test exclusion"
    )

    res <- .filter_slopes(DATA_FIXTURE, exclusion, PROFILES_FIXTURE)
    expect_true(all(res$is.excluded.hl[c(5, 6, 14, 15)]))
    expect_true(all(res$REASON[c(5, 6, 14, 15)] == "Test exclusion"))
  })

  it("should throw an error for invalid data", {
    expect_error(.filter_slopes(NULL, NULL, PROFILES_FIXTURE), "Please provide valid data.")
    expect_error(.filter_slopes(list(), NULL, PROFILES_FIXTURE), "Please provide valid data.")
    expect_error(
      .filter_slopes(list(conc = list()), NULL, PROFILES_FIXTURE), "Please provide valid data."
    )
    expect_error(
      .filter_slopes(list(conc = list()), NULL, PROFILES_FIXTURE), "Please provide valid data."
    )
  })
})

EXISTING_FIXTURE <- data.frame(
  TYPE = "Exclusion",
  PATIENT = 1,
  PROFILE = 1,
  ANALYTE = "A",
  PCSPEC = 1,
  IXrange = "3:6"
)

describe(".check_slope_rule_overlap", {
  it("should add new row if no overlap is detected", {
    # different type #
    NEW <- data.frame(
      TYPE = "Selection",
      PATIENT = 1,
      PROFILE = 1,
      ANALYTE = "A",
      PCSPEC = 1,
      IXrange = "1:3"
    )
    expect_equal(nrow(.check_slope_rule_overlap(EXISTING_FIXTURE, NEW)), 2)

    # different patient #
    NEW <- data.frame(
      TYPE = "Exclusion",
      PATIENT = 2,
      PROFILE = 1,
      ANALYTE = "A",
      PCSPEC = 1,
      IXrange = "1:3"
    )
    expect_equal(nrow(.check_slope_rule_overlap(EXISTING_FIXTURE, NEW)), 2)

    # different profile #
    NEW <- data.frame(
      TYPE = "Exclusion",
      PATIENT = 1,
      PROFILE = 2,
      ANALYTE = "A",
      PCSPEC = 1,
      IXrange = "1:3"
    )
    expect_equal(nrow(.check_slope_rule_overlap(EXISTING_FIXTURE, NEW)), 2)
  })

  it("should remove overlapping points if no new points are detected", {
    NEW <- data.frame(
      TYPE = "Exclusion",
      PATIENT = 1,
      PROFILE = 1,
      ANALYTE = "A",
      PCSPEC = 1,
      IXrange = "4:5"
    )
    expect_equal(.check_slope_rule_overlap(EXISTING_FIXTURE, NEW)$IXrange, "3,6")

    NEW <- data.frame(
      TYPE = "Exclusion",
      PATIENT = 1,
      PROFILE = 1,
      ANALYTE = "A",
      PCSPEC = 1,
      IXrange = "3:4"
    )
    expect_equal(.check_slope_rule_overlap(EXISTING_FIXTURE, NEW)$IXrange, "5:6")
  })

  it("should add new points of partial overlap is detected", {
    NEW <- data.frame(
      TYPE = "Exclusion",
      PATIENT = 1,
      PROFILE = 1,
      ANALYTE = "A",
      PCSPEC = 1,
      IXrange = "4:9"
    )
    expect_equal(.check_slope_rule_overlap(EXISTING_FIXTURE, NEW)$IXrange, "3:9")
  })

  it("should remove full row if full range of rule is removed", {
    NEW <- data.frame(
      TYPE = "Exclusion",
      PATIENT = 1,
      PROFILE = 1,
      ANALYTE = "A",
      PCSPEC = 1,
      IXrange = "3:6"
    )
    expect_equal(nrow(.check_slope_rule_overlap(EXISTING_FIXTURE, NEW)), 0)
  })
})
