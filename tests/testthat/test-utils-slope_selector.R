DATA_FIXTURE <- list(
  conc = list(
    data = data.frame(
      STUDYID = 1,
      PCSPEC = 1,
      USUBJID = rep(1:4, each = 4),
      NCA_PROFILE = 1,
      IX = rep(1:4, times = 4),
      PARAM = rep("A", 16),
      is.included.hl = FALSE,
      is.excluded.hl = FALSE,
      exclude_half.life = FALSE,
      REASON = ""
    )
  )
)

DOSNOS_FIXTURE <- data.frame(
  USUBJID = rep(1:4, each = 1),
  PARAM = rep("A", 4),
  PCSPEC = rep(1, 4),
  NCA_PROFILE = rep(1, 4)
)

slope_groups <- c("USUBJID", "PARAM", "PCSPEC", "NCA_PROFILE")

describe(".filter_slopes", {
  it("should handle slope selection", {
    selection <- data.frame(
      TYPE = rep("Selection", 2),
      USUBJID = c(1, 3),
      NCA_PROFILE = c(1, 1),
      PARAM = c("A", "A"),
      PCSPEC = c(1, 1),
      RANGE = c("1:3", "2:4"),
      REASON = "Test selection"
    )

    res <- filter_slopes(DATA_FIXTURE, selection, DOSNOS_FIXTURE, slope_groups)

    expect_true(all(res$is.included.hl[c(1:3, 6:8)]))
    expect_true(all(res$REASON[c(1:3, 6:8)] == "Test selection"))
  })

  it("should handle slope exclusion", {
    exclusion <- data.frame(
      TYPE = rep("Exclusion", 2),
      USUBJID = c(2, 4),
      NCA_PROFILE = c(1, 1),
      PARAM = c("A", "A"),
      PCSPEC = c(1, 1),
      RANGE = c("1:2", "2:3"),
      REASON = "Test exclusion"
    )

    res <- filter_slopes(DATA_FIXTURE, exclusion, DOSNOS_FIXTURE, slope_groups)

    expect_true(all(res$is.excluded.hl[c(5, 6, 14, 15)]))
    expect_true(all(res$REASON[c(5, 6, 14, 15)] == "Test exclusion"))
  })

  it("should throw an error for invalid data", {

    expect_error(filter_slopes(NULL, NULL, DOSNOS_FIXTURE), "Please provide valid data.")
    expect_error(filter_slopes(list(), NULL, DOSNOS_FIXTURE), "Please provide valid data.")
    expect_error(
      filter_slopes(list(conc = list()), NULL, DOSNOS_FIXTURE), "Please provide valid data."
    )
    expect_error(
      filter_slopes(list(conc = list()), NULL, DOSNOS_FIXTURE), "Please provide valid data."
    )
  })

  it("should throw an error if reasons are missing", {
    selection <- data.frame(
      TYPE = rep("Exclusion", 2),
      USUBJID = c(1, 3),
      NCA_PROFILE = c(1, 1),
      PARAM = c("A", "A"),
      PCSPEC = c(1, 1),
      RANGE = c("1:3", "2:4"),
      REASON = ""
    )

    expect_error(
      filter_slopes(DATA_FIXTURE, selection, DOSNOS_FIXTURE, slope_groups, TRUE),
      "^No reason provided for the following exclusions*"
    )
  })

  it("should return data unchanged if no slopes are provided", {
    res_null <- filter_slopes(DATA_FIXTURE, NULL, DOSNOS_FIXTURE, slope_groups)
    res_empty <- filter_slopes(DATA_FIXTURE, data.frame(), DOSNOS_FIXTURE, slope_groups)
    expect_equal(res_null, DATA_FIXTURE)
    expect_equal(res_empty, DATA_FIXTURE)
  })
})

EXISTING_FIXTURE <- data.frame(
  TYPE = "Exclusion",
  USUBJID = 1,
  NCA_PROFILE = 1,
  PARAM = "A",
  PCSPEC = 1,
  RANGE = "3:6"
)

describe("check_slope_rule_overlap", {
  it("should add new row if no overlap is detected", {
    # different type #
    NEW <- data.frame(
      TYPE = "Selection",
      USUBJID = 1,
      NCA_PROFILE = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "1:3"
    )

    expect_equal(nrow(check_slope_rule_overlap(EXISTING_FIXTURE, NEW, slope_groups)), 2)

    # different USUBJID #
    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 2,
      NCA_PROFILE = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "1:3"
    )

    expect_equal(nrow(check_slope_rule_overlap(EXISTING_FIXTURE, NEW, slope_groups)), 2)

    # different NCA_PROFILE #
    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      NCA_PROFILE = 2,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "1:3"
    )

    expect_equal(nrow(check_slope_rule_overlap(EXISTING_FIXTURE, NEW, slope_groups)), 2)

  })

  it("should remove overlapping points if no new points are detected", {
    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      NCA_PROFILE = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "4:5"
    )

    expect_equal(check_slope_rule_overlap(EXISTING_FIXTURE, NEW, slope_groups)$RANGE, "3,6")

    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      NCA_PROFILE = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "3:4"
    )

    expect_equal(check_slope_rule_overlap(EXISTING_FIXTURE, NEW, slope_groups)$RANGE, "5:6")

  })

  it("should add new points of partial overlap is detected", {
    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      NCA_PROFILE = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "4:9"
    )

    expect_equal(check_slope_rule_overlap(EXISTING_FIXTURE, NEW, slope_groups)$RANGE, "3:9")

  })

  it("should remove full row if full range of rule is removed", {
    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      NCA_PROFILE = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "3:6"
    )

    expect_equal(nrow(check_slope_rule_overlap(EXISTING_FIXTURE, NEW, slope_groups)), 0)

  })

  it("should warn if more than one range for single subject, profile and rule type is detected", {
    EXISTING <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      DOSNO = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "3:6"
    )

    DUPLICATE <- EXISTING %>%
      mutate(
        RANGE = "4:7"
      )

    expect_warning(
      check_slope_rule_overlap(rbind(EXISTING, DUPLICATE), DUPLICATE, slope_groups),
      "More than one range for single subject, profile and rule type detected."
    )
  })
})
