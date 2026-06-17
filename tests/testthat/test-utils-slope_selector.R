library(dplyr)
source(
  file.path(
    system.file("shiny", package = "aNCA"),
    "functions", "utils-slope_selector.R"
  )
)

EXISTING_FIXTURE <- data.frame(
  TYPE = "Exclusion",
  USUBJID = 1,
  ATPTREF = 1,
  PARAM = "A",
  PCSPEC = 1,
  RANGE = "3:6"
)

describe("check_slope_rule_overlap", {
  new_rule <- data.frame(
    TYPE = "Exclusion",
    USUBJID = 1,
    ATPTREF = 1,
    PARAM = "A",
    PCSPEC = 1,
    RANGE = "3:6",
    REASON = "outlier"
  )

  it("returns a data.frame when existing is NULL (first rule added)", {
    result <- check_slope_rule_overlap(NULL, new_rule)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$TYPE, "Exclusion")
  })

  it("returns NULL when cancelling the only existing exclusion", {
    existing <- data.frame(
      TYPE = "Exclusion", USUBJID = 1, ATPTREF = 1,
      PARAM = "A", PCSPEC = 1, RANGE = "3:6", REASON = "outlier"
    )
    result <- check_slope_rule_overlap(existing, new_rule)
    expect_equal(nrow(result), 0)
  })

  it("returns remaining rows when cancelling one of multiple exclusions", {
    existing <- data.frame(
      TYPE = c("Exclusion", "Exclusion"),
      USUBJID = c(1, 1), ATPTREF = c(1, 1),
      PARAM = c("A", "A"), PCSPEC = c(1, 1),
      RANGE = c("3:6", "7:9"), REASON = c("outlier", "other")
    )
    result <- check_slope_rule_overlap(existing, new_rule)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$RANGE, "7:9")
  })

  it("drops extra columns from existing that are not in new (column normalization)", {
    existing <- data.frame(
      TYPE = "Exclusion", USUBJID = 1, ATPTREF = 1,
      PARAM = "A", PCSPEC = 1, RANGE = "3:6",
      EXTRA_COL = "should_be_dropped", stringsAsFactors = FALSE
    )
    new_rule_no_reason <- data.frame(
      TYPE = "Selection", USUBJID = 1, ATPTREF = 1,
      PARAM = "A", PCSPEC = 1, RANGE = "1:3",
      stringsAsFactors = FALSE
    )
    result <- check_slope_rule_overlap(existing, new_rule_no_reason)
    expect_false("EXTRA_COL" %in% names(result))
    expect_equal(nrow(result), 2)
  })

  it("keeps only common columns when new has missing columns (e.g. no REASON)", {
    existing <- data.frame(
      TYPE = "Exclusion", USUBJID = 1, ATPTREF = 1,
      PARAM = "A", PCSPEC = 1, RANGE = "3:6",
      REASON = "outlier", stringsAsFactors = FALSE
    )
    new_no_reason <- data.frame(
      TYPE = "Selection", USUBJID = 1, ATPTREF = 1,
      PARAM = "B", PCSPEC = 1, RANGE = "1:3",
      stringsAsFactors = FALSE
    )
    result <- check_slope_rule_overlap(existing, new_no_reason)
    expect_false("REASON" %in% names(result))
    expect_equal(nrow(result), 2)
  })
})

describe("update_pknca_with_rules", {
  old_data <- FIXTURE_PKNCA_DATA
  group1 <- old_data$intervals %>%
    select(any_of(c(group_vars(old_data)))) %>%
    .[1, , drop = FALSE]

  it("applies selection and exclusion rules to data", {
    slopes_incl <- cbind(
      data.frame(TYPE = "Selection", USUBJID = 1, RANGE = "2:4", REASON = "because I want to"),
      group1
    )
    slopes_excl <- cbind(
      data.frame(TYPE = "Exclusion", USUBJID = 1, RANGE = "2:4", REASON = "always good reasons"),
      group1
    )

    new_with_incl <- update_pknca_with_rules(old_data, slopes_incl)
    new_with_excl <- update_pknca_with_rules(old_data, slopes_excl)

    old_have_points_na <- all(is.na(old_data$conc$data %>%
                                      filter(USUBJID == group1$USUBJID, AFRLT >= 2, AFRLT <= 4) %>%
                                      pull(include_half.life)))

    new_have_points_incl <- all(new_with_incl$conc$data %>%
                                  filter(USUBJID == group1$USUBJID, AFRLT >= 2, AFRLT <= 4) %>%
                                  pull(include_half.life))

    new_have_points_excl <- all(new_with_excl$conc$data %>%
                                  filter(USUBJID == group1$USUBJID, AFRLT >= 2, AFRLT <= 4) %>%
                                  pull(exclude_half.life))

    expect_true(all(old_have_points_na, new_have_points_incl, new_have_points_excl))
  })

  it("returns an error for invalid rule types", {
    slopes_invalid <- cbind(
      data.frame(TYPE = "Invalid", ID = 1, RANGE = "2:4", REASON = "invalid type"),
      group1
    )
    expect_error(
      update_pknca_with_rules(old_data, slopes_invalid),
      regexp = "Unknown TYPE in slopes: Invalid"
    )
  })
})
