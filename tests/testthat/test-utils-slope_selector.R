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
    expect_null(result)
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

describe("resolve_hl_include_exclude_conflicts", {
  base_data <- FIXTURE_PKNCA_DATA
  excl_col <- base_data$conc$columns$exclude_half.life
  incl_col <- base_data$conc$columns$include_half.life
  subjects <- unique(base_data$conc$data$USUBJID)

  it("clears exclude within a profile that has both include and exclude", {
    data <- base_data
    data$conc$data[[incl_col]] <- NA
    data$conc$data[[excl_col]] <- NA
    # Profile A: subject 1 has an inclusion and an exclusion (conflict)
    rows_a <- which(data$conc$data$USUBJID == subjects[1])
    data$conc$data[[incl_col]][rows_a] <- TRUE
    data$conc$data[[excl_col]][rows_a[1]] <- TRUE

    result <- resolve_hl_include_exclude_conflicts(data)
    res_rows_a <- which(result$conc$data$USUBJID == subjects[1])

    # Exclude column fully cleared within the conflicting profile
    expect_true(all(is.na(result$conc$data[[excl_col]][res_rows_a])))
    # The excluded point loses its inclusion; the rest keep it
    expect_true(is.na(result$conc$data[[incl_col]][rows_a[1]]))
    expect_true(all(result$conc$data[[incl_col]][rows_a[-1]] %in% TRUE))
  })

  it("does not clear a standalone exclusion in another profile", {
    data <- base_data
    data$conc$data[[incl_col]] <- NA
    data$conc$data[[excl_col]] <- NA
    # Profile A (subject 1): conflict (both include and exclude)
    rows_a <- which(data$conc$data$USUBJID == subjects[1])
    data$conc$data[[incl_col]][rows_a] <- TRUE
    data$conc$data[[excl_col]][rows_a[1]] <- TRUE
    # Profile B (subject 2): standalone exclusion, no inclusion -> no conflict
    rows_b <- which(data$conc$data$USUBJID == subjects[2])
    data$conc$data[[excl_col]][rows_b[1]] <- TRUE

    result <- resolve_hl_include_exclude_conflicts(data)

    # Profile B's exclusion must survive (regression: global clearing wiped it)
    res_rows_b <- which(result$conc$data$USUBJID == subjects[2])
    expect_true(result$conc$data[[excl_col]][rows_b[1]] %in% TRUE)
    expect_true(all(is.na(result$conc$data[[incl_col]][res_rows_b])))
  })

  it("leaves data untouched when no profile has a conflict", {
    data <- base_data
    data$conc$data[[incl_col]] <- NA
    data$conc$data[[excl_col]] <- NA
    # Only exclusions, no inclusions anywhere -> no conflict
    data$conc$data[[excl_col]][1] <- TRUE

    result <- resolve_hl_include_exclude_conflicts(data)
    expect_identical(result$conc$data[[excl_col]], data$conc$data[[excl_col]])
    expect_identical(result$conc$data[[incl_col]], data$conc$data[[incl_col]])
  })
})
