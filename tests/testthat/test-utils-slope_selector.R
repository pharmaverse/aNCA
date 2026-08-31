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

describe("parse_plot_names_to_df", {
  it("parses key=value plot names into columns plus PLOTID", {
    plots <- setNames(list(1, 2), c(
      "USUBJID=101_PARAM=Analyte1_start=0_end=24",
      "USUBJID=102_PARAM=Analyte1_start=0_end=24"
    ))
    df <- parse_plot_names_to_df(plots)
    expect_setequal(names(df), c("USUBJID", "PARAM", "start", "end", "PLOTID"))
    expect_equal(df$USUBJID, c("101", "102"))
    expect_equal(df$PARAM, c("Analyte1", "Analyte1"))
    expect_equal(df$PLOTID, names(plots))
  })

  it("keeps values that contain underscores intact (issue #1378)", {
    # STUDYID value with underscores previously corrupted the parse and
    # produced NA/'' column names, crashing arrange_plots_by_groups().
    plots <- setNames(list(1, 2), c(
      "STUDYID=same_or_similar_USUBJID=2_PARAM=Drug A_start=0_end=Inf",
      "STUDYID=divergent_USUBJID=11233_PARAM=Drug B_start=0_end=Inf"
    ))
    df <- parse_plot_names_to_df(plots)
    # Both rows must share the same, non-empty column names.
    expect_false(any(is.na(names(df))))
    expect_false(any(names(df) == ""))
    expect_setequal(names(df), c("STUDYID", "USUBJID", "PARAM", "start", "end", "PLOTID"))
    expect_equal(df$STUDYID, c("same_or_similar", "divergent"))
    expect_equal(df$PARAM, c("Drug A", "Drug B"))
  })

  it("keeps underscores in every field, not just STUDYID", {
    plots <- setNames(list(1), "STUDYID=a_b_c_USUBJID=x_1_PARAM=Drug_A_start=0_end=Inf")
    df <- parse_plot_names_to_df(plots)
    expect_equal(df$STUDYID, "a_b_c")
    expect_equal(df$USUBJID, "x_1")
    expect_equal(df$PARAM, "Drug_A")
  })
})

describe("arrange_plots_by_groups", {
  it("orders plots by the requested group columns", {
    plots <- setNames(list("a", "b"), c(
      "USUBJID=2_PARAM=Drug A_start=0_end=Inf",
      "USUBJID=1_PARAM=Drug A_start=0_end=Inf"
    ))
    ordered <- arrange_plots_by_groups(plots, "USUBJID")
    expect_equal(
      names(ordered),
      c(
        "USUBJID=1_PARAM=Drug A_start=0_end=Inf",
        "USUBJID=2_PARAM=Drug A_start=0_end=Inf"
      )
    )
  })

  it("does not error when group values contain underscores (issue #1378)", {
    plots <- setNames(list("a", "b"), c(
      "STUDYID=same_or_similar_USUBJID=2_PARAM=Drug A_start=0_end=Inf",
      "STUDYID=divergent_USUBJID=11233_PARAM=Drug B_start=0_end=Inf"
    ))
    expect_silent(res <- arrange_plots_by_groups(plots, "STUDYID"))
    # "divergent" sorts before "same_or_similar"
    expect_equal(names(res)[1], "STUDYID=divergent_USUBJID=11233_PARAM=Drug B_start=0_end=Inf")
    expect_length(res, 2)
  })

  it("returns the list unchanged when empty", {
    expect_equal(arrange_plots_by_groups(list(), "USUBJID"), list())
  })
})
