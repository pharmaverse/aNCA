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

describe("multiple_matrix_ratios", {
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

describe("calculate_ratios", {
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
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      match_cols = c("start", "end", "USUBJID"),
      ref_groups = ref_groups,
      test_groups = test_groups
    )

    expect_equal(ratios$PPSTRES, c(2 / 3, 4 / 5), tolerance = 1e-2)
    expect_true(all(grepl("RACMAX", ratios$PPTESTCD)))
  })

  it("computes correct ratios for simple case (PKNCAresults)", {
    pknca_res_with_ratios <- calculate_ratios(
      res_simple,
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      match_cols = c("start", "end", "USUBJID"),
      ref_groups = ref_groups,
      test_groups = test_groups
    )
    ratios <- pknca_res_with_ratios$result %>%
      filter(PPTESTCD == "RACMAX")

    expect_equal(nrow(pknca_res_with_ratios$result), nrow(res_simple$result) + 2)
    expect_equal(ratios$PPSTRES, c(2 / 3, 4 / 5), tolerance = 1e-2)
    expect_true(all(grepl("RACMAX", ratios$PPTESTCD)))
  })

  it("handles adjusting_factor", {
    ratios <- calculate_ratios(
      res_simple$result,
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      match_cols = c("start", "end", "USUBJID"),
      ref_groups = ref_groups,
      test_groups = test_groups,
      adjusting_factor = 2
    )

    expect_equal(ratios$PPORRES, c(2 / 3, 4 / 5) * 2)
    expect_true(all(grepl("RACMAX", ratios$PPTESTCD)))
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
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      match_cols = c("start"),
      ref_groups = ref_groups,
      test_groups = test_groups
    )
    ratios <- ratios$result %>%
      filter(PPTESTCD == "RACMAX")

    expect_equal(ratios$PPORRES, c(2 / 3, 4 / 5) * 1000)
    expect_equal(ratios$PPORRESU, rep("fraction", 2))
    expect_true(all(grepl("RACMAX", ratios$PPTESTCD)))
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
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      match_cols = c("start"),
      ref_groups = ref_groups,
      test_groups = test_groups
    )
    ratios <- ratios$result %>%
      filter(PPTESTCD == "RACMAX")

    expect_equal(ratios$PPORRES, c(2 / 3, 4 / 5))
    expect_equal(ratios$PPORRESU, rep("ng/mL/unknown_unit", 2))
  })

  it("returns error when a non-group column is used for match_cols or ref_groups", {
    expect_error(
      calculate_ratios(
        res,
        test_parameter = "CMAX",
        ref_parameter = "CMAX",
        match_cols = c("UNKNOWN_COL"),
        ref_groups = c(ref_groups, "UNKNOWN_COL"),
        test_groups = test_groups
      ),
      "match_cols and ref_groups must contain valid group column names in PKNCAres:"
    )

    expect_error(
      calculate_ratios(
        res,
        test_parameter = "CMAX",
        ref_parameter = "CMAX",
        match_cols = c("start"),
        ref_groups = ref_groups,
        test_groups = data.frame(UNKNOWN_COL = "X")
      ),
      "match_cols and ref_groups must contain valid group column names in PKNCAres:"
    )
  })

  it("allows custom PPTESTCD", {
    ratios <- calculate_ratios(
      res_simple,
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      match_cols = c("start"),
      ref_groups = ref_groups,
      test_groups = test_groups,
      custom_pptestcd = "MYRATIO"
    )
    ratios <- ratios$result %>%
      filter(PPTESTCD == "MYRATIO")
    expect_equal(ratios$PPTESTCD, c("MYRATIO", "MYRATIO"))
  })

  it("computes correct ratios when test_parameter and ref_parameter are different", {
    # Create a result with two parameters: CMAX and TMAX
    res_diff <- res_simple
    res_diff$result <- rbind(
      res_simple$result %>% mutate(PPTESTCD = "CMAX", PPORRES = 2, PPSTRES = 2),
      res_simple$result %>% mutate(PPTESTCD = "TMAX", PPORRES = 3, PPSTRES = 3)
    )
    # Use CMAX as test, TMAX as reference
    ratios <- calculate_ratios(
      res_diff,
      test_parameter = "CMAX",
      ref_parameter = "TMAX",
      match_cols = c("start", "end", "USUBJID"),
      ref_groups = ref_groups,
      test_groups = test_groups
    )
    ratios_df <- ratios$result %>% filter(PPTESTCD == "RACMAX")
    expect_equal(ratios_df$PPORRES, rep(c(2 / 3), 4))
    expect_equal(ratios_df$PPSTRES, rep(c(2 / 3), 4))
    expect_true(all(grepl("RACMAX", ratios_df$PPTESTCD)))
  })

  it("it returns warning in simple case but test_parameter is FAKE ", {
    expect_warning(
      calculate_ratios(
        res_simple$result,
        test_parameter = "FAKE",
        ref_parameter = "CMAX",
        match_cols = c("start", "end", "USUBJID"),
        ref_groups = ref_groups,
        test_groups = test_groups
      ), "No test_parameter"
    )
  })

  it("it returns warning in simple case but ref_parameter is FAKE ", {
    expect_warning(
      calculate_ratios(
        res_simple$result,
        test_parameter = "CMAX",
        ref_parameter = "FAKE",
        match_cols = c("start", "end", "USUBJID"),
        ref_groups = ref_groups,
        test_groups = test_groups
      ), "No ref_parameter"
    )
  })

  it("handles no test and reference matches (data.frame) and returns correct structure", {
    # Use test_groups that don't exist in the data to force an empty df_test
    test_groups_no_match <- data.frame(PARAM = "Z")

    ratios_empty <- calculate_ratios.data.frame(
      res_simple$result,
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      match_cols = c("start", "end", "USUBJID"),
      ref_groups = ref_groups,
      test_groups = test_groups_no_match
    )

    # Check that it returned an empty data frame
    expect_equal(nrow(ratios_empty), 0)

    # Check that the critical columns exist
    expect_true("PPANMETH" %in% names(ratios_empty))
    expect_true("PPORRESU" %in% names(ratios_empty))
    expect_true("PPSTRESU" %in% names(ratios_empty))

    # Check that the columns have the correct (character) type
    expect_true(is.character(ratios_empty$PPANMETH))
    expect_true(is.character(ratios_empty$PPORRESU))
    expect_true(is.character(ratios_empty$PPSTRESU))
  })
  it("uses all alternative groups to ref_groups when test_groups is NULL", {
    test_groups <- NULL
    ref_groups <- data.frame(PARAM = "A")

    ratios <- calculate_ratios(
      res_simple$result,
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      match_cols = c("start", "end", "USUBJID"),
      ref_groups = ref_groups,
      test_groups = test_groups
    )
    expect_equal(ratios$PPSTRES, c(2 / 3, 4 / 5), tolerance = 1e-2)
    expect_true(all(grepl("RACMAX", ratios$PPTESTCD)))

  })
  it("returns only PPORRESU when PPSTRESU is not defined in the input", {
    res_no_units <- res_simple
    res_no_units$result <- res_simple$result %>%
      select(-PPSTRESU, -PPSTRES)

    ratios <- calculate_ratios(
      res_no_units,
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      match_cols = c("start", "end", "USUBJID"),
      ref_groups = ref_groups,
      test_groups = test_groups
    )
    ratios_df <- ratios$result %>%
      filter(PPTESTCD == "RACMAX")

    expect_equal(ratios_df$PPORRESU, rep("fraction", 2))
    expect_false("PPSTRESU" %in% names(ratios_df))
  })

  it("aggregates reference values with aggregate_subject='yes'", {
    # Fixture: subjects 1,2,7 are EX PARAM A; subjects 3,4,5 are IV PARAM A
    # No subject has both routes, so aggregate uses mean of IV subjects
    res_agg <- res
    res_agg$result <- res$result %>%
      filter(PARAM == "A", PPTESTCD == "CMAX", type_interval == "main") %>%
      mutate(PPORRESU = "ng/mL", PPSTRESU = "ng/mL")

    ratios <- calculate_ratio_app(
      res = res_agg,
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      test_group = "(all other levels)",
      ref_group = "ROUTE: intravascular",
      aggregate_subject = "yes"
    )

    ex_subjects <- res_agg$result %>% filter(ROUTE == "extravascular")
    iv_mean <- mean(
      res_agg$result$PPORRES[res_agg$result$ROUTE == "intravascular"],
      na.rm = TRUE
    )

    expect_equal(nrow(ratios), nrow(ex_subjects))
    expect_equal(sort(ratios$PPORRES), sort(ex_subjects$PPORRES / iv_mean))
    expect_true(all(grepl("\\(mean\\)", ratios$PPTESTCD)))
  })

  it("if-needed uses individual match when available", {
    # Subject 8 has both EX (ATPTREF=1) and IV (ATPTREF=2) for PARAM A
    # With if-needed, subject 8 gets an individual ratio (no aggregation needed)
    res_ifn <- res
    res_ifn$result <- res$result %>%
      filter(USUBJID == 8, PARAM == "A", PPTESTCD == "CMAX", type_interval == "main") %>%
      mutate(PPORRESU = "ng/mL", PPSTRESU = "ng/mL")

    ratios <- calculate_ratio_app(
      res = res_ifn,
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      test_group = "(all other levels)",
      ref_group = "ROUTE: intravascular",
      aggregate_subject = "if-needed"
    )

    ex_val <- res_ifn$result %>%
      filter(ROUTE == "extravascular") %>% pull(PPORRES)
    iv_val <- res_ifn$result %>%
      filter(ROUTE == "intravascular") %>% pull(PPORRES)

    expect_equal(nrow(ratios), 1)
    expect_equal(ratios$PPORRES, ex_val / iv_val)
    expect_false(grepl("\\(mean\\)", ratios$PPTESTCD))
  })

  it("if-needed falls back to aggregation when no individual match exists", {
    # Subjects 1,2,7 are EX only; subjects 3,4,5 are IV only (PARAM A)
    # No subject has both routes, so if-needed must fall back to aggregation
    res_ifn <- res
    res_ifn$result <- res$result %>%
      filter(PARAM == "A", PPTESTCD == "CMAX", type_interval == "main",
             USUBJID %in% c(1, 2, 3, 4, 5, 7)) %>%
      mutate(PPORRESU = "ng/mL", PPSTRESU = "ng/mL")

    ratios <- calculate_ratio_app(
      res = res_ifn,
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      test_group = "(all other levels)",
      ref_group = "ROUTE: intravascular",
      aggregate_subject = "if-needed"
    )

    ex_subjects <- res_ifn$result %>% filter(ROUTE == "extravascular")
    iv_mean <- mean(
      res_ifn$result$PPORRES[res_ifn$result$ROUTE == "intravascular"],
      na.rm = TRUE
    )

    expect_equal(nrow(ratios), nrow(ex_subjects))
    expect_equal(sort(ratios$PPORRES), sort(ex_subjects$PPORRES / iv_mean))
    expect_true(all(grepl("\\(mean\\)", ratios$PPTESTCD)))
  })

  it("if-needed uses individual where available and aggregates the rest", {
    # Subject 8 has both routes (individual match possible)
    # Subjects 1,2,7 are EX only (must fall back to aggregation)
    # IV subjects: 3,4,5 (IV only) + 8 (both routes)
    res_mix <- res
    res_mix$result <- res$result %>%
      filter(PARAM == "A", PPTESTCD == "CMAX", type_interval == "main") %>%
      mutate(PPORRESU = "ng/mL", PPSTRESU = "ng/mL")

    ratios <- calculate_ratio_app(
      res = res_mix,
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      test_group = "(all other levels)",
      ref_group = "ROUTE: intravascular",
      aggregate_subject = "if-needed"
    )

    # Subject 8: individual ratio (EX / IV for that subject, no mean suffix)
    s8_ex <- res_mix$result %>%
      filter(USUBJID == 8, ROUTE == "extravascular") %>% pull(PPORRES)
    s8_iv <- res_mix$result %>%
      filter(USUBJID == 8, ROUTE == "intravascular") %>% pull(PPORRES)
    s8_ratio <- ratios %>% filter(USUBJID == 8)
    expect_equal(nrow(s8_ratio), 1)
    expect_equal(s8_ratio$PPORRES, s8_ex / s8_iv)
    expect_false(grepl("\\(mean\\)", s8_ratio$PPTESTCD))

    # Subjects 1,2,7: aggregated ratio (EX / mean of all IV, with mean suffix)
    # Subject 2 has 2 ATPTREFs (both EX), so 4 rows total
    other_ratios <- ratios %>% filter(USUBJID != 8)
    expect_equal(nrow(other_ratios), 4)
    expect_true(all(grepl("\\(mean\\)", other_ratios$PPTESTCD)))
  })
})

describe("parse_interval_parameter", {
  it("parses a range-suffixed parameter", {
    result <- parse_interval_parameter("AUCINT_0-20")
    expect_true(result$is_interval)
    expect_equal(result$base, "AUCINT")
    expect_equal(result$start, 0)
    expect_equal(result$end, 20)
  })

  it("parses decimal start/end values", {
    result <- parse_interval_parameter("AUCINT_0.5-12.5")
    expect_true(result$is_interval)
    expect_equal(result$base, "AUCINT")
    expect_equal(result$start, 0.5)
    expect_equal(result$end, 12.5)
  })

  it("returns is_interval=FALSE for a regular parameter", {
    result <- parse_interval_parameter("CMAX")
    expect_false(result$is_interval)
    expect_equal(result$base, "CMAX")
    expect_null(result$start)
    expect_null(result$end)
  })

  it("handles multi-part base names", {
    result <- parse_interval_parameter("AUCINTAD_0-24")
    expect_true(result$is_interval)
    expect_equal(result$base, "AUCINTAD")
    expect_equal(result$start, 0)
    expect_equal(result$end, 24)
  })
})

describe("calculate_ratio_app with interval parameters", {
  res <- FIXTURE_PKNCA_RES
  res$result$PPTEST <- translate_terms(res$result$PPTESTCD, "PPTESTCD", "PPTEST")

  # The fixture has manual intervals with AUCINT at start_dose=0,end_dose=2
  # and start_dose=2,end_dose=4 for both ATPTREF periods.
  res_interval <- res
  res_interval$result <- res$result %>%
    filter(USUBJID == 8) %>%
    mutate(
      PPORRESU = "ng/mL",
      PPSTRESU = "ng/mL"
    )

  it("computes ratios using interval parameter as test", {
    interval_rows <- res_interval$result %>%
      filter(PPTESTCD == "AUCINT", start_dose == 0, end_dose == 2)
    skip_if(nrow(interval_rows) == 0, "No AUCINT_0-2 rows in fixture")

    ratios <- calculate_ratio_app(
      res = res_interval,
      test_parameter = "AUCINT_0-2",
      ref_parameter = "AUCINT_0-2",
      ref_group = "PARAM: A",
      test_group = "(all other levels)",
      aggregate_subject = "no"
    )

    expect_true(nrow(ratios) > 0)
    expect_true(all(grepl("RAAUCINT", ratios$PPTESTCD)))
  })

  it("filters to the correct interval and excludes other intervals", {
    ratios_0_2 <- calculate_ratio_app(
      res = res_interval,
      test_parameter = "AUCINT_0-2",
      ref_parameter = "AUCINT_0-2",
      ref_group = "PARAM: A",
      test_group = "(all other levels)",
      aggregate_subject = "no"
    )

    ratios_2_4 <- calculate_ratio_app(
      res = res_interval,
      test_parameter = "AUCINT_2-4",
      ref_parameter = "AUCINT_2-4",
      ref_group = "PARAM: A",
      test_group = "(all other levels)",
      aggregate_subject = "no"
    )

    if (nrow(ratios_0_2) > 0 && nrow(ratios_2_4) > 0) {
      expect_false(identical(ratios_0_2$PPORRES, ratios_2_4$PPORRES))
    }
  })

  it("works with non-interval parameters unchanged", {
    ratios <- calculate_ratio_app(
      res = res_interval,
      test_parameter = "CMAX",
      ref_parameter = "CMAX",
      ref_group = "PARAM: A",
      test_group = "(all other levels)",
      aggregate_subject = "no"
    )

    expect_true(nrow(ratios) > 0)
    expect_true(all(grepl("RACMAX", ratios$PPTESTCD)))
  })

  it("supports different interval params as test and ref", {
    # When test and ref are different interval parameters with different ranges,
    # start/end must be excluded from match_cols so the merge can succeed.
    interval_rows_0_2 <- res_interval$result %>%
      filter(PPTESTCD == "AUCINT", start_dose == 0, end_dose == 2)
    skip_if(nrow(interval_rows_0_2) == 0, "No AUCINT_0-2 rows in fixture")

    ratios <- calculate_ratio_app(
      res = res_interval,
      test_parameter = "AUCINT_0-2",
      ref_parameter = "AUCINT_2-4",
      ref_group = "PARAM: A",
      test_group = "(all other levels)",
      aggregate_subject = "no"
    )

    expect_true(nrow(ratios) > 0)
  })
})
