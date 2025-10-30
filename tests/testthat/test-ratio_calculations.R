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
  
  it("handles early exit with no matches (data.frame) and returns correct structure", {
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
    
    # 1. Check that it returned an empty data frame
    expect_equal(nrow(ratios_empty), 0)
    
    # 2. Check that the critical columns exist
    expect_true("PPANMETH" %in% names(ratios_empty))
    expect_true("PPORRESU" %in% names(ratios_empty))
    expect_true("PPSTRESU" %in% names(ratios_empty))
    
    # 3. Check that the columns have the correct (character) type
    expect_true(is.character(ratios_empty$PPANMETH))
    expect_true(is.character(ratios_empty$PPORRESU))
    expect_true(is.character(ratios_empty$PPSTRESU))
  })
  
  it("handles early exit with no matches (PKNCAresults) without bind_rows error", {
    # Use test_groups that don't exist in the data
    test_groups_no_match <- data.frame(PARAM = "Z")
    
    # This will call bind_rows(res_simple$result, ratios_empty) internally.
    # We expect it *not* to error, which confirms the fix.
    res_bound <- expect_no_error(
      calculate_ratios(
        res_simple, # The PKNCAresults object
        test_parameter = "CMAX",
        ref_parameter = "CMAX",
        match_cols = c("start", "end", "USUBJID"),
        ref_groups = ref_groups,
        test_groups = test_groups_no_match
      )
    )
    
    # Check that no rows were added
    expect_equal(nrow(res_bound$result), nrow(res_simple$result))
  })
})
