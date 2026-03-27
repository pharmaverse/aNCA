describe("apply_custom_units", {
  base_result <- data.frame(
    PPTESTCD = c("CMAX", "CMAX", "AUCLST", "AUCLST"),
    PPORRES = c(10, 20, 100, 200),
    PPSTRES = c(10, 20, 100, 200),
    PPSTRESU = c("ng/mL", "ng/mL", "hr*ng/mL", "hr*ng/mL"),
    PPORRESU = c("ng/mL", "ng/mL", "hr*ng/mL", "hr*ng/mL"),
    PARAM = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )

  it("applies conversion factor to matching rows", {
    custom <- data.frame(
      PPTESTCD = "CMAX",
      PPORRESU = "ng/mL",
      PPSTRESU = "ug/mL",
      conversion_factor = 0.001,
      PARAM = "A",
      stringsAsFactors = FALSE
    )
    result <- apply_custom_units(base_result, custom)
    cmax_a <- result[result$PPTESTCD == "CMAX" & result$PARAM == "A", ]
    expect_equal(cmax_a$PPSTRES, 10 * 0.001)
    expect_equal(cmax_a$PPSTRESU, "ug/mL")
    cmax_b <- result[result$PPTESTCD == "CMAX" & result$PARAM == "B", ]
    expect_equal(cmax_b$PPSTRES, 20)
    expect_equal(cmax_b$PPSTRESU, "ng/mL")
  })

  it("preserves original values when no custom units match", {
    custom <- data.frame(
      PPTESTCD = "NONEXISTENT",
      PPORRESU = "x",
      PPSTRESU = "y",
      conversion_factor = 99,
      PARAM = "A",
      stringsAsFactors = FALSE
    )
    result <- apply_custom_units(base_result, custom)
    expect_equal(result$PPSTRES, base_result$PPSTRES)
    expect_equal(result$PPSTRESU, base_result$PPSTRESU)
  })

  it("preserves original PPSTRESU (not NA) for unmatched rows", {
    custom <- data.frame(
      PPTESTCD = "CMAX",
      PPORRESU = "ng/mL",
      PPSTRESU = "ug/mL",
      conversion_factor = 0.001,
      PARAM = "A",
      stringsAsFactors = FALSE
    )
    result <- apply_custom_units(base_result, custom)
    expect_false(any(is.na(result$PPSTRESU)))
    # Unmatched rows keep their original PPSTRESU
    auclst_rows <- result[result$PPTESTCD == "AUCLST", ]
    expect_equal(auclst_rows$PPSTRESU, c("hr*ng/mL", "hr*ng/mL"))
  })

  it("joins ratio rows by PPTESTCD only (ignoring groups)", {
    custom <- data.frame(
      PPTESTCD = c("CMAX", "MRCMAX"),
      PPORRESU = c("ng/mL", "fraction"),
      PPSTRESU = c("ug/mL", "%"),
      conversion_factor = c(0.001, 100),
      PARAM = c("A", NA_character_),
      stringsAsFactors = FALSE
    )
    result_with_ratio <- rbind(
      base_result,
      data.frame(
        PPTESTCD = "MRCMAX", PPORRES = 0.5, PPSTRES = 0.5,
        PPSTRESU = "fraction", PPORRESU = "fraction", PARAM = "A",
        stringsAsFactors = FALSE
      )
    )
    result <- apply_custom_units(result_with_ratio, custom)
    ratio_row <- result[result$PPTESTCD == "MRCMAX", ]
    expect_equal(ratio_row$PPSTRES, 0.5 * 100)
    expect_equal(ratio_row$PPSTRESU, "%")
  })

  it("does not produce conversion_factor column in output", {
    custom <- data.frame(
      PPTESTCD = "CMAX",
      PPORRESU = "ng/mL",
      PPSTRESU = "ug/mL",
      conversion_factor = 0.001,
      PARAM = "A",
      stringsAsFactors = FALSE
    )
    result <- apply_custom_units(base_result, custom)
    expect_false("conversion_factor" %in% names(result))
  })

  it("handles custom_units with no group columns", {
    custom <- data.frame(
      PPTESTCD = "CMAX",
      PPORRESU = "ng/mL",
      PPSTRESU = "ug/mL",
      conversion_factor = 0.001,
      stringsAsFactors = FALSE
    )
    result_no_groups <- base_result[, c("PPTESTCD", "PPORRES", "PPSTRES", "PPSTRESU", "PPORRESU")]
    result <- apply_custom_units(result_no_groups, custom)
    cmax_rows <- result[result$PPTESTCD == "CMAX", ]
    expect_equal(cmax_rows$PPSTRES, c(10, 20) * 0.001)
    expect_equal(cmax_rows$PPSTRESU, c("ug/mL", "ug/mL"))
  })

  it("deduplicates custom_units with duplicate PPTESTCDs", {
    custom <- data.frame(
      PPTESTCD = c("CMAX", "CMAX"),
      PPORRESU = c("ng/mL", "ng/mL"),
      PPSTRESU = c("ug/mL", "mg/mL"),
      conversion_factor = c(0.001, 0.000001),
      PARAM = c("A", "A"),
      stringsAsFactors = FALSE
    )
    result <- apply_custom_units(base_result, custom)
    cmax_a <- result[result$PPTESTCD == "CMAX" & result$PARAM == "A", ]
    # First row kept: ug/mL with factor 0.001
    expect_equal(cmax_a$PPSTRESU, "ug/mL")
    expect_equal(cmax_a$PPSTRES, 10 * 0.001)
    # No row duplication
    expect_equal(nrow(result), nrow(base_result))
  })

  it("deduplicates ratio custom_units with duplicate PPTESTCDs", {
    custom <- data.frame(
      PPTESTCD = c("MRCMAX", "MRCMAX"),
      PPORRESU = c("fraction", "fraction"),
      PPSTRESU = c("%", "ratio"),
      conversion_factor = c(100, 1),
      PARAM = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE
    )
    result_with_ratio <- rbind(
      base_result,
      data.frame(
        PPTESTCD = "MRCMAX", PPORRES = 0.5, PPSTRES = 0.5,
        PPSTRESU = "fraction", PPORRESU = "fraction", PARAM = "A",
        stringsAsFactors = FALSE
      )
    )
    result <- apply_custom_units(result_with_ratio, custom)
    ratio_row <- result[result$PPTESTCD == "MRCMAX", ]
    # First row kept: "%" with factor 100
    expect_equal(ratio_row$PPSTRESU, "%")
    expect_equal(ratio_row$PPSTRES, 0.5 * 100)
    # No row duplication
    expect_equal(nrow(ratio_row), 1)
  })

  it("deduplicates ratio rows with different PPORRESU values", {
    custom <- data.frame(
      PPTESTCD = c("MRCMAX", "MRCMAX"),
      PPORRESU = c("fraction", NA_character_),
      PPSTRESU = c("%", "ratio"),
      conversion_factor = c(100, 1),
      PARAM = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE
    )
    result_with_ratio <- rbind(
      base_result,
      data.frame(
        PPTESTCD = "MRCMAX", PPORRES = 0.5, PPSTRES = 0.5,
        PPSTRESU = "fraction", PPORRESU = "fraction", PARAM = "A",
        stringsAsFactors = FALSE
      )
    )
    result <- apply_custom_units(result_with_ratio, custom)
    ratio_row <- result[result$PPTESTCD == "MRCMAX", ]
    # Deduplicated to one row per PPTESTCD, no row multiplication
    expect_equal(nrow(ratio_row), 1)
  })
})
