# Source the Shiny helper file so its functions are available for testing
source(
  system.file("shiny/functions/utils-units_table.R", package = "aNCA"),
  local = TRUE
)

describe(".append_ratio_rows", {
  default <- data.frame(
    PPTESTCD = c("cmax", "tmax"),
    PPORRESU = c("ng/mL", "hr"),
    PPSTRESU = c("ng/mL", "hr"),
    conversion_factor = c(1, 1),
    default = c(TRUE, TRUE),
    PARAM = c("A", "A"),
    stringsAsFactors = FALSE
  )

  it("returns default_units unchanged when ratio_rows is NULL", {
    result <- .append_ratio_rows(default, NULL)
    expect_equal(result, default)
  })

  it("returns default_units unchanged when ratio_rows has zero rows", {
    empty <- default[0, ]
    result <- .append_ratio_rows(default, empty)
    expect_equal(result, default)
  })

  it("appends ratio rows with default = TRUE and fills missing cols with NA", {
    ratio <- data.frame(
      PPTESTCD = "MRCMAX",
      PPORRESU = "fraction",
      PPSTRESU = "fraction",
      conversion_factor = 1,
      stringsAsFactors = FALSE
    )
    result <- .append_ratio_rows(default, ratio)
    expect_equal(nrow(result), 3)
    expect_true(result$default[3])
    expect_true(is.na(result$PARAM[3]))
    expect_equal(result$PPTESTCD[3], "MRCMAX")
  })

  it("preserves column order from default_units", {
    ratio <- data.frame(
      PPTESTCD = "MRCMAX",
      PPORRESU = "fraction",
      PPSTRESU = "fraction",
      conversion_factor = 1,
      stringsAsFactors = FALSE
    )
    result <- .append_ratio_rows(default, ratio)
    expect_equal(names(result), names(default))
  })
})

describe(".merge_custom_units", {
  default <- data.frame(
    PPTESTCD = c("cmax", "tmax"),
    PPORRESU = c("ng/mL", "hr"),
    PPSTRESU = c("ng/mL", "hr"),
    conversion_factor = c(1, 1),
    default = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  it("overrides PPSTRESU and conversion_factor from custom_units", {
    custom <- data.frame(
      PPTESTCD = "cmax",
      PPORRESU = "ng/mL",
      PPSTRESU = "ug/mL",
      conversion_factor = 0.001,
      stringsAsFactors = FALSE
    )
    result <- .merge_custom_units(default, custom)
    cmax_row <- result[result$PPTESTCD == "cmax", ]
    expect_equal(cmax_row$PPSTRESU, "ug/mL")
    expect_equal(cmax_row$conversion_factor, 0.001)
    expect_false(cmax_row$default)
  })

  it("leaves unmatched rows unchanged", {
    custom <- data.frame(
      PPTESTCD = "nonexistent",
      PPORRESU = "x",
      PPSTRESU = "y",
      conversion_factor = 99,
      stringsAsFactors = FALSE
    )
    result <- .merge_custom_units(default, custom)
    expect_equal(nrow(result), 2)
    expect_true(all(result$default))
  })

  it("does not add new rows from custom_units", {
    custom <- data.frame(
      PPTESTCD = c("cmax", "newparam"),
      PPORRESU = c("ng/mL", "x"),
      PPSTRESU = c("ug/mL", "y"),
      conversion_factor = c(0.001, 1),
      stringsAsFactors = FALSE
    )
    result <- .merge_custom_units(default, custom)
    expect_equal(nrow(result), 2)
  })
})
