describe("get_conversion_factor", {
  it("handles simple time units", {
    expect_equal(get_conversion_factor("hr", "minute"), 60)
    expect_equal(get_conversion_factor("minute", "second"), 60)
    expect_equal(get_conversion_factor("day", "hour"), 24)
    expect_equal(get_conversion_factor("second", "minute"), 1 / 60)
    expect_equal(get_conversion_factor("hour", "second"), 3600)
    expect_equal(get_conversion_factor("minute", "hour"), 1 / 60)
    expect_equal(get_conversion_factor("day", "minute"), 1440)
  })

  it("handles simple concentration units", {
    expect_equal(get_conversion_factor("mg/L", "g/L"), 0.001)
    expect_equal(get_conversion_factor("g/dL", "kg/L"), 0.01)
    expect_equal(get_conversion_factor("ug/mL", "mg/L"), 1)
    expect_equal(get_conversion_factor("kg/L", "g/L"), 1000)
    expect_equal(get_conversion_factor("mg/dL", "g/L"), 0.01)
    expect_equal(get_conversion_factor("g/L", "mg/L"), 1000)
    expect_equal(get_conversion_factor("kg/L", "mg/L"), 1e6)
  })

  it("handles combined units", {
    expect_equal(get_conversion_factor("Hours*ug/mL", "Hours*mg/L"), 1)
    expect_equal(get_conversion_factor("Hours^2*ug/mL", "Hours^2*mg/L"), 1)
    expect_equal(get_conversion_factor("(Hours*ug/mL)/mg", "(Hours*mg/L)/g"), 1000)
    expect_equal(get_conversion_factor("mg/(Hours*ug/mL)", "g/(Hours*mg/L)"), 0.001)
    expect_equal(get_conversion_factor("(ug/mL)/(Hours*ug/mL)", "(mg/L)/(Hours*mg/L)"), 1)
    expect_equal(get_conversion_factor("Hours*mg/L", "Hours*ug/mL"), 1)
    expect_equal(get_conversion_factor("Hours^2*mg/L", "Hours^2*ug/mL"), 1)
    expect_equal(get_conversion_factor("Hours*kg/L", "Hours*g/L"), 1000)
    expect_equal(get_conversion_factor("Hours*mg/L", "Hours*kg/L"), 1e-6)
  })

  it("returns NA for non-convertible units", {
    expect_true(is.na(get_conversion_factor("meter", "second")))
    expect_true(is.na(get_conversion_factor("kg", "hour")))
    expect_true(is.na(get_conversion_factor("liter", "gram")))
    expect_true(is.na(get_conversion_factor("meter", "liter")))
    expect_true(is.na(get_conversion_factor("second", "gram")))
    expect_true(is.na(get_conversion_factor("hour", "meter")))
  })

  it("handles vector inputs", {
    expect_equal(get_conversion_factor(c("hr", "minute"), c("minute", "second")), c(60, 60))
    expect_equal(get_conversion_factor(c("day", "mg/L"), c("hour", "g/L")), c(24, 0.001))
    expect_equal(get_conversion_factor(c("second", "kg/L"), c("minute", "g/L")), c(1 / 60, 1000))
    expect_equal(get_conversion_factor(c("hour", "ug/mL"), c("second", "mg/L")), c(3600, 1))
    expect_equal(get_conversion_factor(c("minute", "day"), c("second", "hour")), c(60, 24))
    expect_equal(get_conversion_factor(c("g/L", "kg/L"), c("mg/L", "g/L")), c(1000, 1000))
  })

  it("returns 1 for same units even if they are non-convertible", {
    expect_equal(get_conversion_factor("kg", "kg"), 1)
    expect_equal(get_conversion_factor("fakeunit_in_kilos", "fakeunit_in_kilos"), 1)
  })
})

describe("convert_to_iso8601_duration", {
  it("converts valid durations to ISO 8601 format", {
    expect_equal(convert_to_iso8601_duration(5, "d"), "P5D")
    expect_equal(convert_to_iso8601_duration(200, "h"), "PT200H")
    expect_equal(convert_to_iso8601_duration(30, "min"), "PT30M")
    expect_equal(convert_to_iso8601_duration(45, "s"), "PT45S")
  })

  it("handles Inf and NA values", {
    expect_equal(convert_to_iso8601_duration(Inf, "d"), "PinfD")
    expect_equal(convert_to_iso8601_duration(NA, "d"), NA)
  })

  it("handles unsupported units gracefully", {
    expect_error(
      convert_to_iso8601_duration(10, "unsupported"),
      "Unsupported unit. Accepted units start with 'y', 'm', 'w', 'd', 'h', or 's'."
    )
  })

  it("handles edge cases for valid units", {
    expect_equal(convert_to_iso8601_duration(1, "y"), "P1Y")
    expect_equal(convert_to_iso8601_duration(2, "month"), "P2M")
    expect_equal(convert_to_iso8601_duration(3, "w"), "P3W")
  })

  it("handles invalid input types", {
    expect_error(
      convert_to_iso8601_duration("five", "d"),
      "'value' must be a numeric."
    )
    expect_error(
      convert_to_iso8601_duration(5, 123),
      "'unit' must be a character string."
    )
  })

  it("handles vectorized inputs correctly", {
    values <- c(5, 10, 15)
    units <- c("d", "h", "min")
    expected_output <- c("P5D", "PT10H", "PT15M")
    result <- convert_to_iso8601_duration(values, units)
    expect_equal(result, expected_output)
  })

  it("handles mixed valid and invalid vectorized inputs", {
    values <- c(5, 10, "invalid")
    units <- c("d", "unsupported", "h")
    expect_error(
      convert_to_iso8601_duration(values, units),
      "'value' must be a numeric."
    )

    values <- c(5, 10, 15)
    units <- c(1, 12, 123)
    expect_error(
      convert_to_iso8601_duration(values, units),
      "'unit' must be a character string."
    )

    values <- c(5, 10, 20)
    units <- c("d", "h", "tt")
    expect_error(
      convert_to_iso8601_duration(values, units),
      "Unsupported unit. Accepted units start with 'y', 'm', 'w', 'd', 'h', or 's'."
    )
  })
})

describe("convert_volume_units()", {

  it("should convert L to mL when AVALU has denominator mL", {
    df <- data.frame(
      PCSPEC = c("urine"),
      AVAL = c(100),
      AVALU = c("ug/mL"),
      VOLUME = c(1),
      VOLUMEU = c("L"),
      stringsAsFactors = FALSE
    )
    result <- convert_volume_units(df)
    expect_equal(result$VOLUME, 1000)
    expect_equal(result$VOLUMEU, "mL")
    expect_equal(result$AMOUNTU, "ug")
  })

  it("should apply fallback conversion when direct conversion is not possible", {
    df <- data.frame(
      PCSPEC = c("feces"),
      AVAL = c(5),
      AVALU = c("mg/g"),
      VOLUME = c(2),
      VOLUMEU = c("mL"),
      stringsAsFactors = FALSE
    )
    result <- convert_volume_units(df)
    expect_equal(result$VOLUMEU, "g")
    expect_equal(result$AMOUNTU, "mg")
  })

  it("should skip rows with NA values", {
    df <- data.frame(
      PCSPEC = c("urine"),
      AVAL = c(1),
      AVALU = c("ng/mL"),
      VOLUME = c(NA),
      VOLUMEU = c(NA),
      stringsAsFactors = FALSE
    )
    result <- convert_volume_units(df)
    expect_true(is.na(result$AMOUNTU))
  })

  it("should handle invalid AVALU formats", {
    df <- data.frame(
      PCSPEC = c("urine"),
      AVAL = c(100),
      AVALU = c("ug"),  # invalid format
      VOLUME = c(1),
      VOLUMEU = c("L"),
      stringsAsFactors = FALSE
    )
    result <- convert_volume_units(df)
    expect_true(result$AMOUNTU[1] == "L ug")
  })

  it("should handle multiple rows correctly", {
    df <- data.frame(
      PCSPEC = c("urine", "feces", "plasma"),
      AVAL = c(100, 10, 50),
      AVALU = c("ug/mL", "mg/g", "ng/mL"),
      VOLUME = c(1, 0.5, 1),
      VOLUMEU = c("L", "mL", "mL"),
      stringsAsFactors = FALSE
    )
    result <- convert_volume_units(df)
    expect_equal(result$VOLUMEU[1], "mL")  # Converted from L to mL
    expect_equal(result$VOLUMEU[2], "g")   # Fallback conversion
    expect_equal(result$VOLUMEU[3], "mL")  # Unchanged
  })

  it("should return original dataframe if required columns are missing", {
    df <- data.frame(
      PCSPEC = c("urine"),
      AVAL = c(100),
      stringsAsFactors = FALSE
    )
    result <- convert_volume_units(df)
    expect_equal(result, df)  # No conversion applied
  })

  it("returns NA if conversion is not possible", {
    df <- data.frame(
      PCSPEC = c("urine"),
      AVAL = c(100),
      AVALU = c("ug/mL"),
      VOLUME = c(1),
      VOLUMEU = c("unknown_unit"),  # Invalid unit
      stringsAsFactors = FALSE
    )

    expect_warning(
      result <- convert_volume_units(df),
      "Row 1: Failed to convert 1 unknown_unit to mL"
    )
    expect_true(is.na(result$AMOUNTU))

  })
})

describe("simplify_unit", {
  it("simplifies a compound unit expression correctly", {
    u <- units::set_units(1, "L*g/mg", mode = "standard")
    result <- simplify_unit(u)
    expect_true(inherits(result, "units"))
    expect_equal(units::deparse_unit(result), "L")
    expect_equal(as.numeric(result), 1000)
  })

  it("preserves the numeric value of the units object", {
    u <- units::set_units(5, "mg*L/(mg*L/mL)", mode = "standard")
    result <- simplify_unit(u)
    expect_equal(units::deparse_unit(result), "mL")
    expect_equal(as.numeric(result), 5)
  })

  it("handles simple units", {
    u <- units::set_units(2, "mg", mode = "standard")
    result <- simplify_unit(u)
    expect_equal(units::deparse_unit(result), "mg")
    expect_equal(as.numeric(result), 2)
  })

  it("handles units with powers", {
    u <- units::set_units(1, "(dg^2*L)/g^3", mode = "standard")
    result <- simplify_unit(u)
    expect_equal(units::deparse_unit(result), "L g-1")
    expect_equal(as.numeric(result), 0.01)
  })

  it("handles character inputs", {
    u <- "(mg*L)/(mL)"
    result <- simplify_unit(u)
    expect_equal(units::deparse_unit(result), "mg")
    expect_equal(as.numeric(result), 1000)
  })

  it("returns an error for invalid inputs", {
    expect_error(simplify_unit(list(a = 1)),
                 "Input must be a valid units object or character string")
  })

  it("returns only the unit as character when as_character = TRUE", {
    u <- units::set_units(2, "mg", mode = "standard")
    result <- simplify_unit(u, as_character = TRUE)
    expect_equal(result, "mg")
  })

  it("returns NA outputs for NA inputs", {
    expect_equal(simplify_unit(NA), NA_real_)
    expect_equal(simplify_unit(NA, as_character = TRUE), NA_character_)
  })
  
  it("handles unitless inputs", {
    expect_equal(units::deparse_unit(simplify_unit("unitless")), "unitless")
    expect_equal(simplify_unit("unitless", as_character = TRUE), "unitless")
  })
})
