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
})

describe("convert_to_iso8601_duration", {
  it("converts valid durations to ISO 8601 format", {
    expect_equal(convert_to_iso8601_duration(5, "d"), "P5D")
    expect_equal(convert_to_iso8601_duration(200, "h"), "PT200H")
    expect_equal(convert_to_iso8601_duration(30, "min"), "PT30M")
    expect_equal(convert_to_iso8601_duration(45, "s"), "PT45S")
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
      "The value must be numeric."
    )
    expect_error(
      convert_to_iso8601_duration(5, 123),
      "The unit must be a character string."
    )
  })
})
