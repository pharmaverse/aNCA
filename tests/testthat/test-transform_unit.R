test_that("get_conversion_factor handles simple time units", {
  expect_equal(get_conversion_factor("hr", "minute"), 60)
  expect_equal(get_conversion_factor("minute", "second"), 60)
  expect_equal(get_conversion_factor("day", "hour"), 24)
  expect_equal(get_conversion_factor("second", "minute"), 1 / 60)
  expect_equal(get_conversion_factor("hour", "second"), 3600)
  expect_equal(get_conversion_factor("minute", "hour"), 1 / 60)
  expect_equal(get_conversion_factor("day", "minute"), 1440)
})

test_that("get_conversion_factor handles simple concentration units", {
  expect_equal(get_conversion_factor("mg/L", "g/L"), 0.001)
  expect_equal(get_conversion_factor("g/dL", "kg/L"), 0.01)
  expect_equal(get_conversion_factor("ug/mL", "mg/L"), 1)
  expect_equal(get_conversion_factor("kg/L", "g/L"), 1000)
  expect_equal(get_conversion_factor("mg/dL", "g/L"), 0.01)
  expect_equal(get_conversion_factor("g/L", "mg/L"), 1000)
  expect_equal(get_conversion_factor("kg/L", "mg/L"), 1e6)
})

test_that("get_conversion_factor handles combined units", {
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

test_that("get_conversion_factor returns NA for non-convertible units", {
  expect_true(is.na(get_conversion_factor("meter", "second")))
  expect_true(is.na(get_conversion_factor("kg", "hour")))
  expect_true(is.na(get_conversion_factor("liter", "gram")))
  expect_true(is.na(get_conversion_factor("meter", "liter")))
  expect_true(is.na(get_conversion_factor("second", "gram")))
  expect_true(is.na(get_conversion_factor("hour", "meter")))
})

test_that("get_conversion_factor handles vector inputs", {
  expect_equal(get_conversion_factor(c("hr", "minute"), c("minute", "second")), c(60, 60))
  expect_equal(get_conversion_factor(c("day", "mg/L"), c("hour", "g/L")), c(24, 0.001))
  expect_equal(get_conversion_factor(c("second", "kg/L"), c("minute", "g/L")), c(1 / 60, 1000))
  expect_equal(get_conversion_factor(c("hour", "ug/mL"), c("second", "mg/L")), c(3600, 1))
  expect_equal(get_conversion_factor(c("minute", "day"), c("second", "hour")), c(60, 24))
  expect_equal(get_conversion_factor(c("g/L", "kg/L"), c("mg/L", "g/L")), c(1000, 1000))
})
