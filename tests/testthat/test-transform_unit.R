library(testthat)
library(units)

test_that("transform_unit handles simple time units", {
  expect_equal(transform_unit("hr", "minute"), 60)
  expect_equal(transform_unit("minute", "second"), 60)
  expect_equal(transform_unit("day", "hour"), 24)
  expect_equal(transform_unit("second", "minute"), 1/60)
  expect_equal(transform_unit("hour", "second"), 3600)
  expect_equal(transform_unit("minute", "hour"), 1/60)
  expect_equal(transform_unit("day", "minute"), 1440)
})

test_that("transform_unit handles simple concentration units", {
  expect_equal(transform_unit("mg/L", "g/L"), 0.001)
  expect_equal(transform_unit("g/dL", "kg/L"), 0.01)
  expect_equal(transform_unit("ug/mL", "mg/L"), 1)
  expect_equal(transform_unit("kg/L", "g/L"), 1000)
  expect_equal(transform_unit("mg/dL", "g/L"), 0.01)
  expect_equal(transform_unit("g/L", "mg/L"), 1000)
  expect_equal(transform_unit("kg/L", "mg/L"), 1e6)
})

test_that("transform_unit handles combined units", {
  expect_equal(transform_unit("Hours*ug/mL", "Hours*mg/L"), 1)
  expect_equal(transform_unit("Hours^2*ug/mL", "Hours^2*mg/L"), 1)
  expect_equal(transform_unit("(Hours*ug/mL)/mg", "(Hours*mg/L)/g"), 1000)
  expect_equal(transform_unit("mg/(Hours*ug/mL)", "g/(Hours*mg/L)"), 0.001)
  expect_equal(transform_unit("(ug/mL)/(Hours*ug/mL)", "(mg/L)/(Hours*mg/L)"), 1)
  expect_equal(transform_unit("Hours*mg/L", "Hours*ug/mL"), 1)
  expect_equal(transform_unit("Hours^2*mg/L", "Hours^2*ug/mL"), 1)
  expect_equal(transform_unit("Hours*kg/L", "Hours*g/L"), 1000)
  expect_equal(transform_unit("Hours*mg/L", "Hours*kg/L"), 1e-6)
})

test_that("transform_unit returns NA for non-convertible units", {
  expect_true(is.na(transform_unit("meter", "second")))
  expect_true(is.na(transform_unit("kg", "hour")))
  expect_true(is.na(transform_unit("liter", "gram")))
  expect_true(is.na(transform_unit("meter", "liter")))
  expect_true(is.na(transform_unit("second", "gram")))
  expect_true(is.na(transform_unit("hour", "meter")))
})

test_that("transform_unit handles vector inputs", {
  expect_equal(transform_unit(c("hr", "minute"), c("minute", "second")), c(60, 60))
  expect_equal(transform_unit(c("day", "mg/L"), c("hour", "g/L")), c(24, 0.001))
  expect_equal(transform_unit(c("second", "kg/L"), c("minute", "g/L")), c(1/60, 1000))
  expect_equal(transform_unit(c("hour", "ug/mL"), c("second", "mg/L")), c(3600, 1))
  expect_equal(transform_unit(c("minute", "day"), c("second", "hour")), c(60, 24))
  expect_equal(transform_unit(c("g/L", "kg/L"), c("mg/L", "g/L")), c(1000, 1000))
})
