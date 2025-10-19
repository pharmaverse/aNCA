test_that("pk.calc.volpk", {
  expect_equal(pk.calc.volpk(c(1, 2, 3)), 6)
  expect_equal(pk.calc.volpk(c(1, NA, 3)), NA_real_)
  expect_equal(pk.calc.volpk(NA), NA_real_)
  expect_equal(pk.calc.volpk(numeric()), NA_real_)
})
