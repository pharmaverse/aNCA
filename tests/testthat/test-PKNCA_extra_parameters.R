test_that("pk.calc.volpk", {
  expect_equal(pk.calc.volpk(c(1, 2, 3)), 6)
  expect_equal(pk.calc.volpk(c(1, NA, 3)), 4)
  expect_equal(pk.calc.volpk(NA), 0)
  expect_equal(pk.calc.volpk(numeric()), 0)
})
