test_that("pk.calc.ertlst", {
  # All NA
  expect_equal(
    pk.calc.ertlst(conc = c(NA, NA), volume = c(1, 1), time = c(0, 1), duration.conc = c(1, 1)),
    structure(NA_real_, exclude = "All concentrations are missing")
  )
  expect_equal(
    pk.calc.ertlst(conc = c(NA, NA), volume = c(NA, NA), time = c(0, 1), duration.conc = c(1, 1)),
    structure(NA_real_, exclude = "All concentrations and volumes are missing")
  )
  # All 0 or NA
  expect_equal(
    pk.calc.ertlst(conc = c(0, NA), volume = c(1, 1), time = c(0, 1), duration.conc = c(1, 1)),
    structure(0, exclude = "1 of 2 concentrations are missing")
  )
  # Normal case
  expect_equal(
    pk.calc.ertlst(conc = c(1, 2, 0), volume = c(1, 1, 1), time = c(0, 1, 2), duration.conc = c(1, 1, 1)),
    max(c(0, 1) + 1/2)
  )
})

test_that("pk.calc.ermax", {
  # All NA
  expect_equal(
    pk.calc.ermax(conc = c(NA, NA), volume = c(1, 1), time = c(0, 1), duration.conc = c(1, 1)),
    structure(NA, exclude = "All concentrations are missing")
  )
  # Normal case
  expect_equal(
    pk.calc.ermax(conc = c(1, 2, 3), volume = c(2, 2, 2), time = c(0, 1, 2), duration.conc = c(2, 2, 2)),
    max(c(1, 2, 3) * 2 / 2)
  )
})

test_that("pk.calc.ertmax", {
  # All NA or 0
  expect_equal(
    pk.calc.ertmax(conc = c(NA, 0), volume = c(1, 1), time = c(0, 1), duration.conc = c(1, 1)),
    structure(NA, exclude = "1 of 2 concentrations are missing")
  )
  # Normal case, last tmax
  expect_equal(
    pk.calc.ertmax(conc = c(1, 3, 2), volume = c(2, 2, 2), time = c(0, 1, 2), duration.conc = c(2, 2, 2), first.tmax = FALSE),
    (1 + 2/2)
  )
  # Normal case, first tmax
  expect_equal(
    pk.calc.ertmax(conc = c(1, 3, 2), volume = c(2, 2, 2), time = c(0, 1, 2), duration.conc = c(2, 2, 2), first.tmax = TRUE),
    (1 + 2/2)
  )
  # Multiple maxima
  expect_equal(
    pk.calc.ertmax(conc = c(1, 3, 3), volume = c(2, 2, 2), time = c(0, 1, 2), duration.conc = c(2, 2, 2), first.tmax = TRUE),
    (1 + 2/2)
  )
  expect_equal(
    pk.calc.ertmax(conc = c(1, 3, 3), volume = c(2, 2, 2), time = c(0, 1, 2), duration.conc = c(2, 2, 2), first.tmax = FALSE),
    (2 + 2/2)
  )
})

test_that("generate_missing_messages", {
  # Ensure that the deparse(substitute()) methods work
  conc <- NA_real_
  volume <- NA_real_
  expect_equal(
    as.character(generate_missing_messages(conc, volume)),
    "All conc and volume are missing"
  )
})

###################################################################

test_that("pk.calc.volpk", {
  expect_equal(pk.calc.volpk(c(1, 2, 3)), 6)
  expect_equal(pk.calc.volpk(c(1, NA, 3)), NA_real_)
  expect_equal(pk.calc.volpk(NA), NA_real_)
  expect_equal(pk.calc.volpk(numeric()), NA_real_)
})
