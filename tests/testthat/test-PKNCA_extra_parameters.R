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
    pk.calc.ermax(conc = c(NA_real_, NA_real_), volume = c(1, 1), time = c(0, 1), duration.conc = c(1, 1)),
    structure(NA_real_, exclude = "All concentrations are missing")
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
    pk.calc.ertmax(conc = c(NA_real_, 0), volume = c(1, 1), time = c(0, 1), duration.conc = c(1, 1)),
    structure(NA_real_, exclude = "1 of 2 concentrations are missing")
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

###################################################################

describe("register_interval_col_if_absent", {
  it("runs the registration callback for a column PKNCA does not define", {
    # A name PKNCA will never define; the callback records that it ran instead
    # of registering, so the global interval-column table is left untouched.
    absent_name <- "anca_definitely_absent_interval_col"
    skip_if(absent_name %in% names(PKNCA::get.interval.cols()))
    register_ran <- FALSE
    ran <- register_interval_col_if_absent(absent_name, function() {
      register_ran <<- TRUE
    })
    expect_true(ran)
    expect_true(register_ran)
  })

  it("skips the registration callback when PKNCA already defines the column", {
    # `ae` is provided by every supported PKNCA version, so aNCA must defer to
    # PKNCA's definition rather than overwrite it.
    skip_if_not("ae" %in% names(PKNCA::get.interval.cols()))
    register_ran <- FALSE
    ran <- register_interval_col_if_absent("ae", function() {
      register_ran <<- TRUE
    })
    expect_false(ran)
    expect_false(register_ran)
  })

  it("registers the excretion columns exactly once, without overwriting PKNCA", {
    # Whichever source defines them, ertlst/ertmax/volpk/fe must be available.
    for (nm in c("ertlst", "ertmax", "volpk", "fe")) {
      expect_true(nm %in% names(PKNCA::get.interval.cols()), info = nm)
    }
  })
})
