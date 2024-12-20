test_that("PKNCA_impute_method_start_log", {
  # No imputation when start is in the data
  expect_equal(
    PKNCA_impute_method_start_log(conc = 3:1, time = 0:2, start = 0, end = 2),
    data.frame(conc = 3:1, time = 0:2)
  )
  # Impute when start is not in the data
  expect_equal(
    PKNCA_impute_method_start_log(conc = 3:1, time = 1:3, start = 0, end = 3),
    data.frame(conc = c(4.5, 3:1), time = 0:3),
    ignore_attr = TRUE
  )
  # Data outside the interval are ignored (before interval)
  expect_equal(
    PKNCA_impute_method_start_log(conc = c(0, 2:1), time = c(-1, 1:2), start = 0, end = 2),
    data.frame(conc = c(0, 4, 2:1), time = c(-1, 0, 1:2)),
    ignore_attr = TRUE
  )
  # No modification if no C1 -> C2 decline in samples
  expect_equal(
    PKNCA_impute_method_start_log(conc = c(1, 1, 1), time = 1:3, start = 0, end = 3),
    data.frame(conc = c(1, 1, 1), time = 1:3),
    ignore_attr = TRUE
  )
  # No modification if C1 = C2 in samples
  expect_equal(
    PKNCA_impute_method_start_log(conc = c(3,3,1), time = 1:3, start = 0, end = 3),
    data.frame(conc = c(3, 3, 1), time = 1:3),
    ignore_attr = TRUE
  )
})

test_that("PKNCA_impute_method_start_c1", {
  # No imputation when start is in the data
  expect_equal(
    PKNCA_impute_method_start_c1(conc = 1:3, time = 0:2, start = 0, end = 2),
    data.frame(conc = 1:3, time = 0:2)
  )
  # Impute when start is not in the data
  expect_equal(
    PKNCA_impute_method_start_c1(conc = 1:3, time = 1:3, start = 0, end = 3),
    data.frame(conc = c(1, 1:3), time = 0:3),
    ignore_attr = TRUE
  )
  # Data outside the interval are ignored (before interval)
  expect_equal(
    PKNCA_impute_method_start_c1(conc = 1:3, time = c(-1, 1:2), start = 0, end = 2),
    data.frame(conc = c(1, 2, 2:3), time = c(-1, 0, 1:2)),
    ignore_attr = TRUE
  )
})
