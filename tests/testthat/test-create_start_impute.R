library(testthat)
library(dplyr)
library(PKNCA)

# Create concentration, dose and interval datasets with all imputation cases
conc_data <- data.frame(
  conc = c(1:3, c(1, 2, 0.5), 1:3, 6:4, 6:4, rep(2, 3), 1:3, 6:4),
  time = c(0:2, c(1, 2, 2.5), 4:6, c(1, 2, 2.5), 4:6, c(1, 2, 2.5), c(1, 2, 2.5), c(1, 2, 2.5)),
  route = c(rep("extravascular", 9), rep("intravascular", 15)),
  analyte = c(rep("A", 21), rep("B", 3)),
  USUBJID = c(rep(1, 3), rep(2, 6), rep(3, 6), rep(4, 3), rep(5, 3), rep(6, 3)),
  DOSNO = c(rep(1, 3), rep(rep(1:2, each = 3), 2), rep(1, 3), rep(1, 3), rep(1, 3))
)

dose_data <- data.frame(
  time = c(0, 0, 3, 0, 3, 0, 0, 0),
  route = c(rep("extravascular", 3), rep("intravascular", 5)),
  dose = 1,
  DRUG = "A",
  duration = c(0, 0, 0, 0, 0, 0, 1, 0),
  USUBJID = c(1, rep(2, 2), rep(3, 2), 4, 5, 6),
  DOSNO = c(1, rep(c(1, 2), each = 2), 1, 1, 1)
)

intervals_data <- data.frame(
  start = c(0, 0, 3, 0, 3, 0, 0, 0),
  end =   c(3, 3, 6, 3, 6, 3, 3, 3),
  USUBJID =  c(1, rep(2, 2), rep(3, 2), 4, 5, 6),
  DOSNO = c(1, 1, 2, 1, 2, 1, 1, 1),  # Include second dose profile for USUBJID 6
  tmax = TRUE,
  auc.obs = TRUE,
  auc.pred = TRUE
)

conc_obj <- PKNCAconc(conc_data, conc ~ time | USUBJID + DOSNO / analyte)
dose_obj <- PKNCAdose(dose_data, dose ~ time | USUBJID + DOSNO,
                      duration = "duration", route = "route")
mydata <- PKNCAdata(conc_obj, dose_obj, intervals = intervals_data)

# Apply the function
result <- create_start_impute(mydata)


test_that("create_start_impute does not add impute (NA) when start is in PKNCAconc", {
  # Check the values in the impute column
  expect_equal(result$intervals %>%
                 dplyr::filter(USUBJID == 1, DOSNO == 1) %>%
                 dplyr::pull(impute),
               NA_character_)
})

test_that("create_start_impute: conc0 when route is extravascular (first dose)", {
  # Check the values in the impute column
  expect_equal(result$intervals %>%
                 dplyr::filter(USUBJID == 2, DOSNO == 1) %>%
                 dplyr::pull(impute),
               "start_conc0")
})

test_that("create_start_impute: predose when route is extravascular (later dose)", {
  # Check the values in the impute column
  expect_equal(result$intervals %>%
                 dplyr::filter(USUBJID == 2, DOSNO == 2) %>%
                 dplyr::pull(impute),
               "start_predose")
})

test_that("create_start_impute: logslope when route is IV bolus (first dose)", {
  # Check the values in the impute column
  expect_equal(result$intervals %>%
                 dplyr::filter(USUBJID == 3, DOSNO == 1) %>%
                 dplyr::pull(impute),
               "start_logslope")
})

test_that("create_start_impute: logslope when route is IV bolus (later dose)", {
  # Check the values in the impute column
  expect_equal(result$intervals %>%
                 dplyr::filter(USUBJID == 3, DOSNO == 2) %>%
                 dplyr::pull(impute),
               "start_logslope")
})

test_that("create_start_impute: c1 when route is IV bolus not monodecaying (first dose)", {
  # Check the values in the impute column
  expect_equal(result$intervals %>%
                 dplyr::filter(USUBJID == 4, DOSNO == 1) %>%
                 dplyr::pull(impute),
               "start_c1")
})

test_that("create_start_impute: conc0 when route is IV bolus not monodecaying (first dose)", {
  # Check the values in the impute column
  expect_equal(result$intervals %>%
                 dplyr::filter(USUBJID == 5, DOSNO == 1) %>%
                 dplyr::pull(impute),
               "start_conc0")
})

test_that("create_start_impute: conc0 when route is IV bolus not monodecaying (first dose)", {
  # Check the values in the impute column
  expect_equal(result$intervals %>%
                 dplyr::filter(USUBJID == 6, DOSNO == 1) %>%
                 dplyr::pull(impute),
               "start_conc0")
})
