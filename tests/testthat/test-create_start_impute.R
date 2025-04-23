# Create concentration, dose and interval datasets with all imputation cases
conc_data <- data.frame(
  conc = c(1:3, c(1, 2, 0.5), 1:3, 6:4, 6:4, rep(2, 3), 1:3, 6:4),
  time = c(0:2, c(1, 2, 2.5), 4:6, c(1, 2, 2.5), 4:6, c(1, 2, 2.5), c(1, 2, 2.5), c(1, 2, 2.5)),
  route = c(rep("extravascular", 9), rep("intravascular", 15)),
  param = c(rep("A", 21), rep("B", 3)),
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
  DOSNO = c(1, c(1, 2), c(1, 2), 1, 1, 1)
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

conc_obj <- PKNCA::PKNCAconc(conc_data, conc ~ time | USUBJID + DOSNO / param)
dose_obj <- PKNCA::PKNCAdose(dose_data, dose ~ time | USUBJID + DOSNO,
                             duration = "duration", route = "route")
mydata <- PKNCA::PKNCAdata(conc_obj, dose_obj, intervals = intervals_data)

# Apply the function
describe("create_start_impute", {
  it("works without issue", {
    expect_no_error({
      result <- create_start_impute(mydata)
    })
  })

  result <- create_start_impute(mydata)

  it("provides a warning when data$intervals is empty or has no rows", {
    mydata_noints <- mydata
    mydata_noints$intervals <- data.frame()
    mydata_noints_res <- suppressWarnings(create_start_impute(mydata_noints))
    expect_warning(create_start_impute(mydata_noints), "No intervals provided. No modification")
  })

  it("does not add impute (NA) when start is in PKNCAconc", {
    not_imputed <- result$intervals %>%
      dplyr::filter(USUBJID == 1, DOSNO == 1) %>%
      dplyr::pull(impute)
    expect_equal(not_imputed, NA_character_)
  })

  it("sets conc0 when route is extravascular (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 2, DOSNO == 1) %>%
                   dplyr::pull(impute),
                 "start_conc0")
  })

  it("sets predose when route is extravascular (later dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 2, DOSNO == 2) %>%
                   dplyr::pull(impute),
                 "start_predose")
  })

  it("sets logslope when route is IV bolus (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 3, DOSNO == 1) %>%
                   dplyr::pull(impute),
                 "start_logslope")
  })

  it("sets logslope when route is IV bolus (later dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 3, DOSNO == 2) %>%
                   dplyr::pull(impute),
                 "start_logslope")
  })

  it("sets c1 when route is IV bolus not monodecaying (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 4, DOSNO == 1) %>%
                   dplyr::pull(impute),
                 "start_c1")
  })

  it("sets conc0 when route is IV bolus not monodecaying (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 5, DOSNO == 1) %>%
                   dplyr::pull(impute),
                 "start_conc0")
  })

  it("sets conc0 when route is IV bolus not monodecaying (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 6, DOSNO == 1) %>%
                   dplyr::pull(impute),
                 "start_conc0")
  })
})
