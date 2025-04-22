pknca_data <- TEST_PKNCA_DATA

describe("create_start_impute", {
  it("works without issue", {
    expect_no_error({
      result <- create_start_impute(pknca_data)
    })
  })

  result <- create_start_impute(pknca_data)

  it("provides a warning when data$intervals is empty or has no rows", {
    mydata_noints <- pknca_data
    mydata_noints$intervals <- data.frame()
    mydata_noints_res <- suppressWarnings(create_start_impute(mydata_noints))
    expect_warning(create_start_impute(mydata_noints), "No intervals provided. No modification")
  })

  it("does not add impute (NA) when start is in PKNCAconc", {
    not_imputed <- result$intervals %>%
      dplyr::filter(USUBJID == 1, DOSNO == 1, type_interval == "main") %>%
      dplyr::pull(impute)
    expect_equal(not_imputed, NA_character_)
  })

  it("sets conc0 when route is extravascular (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 2, DOSNO == 1,  type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_conc0")
  })

  it("sets predose when route is extravascular (later dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 2, DOSNO == 2, type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_predose")
  })

  it("sets logslope when route is IV bolus (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 3, DOSNO == 1, type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_logslope")
  })

  it("sets logslope when route is IV bolus (later dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 3, DOSNO == 2, type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_logslope")
  })

  it("sets c1 when route is IV bolus not monodecaying (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 4, DOSNO == 1, type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_c1")
  })

  it("sets conc0 when route is IV bolus not monodecaying (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 5, DOSNO == 1, type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_conc0")
  })

  it("sets conc0 when route is IV bolus not monodecaying (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 6, DOSNO == 1, type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_conc0")
  })
})
