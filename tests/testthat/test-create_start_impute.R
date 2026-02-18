describe("create_start_impute", {

  # Notes: This function is enforced to assume that DOSNOA was created separating intervals
  # in order to work properly
  pknca_data <- FIXTURE_PKNCA_DATA
  pknca_data$conc$data <- pknca_data$conc$data

  it("runs without issues", {
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
      dplyr::filter(USUBJID == 1, DOSNOA == 1, type_interval == "main") %>%
      dplyr::pull(impute)
    expect_equal(not_imputed, NA_character_)
  })

  it("sets conc0 when route is extravascular (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 2, DOSNOA == 1,  type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_conc0")
  })

  it("sets predose when route is extravascular (later dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 2, DOSNOA == 2, type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_predose")
  })

  it("sets logslope when route is IV bolus (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 3, DOSNOA == 1, type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_logslope")
  })

  it("sets logslope when route is IV bolus (later dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 3, DOSNOA == 2, type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_logslope")
  })

  it("sets c1 when route is IV bolus not monodecaying (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 4, DOSNOA == 1, type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_c1")
  })

  it("sets conc0 when route is IV bolus not monodecaying (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 5, DOSNOA == 1, type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_conc0")
  })

  it("sets conc0 when route is IV bolus not monodecaying (first dose)", {
    expect_equal(result$intervals %>%
                   dplyr::filter(USUBJID == 6, DOSNOA == 1, type_interval == "main") %>%
                   dplyr::pull(impute),
                 "start_conc0")
  })

  it("if METABFL column is not present, imputes assuming no metabolite data", {
    # No drug but there is analyte
    mydata_with_analyte <- pknca_data
    mydata_with_analyte$conc$data$METABFL <- NULL
    result_with_analyte <- create_start_impute(mydata_with_analyte)
    result_with_analyte_impute <- result_with_analyte$intervals %>%
      dplyr::filter(USUBJID == 6, DOSNOA == 1) %>%
      dplyr::pull(impute)
    expect_equal(unique(result_with_analyte_impute), "start_logslope")
  })

  it("if input intervals are ambiguous, specify them and do appropiate start_impute", {
    # Create ambiguous intervals (not separated by group)
    mydata_ambig <- pknca_data
    mydata_ambig$intervals <- mydata_ambig$intervals %>%
      select(-any_of(group_vars(pknca_data))) %>%
      unique()
    result_ambig <- create_start_impute(mydata_ambig)
    expect_true(nrow(setdiff(result$intervals, result_ambig$intervals)) == 0)
  })
})
