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

  it("preserves intervals when no concentration data falls within the interval range", {
    # Narrow one subject's interval end so all samples are beyond it
    mydata_narrow <- pknca_data
    narrow_intervals <- mydata_narrow$intervals %>%
      dplyr::mutate(end = dplyr::if_else(
        USUBJID == 1 & DOSNOA == 1 & type_interval == "main",
        -1,  # end before any sample
        end
      ))
    mydata_narrow$intervals <- narrow_intervals

    result_narrow <- create_start_impute(mydata_narrow)

    # The interval must still exist, not be silently dropped
    expect_equal(
      nrow(dplyr::filter(result_narrow$intervals,
                         USUBJID == 1, DOSNOA == 1, type_interval == "main")),
      1
    )
    # Its impute should be NA since no conc data matched
    expect_equal(
      result_narrow$intervals %>%
        dplyr::filter(USUBJID == 1, DOSNOA == 1, type_interval == "main") %>%
        dplyr::pull(impute),
      NA_character_
    )
    # Other intervals should still have their correct impute values
    expect_equal(
      result_narrow$intervals %>%
        dplyr::filter(USUBJID == 2, DOSNOA == 1, type_interval == "main") %>%
        dplyr::pull(impute),
      "start_conc0"
    )
  })

  it("preserves all intervals when none have matching concentration data", {
    # Set all interval ends to before any sample time
    mydata_no_match <- pknca_data
    mydata_no_match$intervals <- mydata_no_match$intervals %>%
      dplyr::mutate(end = -1)
    original_nrow <- nrow(mydata_no_match$intervals)

    result_no_match <- create_start_impute(mydata_no_match)

    # All intervals must be preserved
    expect_equal(nrow(result_no_match$intervals), original_nrow)
    # All impute values should be NA
    expect_true(all(is.na(result_no_match$intervals$impute)))
    # impute column should be character, not logical
    expect_type(result_no_match$intervals$impute, "character")
  })
})
