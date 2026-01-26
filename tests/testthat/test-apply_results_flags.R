describe("apply_results_flags", {

  # Prepare inputs using FIXTURE data
  pknca_res <- FIXTURE_PKNCA_RES
  pivoted_res <- pivot_wider_pknca_results(pknca_res)

  # Define test flag settings
  flag_settings <- list(
    "R2ADJ" = list(is.checked = TRUE, threshold = 0.8),
    "LAMZSPN" = list(is.checked = TRUE, threshold = 1)
  )

  group_cols <- unname(unlist(pknca_res$data$conc$columns$groups))

  it("assigns ACCEPTED status when no thresholds are breached", {
    # USUBJID 2 meets criteria
    test_data <- pivoted_res %>% filter(USUBJID == 2)

    result <- apply_results_flags(test_data,
                                  pknca_res$data$intervals,
                                  group_cols,
                                  flag_settings)

    expect_equal(result$flagged[1], "ACCEPTED")
  })

  it("assigns FLAGGED status when a threshold is breached", {
    # Force a breach (R2 < 0.99)
    test_data <- pivoted_res %>%
      filter(USUBJID == 2) %>%
      mutate(Exclude = "R2ADJ < 0.99")
    custom_settings <- list(
      "R2ADJ" = list(is.checked = TRUE, threshold = 0.99)
    )

    result <- apply_results_flags(test_data,
                                  pknca_res$data$intervals,
                                  group_cols,
                                  custom_settings)

    expect_equal(result$flagged[1], "FLAGGED")
  })

  it("identifies MISSING parameters that were requested in intervals", {

    test_data <- pivoted_res %>% filter(USUBJID == 1, ATPTREF == 1) #LMZSPN missing

    result <- apply_results_flags(test_data,
                                  pknca_res$data$intervals,
                                  group_cols,
                                  flag_settings)

    expect_equal(result$flagged[1], "MISSING")
  })

  it("concatenates existing Exclude reasons with new NA messages", {
    # exclude col already has reason in data
    test_data <- pivoted_res %>% filter(USUBJID == 1, ATPTREF == 1)

    # Create intervals
    mod_intervals <- pknca_res$data$intervals
    mod_intervals$adj.r.squared <- TRUE

    result <- apply_results_flags(test_data,
                                  mod_intervals,
                                  group_cols,
                                  flag_settings)

    expect_match(result$Exclude[1],
                 "Too few points for half-life calculation (min.hl.points=3 with only 0 points); R2ADJ is NA", #nolint
                 fixed = TRUE)
  })

  it("flags NOT DONE if no flags are requested", {
    # Define test flag settings
    false_settings <- list(
      "R2" = list(is.checked = FALSE, threshold = 0.8),
      "LAMZSPN" = list(is.checked = FALSE, threshold = 3),
      "AUCPEP" = list(is.checked = FALSE, threshold = 20)
    )

    result <- apply_results_flags(pivoted_res,
                                  pknca_res$data$intervals,
                                  group_cols,
                                  false_settings)

    expect_true(all(result$flagged == "NOT DONE"))
  })

  it("does not set to FLAGGED if parameter was not requested", {
    test_data <- pivoted_res %>% filter(USUBJID == 2, ATPTREF == 1)
    # R2 is 0.97

    # Create intervals where R2ADJ is FALSE
    mod_intervals <- pknca_res$data$intervals
    mod_intervals$adj.r.squared <- FALSE

    # Setting R2 as a checked flag
    flag_settings <- list("R2ADJ" = list(is.checked = TRUE, threshold = 0.99))

    result <- apply_results_flags(test_data, mod_intervals, group_cols, flag_settings)

    # Should be ACCEPTED because isTRUE(get(f_col)) will be FALSE for R2
    expect_equal(result$flagged[1], "ACCEPTED")
    expect_false(grepl("R2ADJ < 0.99", result$Exclude[1]))
  })
})
