describe("lambda_slope_plot", {

  conc_pknca_df <- FIXTURE_PKNCA_DATA$conc$data %>%
    # ToDo: The currerent lambda_slope_plot
    # has additional non-neccesary assumptions
    mutate(TIME = AFRLT,
           PCSTRESU = AVALU)

  myres <- FIXTURE_PKNCA_RES
  myres$result <- myres$result %>%
    mutate(PPTESTCD = translate_terms(
      PPTESTCD, "PPTESTCD", "PKNCA"
    ))

  row_values <- myres$data$intervals %>%
    filter(half.life) %>%
    select(any_of(c(
      unname(unlist(myres$data$conc$columns$groups)),
      "AVISIT", "DOSNOA"
    ))) %>%
    filter(USUBJID == 5)

  it("returns a plotly object with valid input", {
    plotly_output <- lambda_slope_plot(
      conc_pknca_df = conc_pknca_df,
      row_values = row_values,
      myres = myres,
      r2adj_threshold = 0.7
    )

    expect_s3_class(plotly_output, "plotly")
    expect_true("layout" %in% names(plotly_output$x))
  })

  it("handles NA in lambda.z.n.points gracefully", {
    myres_mod <- myres

    # Modify lambda.z.n.points to NA for the target row
    myres_mod$result <- myres_mod$result %>%
      mutate(
        PPSTRES = ifelse(
          PPTESTCD == "lambda.z.n.points" &
            USUBJID == row_values$USUBJID,
          NA_real_,
          PPSTRES
        )
      )

    plotly_output <- lambda_slope_plot(
      conc_pknca_df = conc_pknca_df,
      row_values = row_values,
      myres = myres_mod
    )

    expect_s3_class(plotly_output, "plotly")
  })

  it("warns and returns empty plot when AVAL <= 0", {
    conc_modified <- conc_pknca_df
    conc_modified$AVAL <- -1

    expect_warning({
      empty_plot <- lambda_slope_plot(
        conc_pknca_df = conc_modified,
        row_values = row_values,
        myres = myres
      )
      expect_s3_class(empty_plot, "plotly")
    }, "Not enough data for plotting")
  })

  it("returns without error and gives expected warning when plot_data has 0 rows", {
    conc_modified <- conc_pknca_df %>%
      mutate(
        AVAL = ifelse(
          USUBJID == row_values$USUBJID,
          -1,
          AVAL
        )
      )

    expect_warning(
      lambda_slope_plot(
        conc_pknca_df = conc_modified,
        row_values = row_values,
        myres = myres
      ),
      "Not enough data for plotting"
    )
  })

  it("shows warning when Cmax is included in lambda estimation", {
    # Copy inputs
    conc_modified <- conc_pknca_df
    myres_modified <- myres

    # Use the same subject for consistency
    test_id  <- myres$data$intervals %>%
      filter(half.life) %>%
      select(any_of(c(
        unname(unlist(myres$data$conc$columns$groups)),
        "AVISIT", "DOSNOA"
      ))) %>%
      filter(USUBJID == 5)

    row_values <- test_id %>% as.list()

    # Get the corresponding Tmax value
    tmax_value <- myres_modified$result %>%
      filter(PPTESTCD == "tmax") %>%
      filter(
        USUBJID == row_values$USUBJID
      ) %>%
      pull(PPSTRES)

    # Force lambda.z.time.first to be *equal* to tmax, triggering Cmax inclusion
    myres_modified$result <- myres_modified$result %>%
      mutate(
        PPSTRES = ifelse(
          PPTESTCD == "lambda.z.time.first" &
            USUBJID == row_values$USUBJID,
          tmax_value,
          PPSTRES
        )
      )

    # Run plot function
    plotly_output <- lambda_slope_plot(
      conc_pknca_df = conc_modified,
      row_values = row_values,
      myres = myres_modified
    )

    # Check for Cmax warning text
    annotations <- plotly_output$x$layout$annotations
    cmax_warn <- any(sapply(annotations, function(a) {
      is.list(a) && !is.null(a$text) &&
        grepl("Cmax should not be included in lambda calculation", a$text)
    }))

    expect_true(cmax_warn)
  })

})
