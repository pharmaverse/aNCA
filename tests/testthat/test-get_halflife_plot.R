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
      "NCA_PROFILE", "DOSNOA"
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
        "NCA_PROFILE", "DOSNOA"
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


test_that("get_halflife_plot returns a list of plotly objects with valid input", {
  # Use the fixture data as input
  pknca_data <- FIXTURE_PKNCA_DATA
  plots <- get_halflife_plot(pknca_data)
  expect_type(plots, "list")
  expect_true(length(plots) >= 1)
  expect_s3_class(plots[[1]], "plotly")
  expect_true("layout" %in% names(plots[[1]]$x))
})

test_that("get_halflife_plot warns and returns empty list when no groups present", {
  # Remove all lambda.z rows from result to simulate no groups
  pknca_data <- FIXTURE_PKNCA_DATA
  pknca_data$conc$data <- pknca_data$conc$data[0, ]
  plots <- get_halflife_plot(pknca_data)
  expect_type(plots, "list")
  expect_length(plots, 0)
})

# Marker color/shape tests for get_halflife_plot
test_that("get_halflife_plot: marker colors and shapes - no exclusion/inclusion", {
  pknca_data <- FIXTURE_PKNCA_DATA
  # Remove exclude/include columns if present, or set all to FALSE
  pknca_data$conc$data$exclude_half.life <- FALSE
  pknca_data$conc$data$include_half.life <- FALSE
  plots <- get_halflife_plot(pknca_data)
  expect_true(length(plots) >= 1)
  plot_data <- plots[[1]]$x$data[[2]]
  # All points should be black (no inclusion/exclusion)
  expect_true(all(plot_data$marker$color == "black"))
  # All points should be circles (no exclusion)
  expect_true(all(plot_data$marker$symbol == "circle"))
})

test_that("get_halflife_plot: marker colors and shapes - exclusion of a lambda.z point", {
  pknca_data <- FIXTURE_PKNCA_DATA
  pknca_data$intervals <- pknca_data$intervals[2,]

  # Exclude a point in the lambda.z calculation
  pknca_data_with_excl <- pknca_data
  pknca_data_with_excl$conc$data <- pknca_data$conc$data %>%
    mutate(
      exclude_half.life = ifelse(
        USUBJID == USUBJID[2] & AFRLT == 2.5,
        TRUE,
        FALSE
      )
    )
  plots <- get_halflife_plot(pknca_data)
  plots_with_excl <- get_halflife_plot(pknca_data_with_excl)

  plots_details <- plots[[1]]$x$data[[2]]$marker
  expected_plots_details <- list(
    color = c("red", "red", "green", "green", "green"),
    size = 15,
    symbol = c("circle", "circle", "circle", "circle", "circle"),
    line = list(color = "rgba(255,127,14,1)")
  )
  plots_with_excl_details <- plots_with_excl[[1]]$x$data[[2]]$marker
  expected_plots_details_with_excl <- list(
    color = c("red", "black", "green", "green", "green"),
    size = 15,
    symbol = c("x", "circle", "circle", "circle", "circle"),
    line = list(color = "rgba(255,127,14,1)")
  )


  expect_true(length(plots) >= 1)
  plot_data <- plots[[1]]$x$data[[2]]
  # At least one point should be red (excluded)
  expect_true(any(plot_data$marker$color == "red"))
  # At least one point should be an 'x' (excluded)
  expect_true(any(plot_data$marker$symbol == "x"))
})

test_that("get_halflife_plot: marker colors and shapes - inclusion of lambda.z points", {
  pknca_data <- FIXTURE_PKNCA_DATA
  # Mark first two points as included in lambda.z
  pknca_data$conc$data$exclude_half.life <- FALSE
  pknca_data$conc$data$include_half.life <- FALSE
  pknca_data$conc$data$include_half.life[1:2] <- TRUE
  plots <- get_halflife_plot(pknca_data)
  expect_true(length(plots) >= 1)
  plot_data <- plots[[1]]$x$data[[2]]
  # At least two points should be green (included in lambda.z)
  expect_true(sum(plot_data$marker$color == "green") >= 2)
  # Included points should be circles
  expect_true(all(plot_data$marker$symbol[plot_data$marker$color == "green"] == "circle"))
})

