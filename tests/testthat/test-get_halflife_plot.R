test_that("get_halflife_plot returns a list of plotly objects with valid input", {
  # Use the fixture data as input
  pknca_data <- FIXTURE_PKNCA_DATA
  plots <- get_halflife_plots(pknca_data)[["plots"]]
  expect_type(plots, "list")
  expect_true(length(plots) >= 1)
  expect_s3_class(plots[[1]], "plotly")
  expect_true("layout" %in% names(plots[[1]]$x))
})

test_that("get_halflife_plot warns and returns empty list when no groups present", {
  # Remove all lambda.z rows from result to simulate no groups
  pknca_data <- FIXTURE_PKNCA_DATA
  pknca_data$conc$data <- pknca_data$conc$data[0, ]
  plots <- get_halflife_plots(pknca_data)[["plots"]]
  expect_type(plots, "list")
  expect_length(plots, 0)
})

# Marker color/shape tests for get_halflife_plot
test_that("get_halflife_plot: marker colors and shapes - no exclusion/inclusion", {
  pknca_data <- FIXTURE_PKNCA_DATA
  # Remove exclude/include columns if present, or set all to FALSE
  pknca_data$conc$data$exclude_half.life <- FALSE
  pknca_data$conc$data$include_half.life <- FALSE
  plots <- get_halflife_plots(pknca_data)[["plots"]]
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
        USUBJID == unique(USUBJID)[2] & AFRLT == 2.5,
        TRUE,
        FALSE
      )
    )
  plots <- get_halflife_plots(pknca_data)[["plots"]]
  plots_with_excl <- get_halflife_plots(pknca_data_with_excl)[["plots"]]

  plots_details <- plots[[1]]$x$data[[2]]$marker
  expected_plots_details <- list(
    color = c("red", "red", "green", "green", "green"),
    size = 15,
    symbol = c("circle", "circle", "circle", "circle", "circle"),
    line = list(color = "rgba(255,127,14,1)")
  )
  plots_with_excl_details <- plots_with_excl[[1]]$x$data[[2]]$marker
  expected_plots_details_with_excl <- list(
    color = c("red", "red", "red", "green", "green"),
    size = 15,
    symbol = c("circle", "circle", "x", "circle", "circle"),
    line = list(color = "rgba(255,127,14,1)")
  )

  expect_equal(plots_details, expected_plots_details, ignore_attr = TRUE)
  expect_equal(plots_with_excl_details, expected_plots_details_with_excl, ignore_attr = TRUE)
})

test_that("get_halflife_plot: marker colors and shapes - inclusion of lambda.z points", {
  pknca_data <- FIXTURE_PKNCA_DATA
  pknca_data$intervals <- pknca_data$intervals[3,]
  
  # Mark first two points as included in lambda.z
  pknca_data$conc$data$exclude_half.life <- FALSE
  pknca_data$conc$data$include_half.life <- NA
  pknca_data_with_incl <- pknca_data
  pknca_data_with_incl$conc$data <- pknca_data$conc$data %>%
    mutate(
      include_half.life = ifelse(
        USUBJID == unique(USUBJID)[3] & AFRLT >= 0.5,
        TRUE,
        FALSE
      )
    )
  plots <- get_halflife_plots(pknca_data)[["plots"]]
  plots_with_incl <- get_halflife_plots(pknca_data_with_incl)[["plots"]]
  
  plots_details <- plots[[1]]$x$data[[2]]$marker
  expected_plots_details <- list(
    color = c("red", "red", "green", "green", "green"),
    size = 15,
    symbol = c("circle", "circle", "circle", "circle", "circle"),
    line = list(color = "rgba(255,127,14,1)")
  )
  plots_with_incl_details <- plots_with_incl[[1]]$x$data[[2]]$marker
  expected_plots_details_with_incl <- list(
    color = c("green", "green", "green", "green", "green"),
    size = 15,
    symbol = c("circle", "circle", "circle", "circle", "circle"),
    line = list(color = "rgba(255,127,14,1)")
  )

  expect_equal(plots_details, expected_plots_details, ignore_attr = TRUE)
  expect_equal(plots_with_incl_details, expected_plots_details_with_incl, ignore_attr = TRUE)
})

# Test when there is no exclusion column specified
test_that("get_halflife_plot: marker colors and shapes - exclusion column missing still works", {
  pknca_data <- FIXTURE_PKNCA_DATA
  pknca_data$intervals <- pknca_data$intervals[2,]
  
  # Remove the exclude_half.life column
  pknca_data$conc$data$exclude_half.life <- NULL
  pknca_data$conc$columns$exclude_half.life <- NULL
  plots <- get_halflife_plots(pknca_data)[["plots"]]

  plots_details <- plots[[1]]$x$data[[2]]$marker
  expected_plots_details <- list(
    color = c("red", "red", "green", "green", "green"),
    size = 15,
    symbol = c("circle", "circle", "circle", "circle", "circle"),
    line = list(color = "rgba(255,127,14,1)")
  )
  
  expect_equal(plots_details, expected_plots_details, ignore_attr = TRUE)
})