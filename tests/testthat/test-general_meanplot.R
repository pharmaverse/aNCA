# Sample data for testing
sample_data <- data.frame(
  STUDYID = rep("Study1", 48),
  USUBJID = rep(c("Subject1", "Subject2", "Subject3", "Subject4"), each = 12),
  SEX = rep(rep(c("M", "F"), each = 12), times = 2),
  PARAM = rep(c("Analyte1", "Analyte 2"), each = 24),
  PCSPEC = rep(c("Spec1", "Spec2"), each = 24),
  NCA_PROFILE = rep(1, 48),
  EVID = rep(0, 48),
  NRRLT = rep(1:6, 8),
  AVAL = rep(c(
    10, 20, 30, 40, 50, 60,
    15, 25, 35, 45, 55, 65,
    12, 22, 32, 42, 52, 62,
    18, 28, 38, 48, 58, 68
  ), 2),
  RRLTU = rep("hours", 48),
  AVALU = rep("ng/mL", 48),
  DOSEA = rep(35, 48)
)

describe("general_meanplot functions correctly", {
  it("returns a ggplot object", {
    p <- general_meanplot(
      data = sample_data,
      selected_studyids = "Study1",
      selected_analytes = "Analyte1",
      selected_pcspecs = "Spec1",
      selected_cycles = 1
    )
    expect_s3_class(p, "ggplot")
    expect_s3_class(ggplotly(p), "plotly")
  })
  it("handles empty data gracefully", {
    empty_data <- sample_data[0, ]
    p <- general_meanplot(
      data = empty_data,
      selected_studyids = "Study1",
      selected_analytes = "Analyte1",
      selected_pcspecs = "Spec1",
      selected_cycles = 1
    )
    expect_s3_class(p, "ggplot")
    expect_s3_class(ggplotly(p), "plotly")
    expect_true(ggplotly(p)$x$layout$title$text == "No data available")
  })
  it("handles missing columns gracefully", {
    incomplete_data <- sample_data %>% select(-AVAL)
    expect_error(
      general_meanplot(
        data = incomplete_data,
        selected_studyids = "Study1",
        selected_analytes = "Analyte1",
        selected_pcspecs = "Spec1",
        selected_cycles = 1
      ),
      "object 'AVAL' not found"
    )
  })
  it("can plot with standard deviation error bars", {
    # Can plot both min and max standard deviation error bars
    p <- general_meanplot(
      data = sample_data,
      selected_studyids = "Study1",
      selected_analytes = "Analyte1",
      selected_pcspecs = "Spec1",
      selected_cycles = 1,
      plot_sd_min = TRUE,
      plot_sd_max = TRUE
    )
    expect_s3_class(p, "ggplot")
    expect_s3_class(ggplotly(p), "plotly")
    # Check for error bars in the plotly object
    has_error_bars <- any(sapply(ggplotly(p)$x$data, function(trace) "error_y" %in% names(trace)))
    expect_true(has_error_bars)

    # Can plot only min standard deviation error bars
    p_min <- general_meanplot(
      data = sample_data,
      selected_studyids = "Study1",
      selected_analytes = "Analyte1",
      selected_pcspecs = "Spec1",
      selected_cycles = 1,
      plot_sd_min = TRUE,
      plot_sd_max = FALSE
    )
    expect_s3_class(p_min, "ggplot")
    expect_s3_class(ggplotly(p_min), "plotly")
    has_error_bars_min <- any(
      sapply(
        ggplotly(p_min)$x$data, function(trace) "error_y" %in% names(trace)
      )
    )
    expect_true(has_error_bars_min)

    # Can plot only max standard deviation error bars
    p_max <- general_meanplot(
      data = sample_data,
      selected_studyids = "Study1",
      selected_analytes = "Analyte1",
      selected_pcspecs = "Spec1",
      selected_cycles = 1,
      plot_sd_min = FALSE,
      plot_sd_max = TRUE
    )
    expect_s3_class(p_max, "ggplot")
    expect_s3_class(ggplotly(p_max), "plotly")
    has_error_bars_max <- any(
      sapply(
        ggplotly(p_max)$x$data, function(trace) "error_y" %in% names(trace)
      )
    )
    expect_true(has_error_bars_max)

    # Can SD bar for a plot in log scale
    p_log <- general_meanplot(
      data = sample_data,
      selected_studyids = "Study1",
      selected_analytes = "Analyte1",
      selected_pcspecs = "Spec1",
      selected_cycles = 1,
      plot_ylog = TRUE,
      plot_sd_min = TRUE,
      plot_sd_max = TRUE
    )
    expect_s3_class(p_log, "ggplot")
    expect_s3_class(ggplotly(p_log), "plotly")
    has_error_bars_log <- any(
      sapply(
        ggplotly(p_log)$x$data, function(trace) "error_y" %in% names(trace)
      )
    )
    expect_true(has_error_bars_log)
  })

  it("can plot with confidence interval ribbon", {
    p <- general_meanplot(
      data = sample_data,
      selected_studyids = "Study1",
      selected_analytes = "Analyte1",
      selected_pcspecs = "Spec1",
      selected_cycles = 1,
      plot_ci = TRUE
    )
    expect_s3_class(p, "ggplot")
    expect_s3_class(ggplotly(p), "plotly")
    # Check for error bars in the plotly object
    has_ribbon <- any(sapply(ggplotly(p)$x$data, function(trace) {
      "type" %in% names(trace) &&
        trace$type == "scatter"
    }))
    has_ci_in_legend <- ggplotly(p)$x$layout$legend$title$text == "DOSEA (95% CI)"
    expect_true(has_ci_in_legend)
  })

  it("can plot with logarithmic scale", {
    p <- general_meanplot(
      data = sample_data,
      selected_studyids = "Study1",
      selected_analytes = "Analyte1",
      selected_pcspecs = "Spec1",
      selected_cycles = 1,
      plot_ylog = TRUE
    )
    expect_s3_class(p, "ggplot")
    # Check log-transform was added in the ggplot object
    has_log_scale <- grepl("log", p$scales$scales[[1]]$trans$name)
    # Check data was log transformed in ggplot
    has_log_data <- all(log10(p$data$Mean) == ggplot_build(p)$data[[1]][["y"]])
    expect_true(has_log_scale && has_log_data)
  })

  it("can handle multiple id variables to group and color", {
    p <- general_meanplot(
      data = sample_data,
      selected_studyids = "Study1",
      selected_analytes = "Analyte1",
      selected_pcspecs = "Spec1",
      selected_cycles = 1,
      id_variable = c("PARAM", "SEX")
    )
    expect_s3_class(p, "ggplot")
    expect_s3_class(ggplotly(p), "plotly")
  })
})
