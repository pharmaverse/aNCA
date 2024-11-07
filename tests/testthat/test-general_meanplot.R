# Sample data for testing
sample_data <- data.frame(
  STUDYID = rep("Study1", 24),
  USUBJID = rep(c("Subject1", "Subject2", "Subject3", "Subject4"), each = 6),
  ANALYTE = rep("Analyte1", 24),
  DOSNO = rep(1, 24),
  EVID = rep(0, 24),
  NRRLT = rep(1:6, 4),
  AVAL = c(
    10, 20, 30, 40, 50, 60,
    15, 25, 35, 45, 55, 65,
    12, 22, 32, 42, 52, 62,
    18, 28, 38, 48, 58, 68
  ),
  RRLTU = rep("hours", 24),
  AVALU = rep("ng/mL", 24),
  DOSEA = rep(35, 24)
)

describe("general_meanplot functions correctly", {
  it("returns a ggplot object", {
    p <- general_meanplot(
      data = sample_data,
      selected_studyids = "Study1",
      selected_analytes = "Analyte1",
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
        selected_cycles = 1
      ),
      "object 'AVAL' not found"
    )
  })
  it("can plot with standard deviation error bars", {
    p <- general_meanplot(
      data = sample_data,
      selected_studyids = "Study1",
      selected_analytes = "Analyte1",
      selected_cycles = 1,
      plot_sd = TRUE
    )
    expect_s3_class(p, "ggplot")
    expect_s3_class(ggplotly(p), "plotly")
    # Check for error bars in the plotly object
    has_error_bars <- any(sapply(ggplotly(p)$x$data, function(trace) "error_y" %in% names(trace)))
    expect_true(has_error_bars)
  })

  it("can plot with confidence interval ribbon", {
    p <- general_meanplot(
      data = sample_data,
      selected_studyids = "Study1",
      selected_analytes = "Analyte1",
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
    expect_true(has_ribbon)
  })

  it("can plot with logarithmic scale", {
    p <- general_meanplot(
      data = sample_data,
      selected_studyids = "Study1",
      selected_analytes = "Analyte1",
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
})
