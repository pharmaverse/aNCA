# Sample data for testing
sample_data <- data.frame(
  STUDYID = rep("Study1", 24),
  USUBJID = rep(c("Subject1", "Subject2", "Subject3", "Subject4"), each = 6),
  ANALYTE = rep("Analyte1", 24),
  DOSNO = rep(1, 24),
  EVID = rep(0, 24),
  NRRLT = rep(1:6, 4),
  ARRLT = rep(1:6, 4),
  ARRLT = rep(1:6, 4),
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
# testthat::test_file("tests/testhat/test-general_lineplot.R")
describe("general_lineplot functions correctly", {
  it("returns a ggplot object", {
    p <- general_lineplot(
      data = sample_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"),
      colorby_var = "DOSNO",
      time_scale = "By Cycle",
      xaxis_scale = "Linear",
      cycle = 1
    )
    expect_s3_class(p, "ggplot")
  })

  it("handles empty data gracefully", {
    empty_data <- sample_data[0, ]
    p <- general_lineplot(
      data = empty_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"),
      colorby_var = "DOSNO",
      time_scale = "By Cycle",
      xaxis_scale = "Linear",
      cycle = 1
    )
    expect_s3_class(p, "ggplot")
    expect_true(length(p$layers) == 0)
  })

  it("handles missing columns gracefully", {
    incomplete_data <- sample_data %>% select(-AVAL)
    expect_error(
      general_lineplot(
        data = incomplete_data,
        selected_analytes = "Analyte1",
        selected_usubjids = c("Subject1", "Subject2"),
        colorby_var = "DOSNO",
        time_scale = "By Cycle",
        xaxis_scale = "Linear",
        cycle = 1
      ),
      "object 'AVAL' not found"
    )
  })

  it("can plot with logarithmic scale", {
    p <- general_lineplot(
      data = sample_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"),
      colorby_var = "DOSNO",
      time_scale = "By Cycle",
      xaxis_scale = "Log",
      cycle = 1
    )
    expect_s3_class(p, "ggplot")

    # Check for logarithmic scale in the plot
    is_log_scale <- grepl("log", p$scales$scales[[1]]$trans$name)
    expect_true(is_log_scale)
  })
})
