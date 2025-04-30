# Sample data for testing
sample_data <- data.frame(
  STUDYID = rep("Study1", 24),
  USUBJID = rep(c("Subject1", "Subject2", "Subject3", "Subject4"), each = 6),
  PARAM = rep(c("Analyte1", "Analyte 2"), each = 12),
  PCSPEC = rep(c("Spec1", "Spec2"), each = 12),
  DOSNO = rep(1, 24),
  EVID = rep(0, 24),
  NRRLT = rep(1:6, 4),
  ARRLT = rep(1:6, 4),
  AFRLT = rep(1:6, 4),
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

describe("general_lineplot functions correctly", {
  it("returns a ggplot object", {
    p <- general_lineplot(
      data = sample_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"),
      selected_pcspec = "Spec1",
      colorby_var = "DOSNO",
      time_scale = "By Cycle",
      yaxis_scale = "Linear",
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
      selected_pcspec = "Spec1",
      colorby_var = "DOSNO",
      time_scale = "By Cycle",
      yaxis_scale = "Linear",
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
        selected_pcspec = "Spec1",
        colorby_var = "DOSNO",
        time_scale = "By Cycle",
        yaxis_scale = "Linear",
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
      selected_pcspec = "Spec1",
      colorby_var = "DOSNO",
      time_scale = "By Cycle",
      yaxis_scale = "Log",
      cycle = 1
    )
    expect_s3_class(p, "ggplot")

    # Check for logarithmic scale in the plot
    is_log_scale <- grepl("log", p$scales$scales[[1]]$trans$name)
    expect_true(is_log_scale)
  })

  it("returns a ggplot object with no data available when preprocessed_data is empty", {
    filtered_data <- sample_data %>% filter(USUBJID == "NonExistentSubject")
    p <- general_lineplot(
      data = filtered_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("NonExistentSubject"),
      selected_pcspec = "Spec1",
      colorby_var = "DOSNO",
      time_scale = "By Cycle",
      yaxis_scale = "Linear",
      cycle = 1
    )
    expect_s3_class(p, "ggplot")
    expect_true(p$labels$title == "No data available for selected parameters")
  })

  it("handles predose records when ARRLT < 0 and AFRLT > 0", {
    modified_data <- sample_data
    modified_data$ARRLT <- c(-1, 0, 1, 2, 3, 4, -2, 0, 1, 2, 3, 4,
                             -1, 0, 1, 2, 3, 4, -2, 0, 1, 2, 3, 4)
    modified_data$AFRLT <- c(0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0,
                             1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5)
    p <- general_lineplot(
      data = modified_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"),
      selected_pcspec = "Spec1",
      colorby_var = "DOSNO",
      time_scale = "By Cycle",
      yaxis_scale = "Linear",
      cycle = 1
    )
    expect_s3_class(p, "ggplot")
    expect_true(nrow(p$data) > 0) # Ensure predose records are handled
  })

  it("handles time_scale not equal to 'By Cycle'", {
    p <- general_lineplot(
      data = sample_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"),
      selected_pcspec = "Spec1",
      colorby_var = "DOSNO",
      time_scale = "Overall",
      yaxis_scale = "Linear",
      cycle = 1
    )
    expect_s3_class(p, "ggplot")
    expect_true("AFRLT" %in% names(p$data)) # Ensure time variable is AFRLT
  })
})
