TEST_DATA <- data.frame(
  TIME = 1:6,
  RESULT = c(5, 6, 8, 9, 12, 11),
  DOSE = as.factor(c(10, 10, 20, 20, 30, 30)),
  ARM = rep(c("A", "B"), each = 3)
)
DUMMY_LABELS_DF <- data.frame()

describe("faceted_qc_plot", {
  it("returns a ggplot object without errors", {
    plot_object <- expect_no_error(
      faceted_qc_plot(
        data = TEST_DATA,
        x_var = "TIME",
        y_var = "RESULT",
        colour_var = "DOSE",
        grouping_vars = "ARM",
        labels_df = DUMMY_LABELS_DF,
        title = "Test Plot"
      )
    )
    expect_s3_class(plot_object, "ggplot")
  })

  it("maps aesthetics and labels correctly", {
    plot_object <- faceted_qc_plot(
      data = TEST_DATA,
      x_var = "TIME",
      y_var = "RESULT",
      colour_var = "DOSE",
      grouping_vars = "ARM",
      labels_df = DUMMY_LABELS_DF,
      title = "Test Plot"
    )

    expect_equal(plot_object$labels$x, "TIME")
    expect_equal(plot_object$labels$y, "RESULT")
    expect_equal(plot_object$labels$colour, "DOSE")
    expect_equal(plot_object$labels$title, "Test Plot")
    expect_true(grepl("ARM", plot_object$labels$subtitle))
  })

  it("creates the correct number of facets", {
    plot_object <- faceted_qc_plot(
      data = TEST_DATA,
      x_var = "TIME",
      y_var = "RESULT",
      colour_var = "DOSE",
      grouping_vars = "ARM",
      labels_df = DUMMY_LABELS_DF,
      title = "Test Plot"
    )
    built_plot <- ggplot2::ggplot_build(plot_object)
    num_panels <- length(unique(built_plot$layout$layout$PANEL))
    num_groups <- length(unique(TEST_DATA$ARM))

    expect_equal(num_panels, num_groups)
  })

  it("handles multiple grouping variables for faceting", {
    data_multi_group <- TEST_DATA %>% mutate(SEX = rep(c("F", "M", "F"), 2))
    plot_object <- faceted_qc_plot(
      data = data_multi_group,
      x_var = "TIME",
      y_var = "RESULT",
      colour_var = "DOSE",
      grouping_vars = c("ARM", "SEX"),
      labels_df = DUMMY_LABELS_DF,
      title = "Test Plot"
    )
    built_plot <- ggplot2::ggplot_build(plot_object)
    num_panels <- length(unique(built_plot$layout$layout$PANEL))
    num_groups <- nrow(unique(data_multi_group[, c("ARM", "SEX")]))

    expect_equal(num_panels, num_groups)
  })
})