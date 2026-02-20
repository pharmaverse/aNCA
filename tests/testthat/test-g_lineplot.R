# --- Setup: Sample Data ---

# 1. Sample data for INDIVIDUAL plot mode
ind_data <- expand.grid(
  NFRLT = c(0, 1, 2, 4, 8, 12),
  USUBJID = c("Subject1", "Subject2")
) %>%
  mutate(
    AVAL = ifelse(USUBJID == "Subject1", 50, 80) * exp(-0.5 * NFRLT) + rnorm(n(), 0, 1),
    PARAM = "Analyte1",
    DOSEA = "Dose 1",
    color_var = interaction(USUBJID, DOSEA, sep = ", "),
    RRLTU = "hours",
    AVALU = "ng/mL"
  ) %>%
  # Add non-positive value for log test
  bind_rows(data.frame(
    NFRLT = 24, USUBJID = "Subject1", AVAL = 0, PARAM = "Analyte1",
    DOSEA = "Dose 1", color_var = "Subject1, Dose 1",
    RRLTU = "hours", AVALU = "ng/mL"
  )) %>%

  # Represent dosing time in a variable
  mutate(TIME_DOSE = ifelse(NFRLT < 6, 0, 6))

# 2. Sample data for MEAN plot mode
mean_data <- expand.grid(
  NFRLT = c(0, 2, 4, 8),
  color_var = c("GroupA", "GroupB")
) %>%
  mutate(
    Mean = ifelse(color_var == "GroupA", 100, 80) * exp(-0.3 * NFRLT),
    SD = Mean * 0.2, # 20% CV
    N = 4,
    SE = SD / sqrt(N),
    SD_min = Mean - SD,
    SD_max = Mean + SD,
    CI_lower = Mean - 1.96 * SE,
    CI_upper = Mean + 1.96 * SE,
    PARAM = "Analyte1",
    RRLTU = "hours",
    AVALU = "ng/mL"
  ) %>%
  # Add non-positive value for log test
  bind_rows(data.frame(
    NFRLT = 12, color_var = "GroupA", Mean = 0, SD = 0, N = 4, SE = 0,
    SD_min = 0, SD_max = 0, CI_lower = 0, CI_upper = 0,
    PARAM = "Analyte1", RRLTU = "hours", AVALU = "ng/mL"
  )) %>%
  # Represent dosing time in a variable
  mutate(TIME_DOSE = ifelse(NFRLT < 6, 0, 6))

# --- Tests ---

describe("g_lineplot: structure and arguments", {
  it("returns a ggplot object with individual labels", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      x_unit = "RRLTU",
      y_unit = "AVALU",
      color_by = "USUBJID",
      labels_df = metadata_nca_variables
    )
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "PK Concentration - Time Profile")
    expect_equal(p$labels$y, "Analysis Value [ng/mL]")
    expect_equal(p$labels$x, "Nom. Rel. Time from Analyte First Dose [hours]")
    expect_equal(p$labels$colour, "USUBJID")
  })

  it("applies faceting", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      facet_by = "PARAM"
    )
    expect_s3_class(p$facet, "FacetWrap")
  })

  it("applies log scale", {
    p <- g_lineplot(
      data = ind_data %>% filter(AVAL > 0), # Remove non-positive for log test
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      ylog_scale = TRUE
    )
    # Test: Check that the log scale was *added* to the plot
    plot_build <- ggplot_build(p)
    expect_equal(plot_build$layout$panel_scales_y[[1]]$trans$name, "log-10")
  })

  it("shows threshold line", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      threshold_value = 10
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomHline" %in% layer_classes)
  })

  it("shows dose lines and respects facets", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      facet_by = c("PARAM", "DOSEA"),
      vline_var = "TIME_DOSE"
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomVline" %in% layer_classes)

    vline_layer <- p$layers[[which(layer_classes == "GeomVline")]]
    # Check vline data contains facet variables
    expect_true(all(c("PARAM", "DOSEA") %in% names(vline_layer$data)))
  })

  it("adds facet labels with subject counts", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      color_by = "USUBJID",
      facet_by = "PARAM"
    )
    expect_true("facet_label" %in% names(p$data))
    expect_true(any(grepl("PARAM: Analyte1", unique(p$data$facet_label))))
    expect_true(any(grepl("\\(n=2\\)", unique(p$data$facet_label))))
  })

  it("uses precomputed facet count column", {
    mean_data_with_count <- mean_data %>%
      mutate(USUBJID_COUNT = 7)

    p <- g_lineplot(
      data = mean_data_with_count,
      x_var = "time_var",
      y_var = "Mean",
      color_by = "color_var",
      facet_by = "PARAM",
      facet_count_n = "USUBJID_COUNT"
    )
    expect_true(any(grepl("\\(n=7\\)", unique(p$data$facet_label))))
  })

  it("applies x and y limits", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      x_limits = c(1, 8),
      y_limits = c(10, 100)
    )
    expect_equal(p$coordinates$limits$x, c(1, 8))
    expect_equal(p$coordinates$limits$y, c(10, 100))
  })

  it("if specified, applies a custom palette color", {
    palette_options <- c("plasma", "cividis", "inferno")
    n_colors <- length(unique(ind_data$color_var))
    for (pal in palette_options) {
      p <- g_lineplot(
        data = ind_data,
        x_var = "NFRLT",
        y_var = "AVAL",
        color_by = "color_var",
        palette = pal
      )
      p_build <- ggplot_build(p)
      plot_colors <- unique(p_build$data[[1]]$colour)
      exp_colors <- ggplot2::scale_fill_viridis_d(option = pal)$palette(n_colors)
      expect_true(all(plot_colors %in% exp_colors))
    }
  })

  it("handles multiple color_by labels", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = c("USUBJID", "DOSEA")
    )
    expect_equal(p$labels$colour, "USUBJID, DOSEA")
  })

  it("handles empty data.frame with a plot informing of no data", {
    empty_ind_data <- ind_data[0, ]
    p <- g_lineplot(
      data = empty_ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID"
    )
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Error")
    gg_build <- ggplot_build(p)
    expect_equal(
      gg_build[[1]][[1]]$label,
      "No data available for the plot"
    )
  })
})

describe("g_lineplot: Tooltips", {
  it("constructs default tooltips if no vars provided", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID"
    )
    # Check that tooltip_text column was created in the plot data
    expect_true("tooltip_text" %in% names(p$data))
  })

  it("uses generate_tooltip_text when labels_df is provided", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      tooltip_vars = c("USUBJID", "AVAL"),
      labels_df = metadata_nca_variables
    )
    # Check for bold tags added by generate_tooltip_text
    expect_true(any(grepl("<b>Unique Subject Identifier</b>", p$data$tooltip_text)))
    expect_true(any(grepl("<b>Analysis Value</b>", p$data$tooltip_text)))
  })

  it("falls back to simple paste if labels_df is missing but vars provided", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      tooltip_vars = c("USUBJID", "AVAL"),
      labels_df = NULL
    )
    # Check for simple format "Var: Value"
    expect_true(any(grepl("USUBJID: Subject1", p$data$tooltip_text)))
    # Check NO bold tags
    expect_false(any(grepl("<b>", p$data$tooltip_text)))
  })

  it("applies x and y limits", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      color_by = "USUBJID",
      x_limits = c(1, 8),
      y_limits = c(10, 100)
    )
    expect_equal(p$coordinates$limits$x, c(1, 8))
    expect_equal(p$coordinates$limits$y, c(10, 100))
  })
})
