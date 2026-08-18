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
    expect_equal(p$labels$colour, "Unique Subject Identifier")
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

  it("can lock y-axis scale across facets while keeping x free", {
    p_free <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      facet_by = "USUBJID"
    )
    p_locked <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      facet_by = "USUBJID",
      lock_y_axis = TRUE
    )

    expect_true(p_free$facet$params$free$x)
    expect_true(p_free$facet$params$free$y)
    expect_true(p_locked$facet$params$free$x)
    expect_false(p_locked$facet$params$free$y)
  })

  it("errors when a single facet contains more than one unit", {
    bad_data <- ind_data %>%
      filter(USUBJID == "Subject1") %>%
      mutate(AVALU = ifelse(NFRLT <= 2, "ng/mL", "ug/mL"))

    expect_warning(
      p <- g_lineplot(
        data = bad_data,
        x_var = "NFRLT",
        y_var = "AVAL",
        y_unit = "AVALU",
        color_by = "USUBJID",
        facet_by = "USUBJID",
        lock_y_axis = TRUE
      ),
      "multiple units"
    )
    expect_equal(p$labels$title, "Error")
  })

  it("converts convertible facet units to a shared target when locked", {
    # Two facets, equal row counts; target resolves to ng/mL (alphabetical tie-break)
    conv_data <- ind_data %>%
      filter(AVAL > 0) %>%
      mutate(AVALU = ifelse(USUBJID == "Subject2", "ug/mL", "ng/mL"))

    p <- g_lineplot(
      data = conv_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      y_unit = "AVALU",
      color_by = "USUBJID",
      facet_by = "USUBJID",
      lock_y_axis = TRUE
    )

    # y-axis label uses the single shared target unit, not a collapsed list
    expect_equal(p$labels$y, "AVAL [ng/mL]")
    expect_false(grepl("ug/mL", p$labels$y))
    expect_false(p$facet$params$free$y)

    # Subject2 values scaled by 1000 (ug/mL -> ng/mL); all rows now ng/mL
    expect_true(all(p$data$AVALU == "ng/mL"))
    s2_in <- conv_data$AVAL[conv_data$USUBJID == "Subject2"]
    s2_out <- p$data$AVAL[p$data$USUBJID == "Subject2"]
    expect_equal(sort(s2_out), sort(s2_in * 1000))
  })

  it("does not convert when lock_y_axis is FALSE", {
    conv_data <- ind_data %>%
      filter(AVAL > 0) %>%
      mutate(AVALU = ifelse(USUBJID == "Subject2", "ug/mL", "ng/mL"))

    p <- g_lineplot(
      data = conv_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      y_unit = "AVALU",
      color_by = "USUBJID",
      facet_by = "USUBJID",
      lock_y_axis = FALSE
    )

    s2_out <- p$data$AVAL[p$data$USUBJID == "Subject2"]
    s2_in <- conv_data$AVAL[conv_data$USUBJID == "Subject2"]
    expect_equal(sort(s2_out), sort(s2_in))
  })

  it("scales the SD/CI family with y when converting units", {
    fam_data <- mean_data %>%
      filter(Mean > 0) %>%
      mutate(
        facet_grp = rep(c("F1", "F2"), length.out = n()),
        AVALU = ifelse(facet_grp == "F2", "ug/mL", "ng/mL")
      )

    p <- g_lineplot(
      data = fam_data,
      x_var = "NFRLT",
      y_var = "Mean",
      y_unit = "AVALU",
      color_by = "color_var",
      facet_by = "facet_grp",
      lock_y_axis = TRUE
    )

    in_f2 <- fam_data %>% filter(facet_grp == "F2")
    out_f2 <- p$data %>% filter(facet_grp == "F2")
    # ug/mL -> ng/mL multiplies every y-family column by 1000
    expect_equal(sort(out_f2$Mean), sort(in_f2$Mean * 1000))
    expect_equal(sort(out_f2$SD_min), sort(in_f2$SD_min * 1000))
    expect_equal(sort(out_f2$CI_upper), sort(in_f2$CI_upper * 1000))
  })

  it("annotates facet strip with unit when conversion is impossible", {
    # nmol/L is not convertible to a mass/volume unit without molecular weight
    mixed_data <- ind_data %>%
      filter(AVAL > 0) %>%
      mutate(AVALU = ifelse(USUBJID == "Subject2", "nmol/L", "ng/mL"))

    expect_warning(
      p <- g_lineplot(
        data = mixed_data,
        x_var = "NFRLT",
        y_var = "AVAL",
        y_unit = "AVALU",
        color_by = "USUBJID",
        facet_by = "USUBJID",
        lock_y_axis = TRUE
      ),
      "not aligned"
    )

    expect_true("facet_label" %in% names(p$data))
    non_conv_label <- unique(p$data$facet_label[p$data$USUBJID == "Subject2"])
    expect_true(any(grepl("\\[nmol/L\\]", non_conv_label)))
    # Convertible facet keeps the target unit and is not annotated
    conv_label <- unique(p$data$facet_label[p$data$USUBJID == "Subject1"])
    expect_false(any(grepl("\\[", conv_label)))
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
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      facet_by = "PARAM",
      facet_count_n = "USUBJID"
    )
    expect_true("facet_label" %in% names(p$data))
    expect_true(any(grepl("PARAM: Analyte1", unique(p$data$facet_label))))
    expect_true(any(grepl("\\(n=2\\)", unique(p$data$facet_label))))
  })

  it("does not add facet counts when facet_count_n is NULL", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      facet_by = "PARAM"
    )
    expect_false("facet_label" %in% names(p$data))
  })

  it("uses precomputed facet count column", {
    mean_data_with_count <- mean_data %>%
      mutate(USUBJID_COUNT = 7)

    p <- g_lineplot(
      data = mean_data_with_count,
      x_var = "NFRLT",
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
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      x_limits = c(1, 8),
      y_limits = c(10, 100)
    )
    expect_equal(p$coordinates$limits$x, c(1, 8))
    expect_equal(p$coordinates$limits$y, c(10, 100))
  })
})

describe("g_lineplot: show_legend", {
  it("hides legend when show_legend is FALSE", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      show_legend = FALSE
    )
    expect_equal(p$theme$legend.position, "none")
  })

  it("shows legend by default", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID"
    )
    expect_true(is.null(p$theme$legend.position) ||
                  p$theme$legend.position != "none")
  })
})

describe("g_lineplot: .build_axis_label", {
  it("returns label with unit suffix when unit column is provided", {
    result <- .build_axis_label("AVAL", "AVALU", ind_data, metadata_nca_variables)
    expect_true(grepl("\\[ng/mL\\]$", result))
    expect_true(grepl("^Analysis Value", result))
  })

  it("returns label without unit when unit column is NULL", {
    result <- .build_axis_label("AVAL", NULL, ind_data, metadata_nca_variables)
    expect_equal(result, "Analysis Value")
  })

  it("falls back to variable name when labels_df is NULL", {
    result <- .build_axis_label("AVAL", NULL, ind_data, NULL)
    expect_equal(result, "AVAL")
  })
})

describe("g_lineplot: .build_tooltip", {
  it("sets tooltip_text to NA when tooltip_vars is NULL", {
    result <- .build_tooltip(ind_data, NULL, NULL)
    expect_true(all(is.na(result$tooltip_text)))
  })

  it("uses generate_tooltip_text when labels_df is provided", {
    result <- .build_tooltip(
      ind_data, c("USUBJID", "AVAL"), metadata_nca_variables
    )
    expect_true(any(grepl("<b>Unique Subject Identifier</b>", result$tooltip_text)))
  })

  it("uses simple paste when labels_df is NULL", {
    result <- .build_tooltip(ind_data, c("USUBJID", "AVAL"), NULL)
    expect_true(any(grepl("USUBJID: Subject1", result$tooltip_text)))
    expect_false(any(grepl("<b>", result$tooltip_text)))
  })
})

describe("g_lineplot: color_labels", {
  it("uses color_labels for legend title when provided", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      color_labels = "Subject ID"
    )
    expect_equal(p$labels$colour, "Subject ID")
  })

  it("derives color_labels from labels_df when not provided", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = "USUBJID",
      labels_df = metadata_nca_variables
    )
    expect_equal(p$labels$colour, "Unique Subject Identifier")
  })

  it("falls back to variable name when label lookup returns NA", {
    sparse_labels <- data.frame(
      Dataset = "ADNCA",
      Variable = "USUBJID",
      Label = "Subject ID",
      stringsAsFactors = FALSE
    )
    p <- g_lineplot(
      data = ind_data,
      x_var = "NFRLT",
      y_var = "AVAL",
      color_by = c("USUBJID", "DOSEA"),
      labels_df = sparse_labels
    )
    # DOSEA has no label in sparse_labels, so get_label returns "DOSEA"
    expect_true(grepl("Subject ID", p$labels$colour))
    expect_true(grepl("DOSEA", p$labels$colour))
  })
})
