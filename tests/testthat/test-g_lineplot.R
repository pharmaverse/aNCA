
# --- Setup: Sample Data ---

# 1. Sample data for INDIVIDUAL plot mode
ind_data <- expand.grid(
  time_var = c(0, 1, 2, 4, 8, 12),
  USUBJID = c("Subject1", "Subject2")
) %>%
  mutate(
    AVAL = ifelse(USUBJID == "Subject1", 50, 80) * exp(-0.5 * time_var) + rnorm(n(), 0, 1),
    PARAM = "Analyte1",
    DOSEA = "Dose 1",
    color_var = interaction(USUBJID, DOSEA, sep = ", "),
    RRLTU = "hours",
    AVALU = "ng/mL"
  ) %>%
  # Add non-positive value for log test
  bind_rows(data.frame(
    time_var = 24, USUBJID = "Subject1", AVAL = 0, PARAM = "Analyte1",
    DOSEA = "Dose 1", color_var = "Subject1, Dose 1",
    RRLTU = "hours", AVALU = "ng/mL"
  ))

# 2. Sample data for MEAN plot mode
mean_data <- expand.grid(
  time_var = c(0, 2, 4, 8),
  color_var = c("GroupA", "GroupB")
) %>%
  mutate(
    Mean = ifelse(color_var == "GroupA", 100, 80) * exp(-0.3 * time_var),
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
    time_var = 12, color_var = "GroupA", Mean = 0, SD = 0, N = 4, SE = 0,
    SD_min = 0, SD_max = 0, CI_lower = 0, CI_upper = 0,
    PARAM = "Analyte1", RRLTU = "hours", AVALU = "ng/mL"
  ))

# 3. Sample data for dose lines
dose_data <- data.frame(
  TIME_DOSE = c(0, 168),
  PARAM = c("Analyte1", "Analyte1"),
  DOSEA = c("Dose 1", "Dose 1")
)

# --- Tests ---

describe("g_lineplot: Individual Plot Mode", {
  it("returns a ggplot object with individual labels", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      group_var = "USUBJID",
      colorby_var = "USUBJID"
    )
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "PK Concentration - Time Profile")
    expect_equal(p$labels$y, "Concentration [ng/mL]")
    expect_equal(p$labels$x, "Time [hours]")
    expect_equal(p$labels$colour, "USUBJID")
  })

  it("applies faceting", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      group_var = "USUBJID",
      colorby_var = "USUBJID",
      facet_by = "PARAM"
    )
    expect_s3_class(p$facet, "FacetWrap")
  })

  it("applies log scale", {
    p <- g_lineplot(
      data = ind_data, # Contains an AVAL = 0 record
      x_var = "time_var",
      y_var = "AVAL",
      group_var = "USUBJID",
      colorby_var = "USUBJID",
      ylog_scale = TRUE
    )
    # Test: Check that the log scale was *added* to the plot
    is_log_scale <- grepl("log", p$scales$scales[[1]]$trans$name)
    expect_true(is_log_scale)
  })

  it("shows threshold line", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      group_var = "USUBJID",
      colorby_var = "USUBJID",
      show_threshold = TRUE,
      threshold_value = 10
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomHline" %in% layer_classes)
  })

  it("shows dose lines and respects facets", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      group_var = "USUBJID",
      colorby_var = "USUBJID",
      facet_by = c("PARAM", "DOSEA"),
      show_dose = TRUE,
      dose_data = dose_data
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomVline" %in% layer_classes)

    vline_layer <- p$layers[[which(layer_classes == "GeomVline")]]
    # Check vline data contains facet variables
    expect_true(all(c("PARAM", "DOSEA") %in% names(vline_layer$data)))
  })

  it("applies a custom palette", {
    test_palette <- c("Subject1, Dose 1" = "#FF0000", "Subject2, Dose 1" = "#0000FF")
    p <- g_lineplot(
      data = ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      group_var = "USUBJID",
      colorby_var = "color_var",
      palette = test_palette
    )
    p_build <- ggplot_build(p)
    plot_colors <- unique(p_build$data[[1]]$colour)
    expect_true(all(plot_colors %in% test_palette))
  })

  it("ignores mean-plot-only arguments (sd, ci)", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      group_var = "USUBJID",
      colorby_var = "USUBJID",
      show_sd_min = TRUE, # Should be ignored
      show_sd_max = TRUE, # Should be ignored
      show_ci = TRUE      # Should be ignored
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_false("GeomErrorbar" %in% layer_classes)
    expect_false("GeomRibbon" %in% layer_classes)
    # Legend label should not have (95% CI)
    expect_equal(p$labels$colour, "USUBJID")
  })

  it("handles multiple colorby_var labels", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      group_var = "USUBJID",
      colorby_var = c("USUBJID", "DOSEA")
    )
    expect_equal(p$labels$colour, "USUBJID, DOSEA")
  })
})

describe("g_lineplot: Mean Plot Mode", {
  it("returns a ggplot object with mean labels", {
    p <- g_lineplot(
      data = mean_data,
      x_var = "time_var",
      y_var = "Mean",
      group_var = "color_var",
      colorby_var = "color_var"
    )
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Mean PK Concentration - Time Profile")
    expect_equal(p$labels$y, "Mean Concentration [ng/mL]")
    expect_equal(p$labels$x, "Nominal Time [hours]")
    expect_equal(p$labels$colour, "color_var")
  })

  it("applies log scale", {
    p <- g_lineplot(
      data = mean_data, # Contains a Mean = 0 record
      x_var = "time_var",
      y_var = "Mean",
      group_var = "color_var",
      colorby_var = "color_var",
      ylog_scale = TRUE
    )
    # Test: Check that the log scale was *added* to the plot
    is_log_scale <- grepl("log", p$scales$scales[[1]]$trans$name)
    expect_true(is_log_scale)
  })
  
  it("shows SD error bars (min, max, and both)", {
    # Both min and max
    p_both <- g_lineplot(
      data = mean_data, x_var = "time_var", y_var = "Mean", group_var = "color_var",
      colorby_var = "color_var", show_sd_min = TRUE, show_sd_max = TRUE
    )
    p_both_build <- ggplot_build(p_both)
    
    # Layer 3 is geom_errorbar
    err_data_both <- p_both_build$data[[3]] %>% filter(ymax > 0)
    
    expect_true(all(err_data_both$ymin < err_data_both$ymax))
    
    # Only min
    p_min <- g_lineplot(
      data = mean_data, x_var = "time_var", y_var = "Mean", group_var = "color_var",
      colorby_var = "color_var", show_sd_min = TRUE, show_sd_max = FALSE
    )
    p_min_build <- ggplot_build(p_min)
    err_data_min <- p_min_build$data[[3]] %>% filter(ymax > 0)

    expect_true(all(err_data_min$ymin < err_data_min$ymax))
    
    # Only max
    p_max <- g_lineplot(
      data = mean_data, x_var = "time_var", y_var = "Mean", group_var = "color_var",
      colorby_var = "color_var", show_sd_min = FALSE, show_sd_max = TRUE
    )
    p_max_build <- ggplot_build(p_max)
    err_data_max <- p_max_build$data[[3]] %>% filter(ymax > 0)
    # When SD_min is FALSE, ymin corresponds to 'Mean' (y_var)
    # ymax corresponds to Mean + SD
    expect_true(all(err_data_max$ymin < err_data_max$ymax))
  })

  it("shows SD error bars and explicitly sets inheritance to FALSE", {
    p <- g_lineplot(
      data = mean_data, x_var = "time_var", y_var = "Mean", group_var = "color_var",
      colorby_var = "color_var", show_sd_min = TRUE, show_sd_max = TRUE
    )
    
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    idx <- which(layer_classes == "GeomErrorbar")
    errorbar_layer <- p$layers[[idx]]
    
    # Verify inherit.aes is FALSE
    expect_false(errorbar_layer$inherit.aes)
    
    # Verify 'text' is NOT in the mapping for errorbars
    expect_null(errorbar_layer$mapping$text)
    expect_true(!is.null(errorbar_layer$mapping$ymin))
    expect_true(!is.null(errorbar_layer$mapping$ymax))
  })

  it("shows CI ribbon and updates legend", {
    p <- g_lineplot(
      data = mean_data,
      x_var = "time_var",
      y_var = "Mean",
      group_var = "color_var",
      colorby_var = "color_var",
      show_ci = TRUE
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomRibbon" %in% layer_classes)
    # Check for legend title update
    expect_true(grepl("(95% CI)", p$labels$colour))
  })

  it("can show both SD bars and CI ribbon", {
    p <- g_lineplot(
      data = mean_data,
      x_var = "time_var",
      y_var = "Mean",
      group_var = "color_var",
      colorby_var = "color_var",
      show_sd_min = TRUE,
      show_sd_max = TRUE,
      show_ci = TRUE
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomErrorbar" %in% layer_classes)
    expect_true("GeomRibbon" %in% layer_classes)
  })
})

describe("g_lineplot: Tooltips", {
  it("constructs default tooltips if no vars provided", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      group_var = "USUBJID",
      colorby_var = "USUBJID"
    )
    # Check that tooltip_text column was created in the plot data
    expect_true("tooltip_text" %in% names(p$data))
  })
  
  it("uses generate_tooltip_text when labels_df is provided", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      group_var = "USUBJID",
      colorby_var = "USUBJID",
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
      x_var = "time_var",
      y_var = "AVAL",
      group_var = "USUBJID",
      colorby_var = "USUBJID",
      tooltip_vars = c("USUBJID", "AVAL"),
      labels_df = NULL
    )
    # Check for simple format "Var: Value"
    expect_true(any(grepl("USUBJID: Subject1", p$data$tooltip_text)))
    # Check NO bold tags
    expect_false(any(grepl("<b>", p$data$tooltip_text)))
  })
})

describe("g_lineplot: Graceful Handling", {
  it("handles empty data.frame", {
    empty_ind_data <- ind_data[0, ]
    p <- g_lineplot(
      data = empty_ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      group_var = "USUBJID",
      colorby_var = "USUBJID"
    )
    expect_s3_class(p, "ggplot")
    # g_lineplot *always* adds geom_line and geom_point
    expect_equal(length(p$layers), 2)
  })

  it("handles missing aesthetic columns", {
    expect_error(
      print(
        g_lineplot(
          data = ind_data,
          x_var = "MISSING_COLUMN", # This column doesn't exist
          y_var = "AVAL",
          group_var = "USUBJID",
          colorby_var = "USUBJID"
        )
      ),
      # use 'regexp' to catch this specific error message
      regexp = "Column `MISSING_COLUMN` not found"
    )
  })

  it("handles missing mean-plot columns", {
    mean_data_missing_ci <- mean_data %>% select(-CI_lower)
    # Test for missing CI column
    expect_error(
      print(
        g_lineplot(
          data = mean_data_missing_ci,
          x_var = "time_var",
          y_var = "Mean",
          group_var = "color_var",
          colorby_var = "color_var",
          show_ci = TRUE # This will fail
        )
      ),
      regexp = "object 'CI_lower' not found"
    )

    mean_data_missing_sd <- mean_data %>% select(-SD_min)
    # Test for missing SD column
    expect_error(
      print(
        g_lineplot(
          data = mean_data_missing_sd,
          x_var = "time_var",
          y_var = "Mean",
          group_var = "color_var",
          colorby_var = "color_var",
          show_sd_min = TRUE # This will fail
        )
      ),
      regexp = "object 'SD_min' not found"
    )
  })
})
