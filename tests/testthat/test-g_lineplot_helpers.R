# Unit tests for internal helper functions in g_lineplot.R

# ---------------------------------------------------------------------------
# Shared test data
# ---------------------------------------------------------------------------

line_data <- data.frame(
  NFRLT    = c(0, 1, 2, 4),
  AVAL     = c(10, 8, 6, 4),
  USUBJID  = "S1",
  DOSEA    = "Dose 1",
  PARAM    = "Analyte1",
  RRLTU    = "hours",
  AVALU    = "ng/mL",
  SD_min   = c(8, 6, 5, 3),
  SD_max   = c(12, 10, 7, 5),
  CI_lower = c(7, 5, 4, 2),
  CI_upper = c(13, 11, 8, 6),
  group_var = "S1",
  stringsAsFactors = FALSE
)

# labels_df compatible with metadata_nca_variables fixture
sparse_labels <- data.frame(
  Dataset  = "ADNCA",
  Variable = c("AVAL", "NFRLT"),
  Label    = c("Analysis Value", "Nom. Time"),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# .build_axis_label
# ---------------------------------------------------------------------------

describe(".build_axis_label", {
  it("returns label with unit suffix from unit column", {
    result <- aNCA:::.build_axis_label(
      "AVAL", "AVALU", line_data, sparse_labels
    )
    expect_true(grepl("\\[ng/mL\\]$", result))
    expect_true(grepl("^Analysis Value", result))
  })

  it("returns label without unit when unit_col is NULL", {
    result <- aNCA:::.build_axis_label("AVAL", NULL, line_data, sparse_labels)
    expect_equal(result, "Analysis Value")
    expect_false(grepl("\\[", result))
  })

  it("falls back to variable name when labels_df is NULL", {
    result <- aNCA:::.build_axis_label("AVAL", NULL, line_data, NULL)
    expect_equal(result, "AVAL")
  })

  it("handles multiple unique unit values by collapsing with comma", {
    multi_unit_data       <- line_data
    multi_unit_data$AVALU <- c("ng/mL", "ug/mL", "ng/mL", "ng/mL")
    result <- aNCA:::.build_axis_label("AVAL", "AVALU", multi_unit_data, NULL)
    expect_true(grepl("ng/mL", result))
    expect_true(grepl("ug/mL", result))
  })
})

# ---------------------------------------------------------------------------
# .build_tooltip
# ---------------------------------------------------------------------------

describe(".build_tooltip", {
  it("sets tooltip_text to NA when tooltip_vars is NULL", {
    result <- aNCA:::.build_tooltip(line_data, NULL, NULL)
    expect_true(all(is.na(result$tooltip_text)))
  })

  it("uses generate_tooltip_text when labels_df is provided", {
    result <- aNCA:::.build_tooltip(
      line_data, c("USUBJID", "AVAL"), sparse_labels
    )
    expect_true(any(grepl("<b>Analysis Value</b>", result$tooltip_text)))
  })

  it("uses simple 'Var: Value' format when labels_df is NULL", {
    result <- aNCA:::.build_tooltip(line_data, c("USUBJID", "AVAL"), NULL)
    expect_true(any(grepl("USUBJID: S1", result$tooltip_text)))
    expect_false(any(grepl("<b>", result$tooltip_text)))
  })

  it("silently ignores tooltip_vars not present in data", {
    # Only valid vars are pasted; missing ones are skipped via intersect()
    result <- aNCA:::.build_tooltip(
      line_data, c("NONEXISTENT", "AVAL"), NULL
    )
    # Should still produce tooltip_text without error
    expect_true("tooltip_text" %in% names(result))
  })
})

# ---------------------------------------------------------------------------
# .resolve_color_labels
# ---------------------------------------------------------------------------

describe(".resolve_color_labels", {
  it("returns color_labels unchanged when explicitly provided", {
    result <- aNCA:::.resolve_color_labels("USUBJID", "My Label", NULL)
    expect_equal(result, "My Label")
  })

  it("returns NULL when both color_labels and labels_df are NULL", {
    result <- aNCA:::.resolve_color_labels("USUBJID", NULL, NULL)
    expect_null(result)
  })

  it("derives labels from labels_df when color_labels is NULL", {
    result <- aNCA:::.resolve_color_labels("AVAL", NULL, sparse_labels)
    expect_equal(unname(result), "Analysis Value")
  })

  it("uses variable name as fallback for missing lookups", {
    result <- aNCA:::.resolve_color_labels(
      c("AVAL", "DOSEA"), NULL, sparse_labels
    )
    expect_equal(length(result), 2)
    expect_equal(unname(result[1]), "Analysis Value")
    # DOSEA not in sparse_labels -> get_label returns "DOSEA"
    expect_equal(unname(result[2]), "DOSEA")
  })
})

# ---------------------------------------------------------------------------
# .build_color_legend_title
# ---------------------------------------------------------------------------

describe(".build_color_legend_title", {
  it("joins multiple color_labels with newlines when provided", {
    result <- aNCA:::.build_color_legend_title(
      c("A", "B"), c("Label A", "Label B")
    )
    expect_equal(result, "Label A\nLabel B")
  })

  it("joins color_by with commas when color_labels is NULL", {
    result <- aNCA:::.build_color_legend_title(c("A", "B"), NULL)
    expect_equal(result, "A, B")
  })

  it("replaces NA labels with variable name", {
    result <- aNCA:::.build_color_legend_title(c("A", "B"), c("Label A", NA))
    expect_equal(result, "Label A\nB")
  })
})

# ---------------------------------------------------------------------------
# .build_plot_data
# ---------------------------------------------------------------------------

describe(".build_plot_data", {
  it("adds color_var column as interaction of color_by", {
    result <- aNCA:::.build_plot_data(
      line_data, "NFRLT", "USUBJID", NULL, NULL, NULL, NULL
    )
    expect_true("color_var" %in% names(result))
    expect_equal(as.character(result$color_var[1]), "S1")
  })

  it("combines multiple color_by variables with separator", {
    multi_data       <- line_data
    multi_data$DOSEA <- "Dose 1"
    result <- aNCA:::.build_plot_data(
      multi_data, "NFRLT", c("USUBJID", "DOSEA"),
      NULL, NULL, NULL, NULL
    )
    expect_true(all(grepl("S1, Dose 1", as.character(result$color_var))))
  })

  it("adds group_var when group_by is specified", {
    result <- aNCA:::.build_plot_data(
      line_data, "NFRLT", "USUBJID", "PARAM", NULL, NULL, NULL
    )
    expect_true("group_var" %in% names(result))
  })

  it("sorts data by x_var", {
    shuffled <- line_data[c(3, 1, 4, 2), ]
    result <- aNCA:::.build_plot_data(
      shuffled, "NFRLT", "USUBJID", NULL, NULL, NULL, NULL
    )
    expect_equal(result$NFRLT, sort(line_data$NFRLT))
  })

  it("builds facet_label when facet_count_n and facet_by are provided", {
    result <- aNCA:::.build_plot_data(
      line_data, "NFRLT", "USUBJID",
      NULL, NULL, "PARAM", "USUBJID"
    )
    expect_true("facet_label" %in% names(result))
    expect_true(any(grepl("Analyte1", result$facet_label)))
    expect_true(any(grepl("n=1",      result$facet_label)))
  })

  it("appends dose-normalised suffix to color_var when linetype_by set", {
    lt_data <- line_data
    # Rows with non-empty linetype_by get a "(DN)" suffix on color_var;
    # rows with empty string stay as-is.
    dn_marker      <- "DN"
    lt_data$lt_var <- c(dn_marker, "", dn_marker, "")
    result <- aNCA:::.build_plot_data(
      lt_data, "NFRLT", "USUBJID", "USUBJID", "lt_var", NULL, NULL
    )
    dn_rows    <- lt_data$lt_var != ""
    plain_rows <- lt_data$lt_var == ""
    expect_true(all(grepl(" \\(DN\\)$", as.character(result$color_var[dn_rows]))))
    expect_false(any(grepl(dn_marker, as.character(result$color_var[plain_rows]))))
  })

  it("color_var is a factor with default levels first when linetype_by set", {
    dn_marker      <- "DN"
    lt_data        <- line_data
    lt_data$lt_var <- c(dn_marker, dn_marker, "", "")
    result <- aNCA:::.build_plot_data(
      lt_data, "NFRLT", "USUBJID", "USUBJID", "lt_var", NULL, NULL
    )
    expect_s3_class(result$color_var, "factor")
    lvls <- levels(result$color_var)
    # Plain level should come before the dose-normalised level
    plain_idx <- which(lvls == "S1")
    dn_idx    <- which(lvls == "S1 (DN)")
    expect_true(plain_idx < dn_idx)
  })
})

# ---------------------------------------------------------------------------
# .build_aes
# ---------------------------------------------------------------------------

describe(".build_aes", {
  it("returns a list with x, y, color, group=NULL, text mapping", {
    aes_args <- aNCA:::.build_aes("NFRLT", "AVAL", NULL, NULL)
    expect_equal(as.character(aes_args$x), "NFRLT")
    expect_equal(as.character(aes_args$y), "AVAL")
    expect_null(aes_args$group)
    expect_false(is.null(aes_args$text))
  })

  it("sets group when group_by is provided", {
    aes_args <- aNCA:::.build_aes("NFRLT", "AVAL", "USUBJID", NULL)
    expect_false(is.null(aes_args$group))
  })

  it("adds linetype when linetype_by is provided", {
    aes_args <- aNCA:::.build_aes("NFRLT", "AVAL", NULL, "PARAM")
    expect_true("linetype" %in% names(aes_args))
  })
})

# ---------------------------------------------------------------------------
# .add_y_scale
# ---------------------------------------------------------------------------

describe(".add_y_scale", {
  it("returns NULL when ylog_scale is FALSE", {
    expect_null(aNCA:::.add_y_scale(FALSE))
  })

  it("returns a log10 scale layer when ylog_scale is TRUE", {
    layer <- aNCA:::.add_y_scale(TRUE)
    expect_true(inherits(layer, "Scale"))
  })
})

# ---------------------------------------------------------------------------
# .add_faceting
# ---------------------------------------------------------------------------

describe(".add_faceting", {
  it("returns NULL when facet_by is NULL", {
    expect_null(aNCA:::.add_faceting(NULL))
  })

  it("returns NULL when facet_by is empty character vector", {
    expect_null(aNCA:::.add_faceting(character(0)))
  })

  it("returns a FacetWrap specification for a non-empty facet_by", {
    result <- aNCA:::.add_faceting("PARAM")
    expect_true(inherits(result, "FacetWrap"))
  })
})

# ---------------------------------------------------------------------------
# .add_thr
# ---------------------------------------------------------------------------

describe(".add_thr", {
  it("returns NULL for non-numeric threshold", {
    expect_null(aNCA:::.add_thr(NULL))
    expect_null(aNCA:::.add_thr("10"))
  })

  it("returns NULL for non-finite threshold", {
    expect_null(aNCA:::.add_thr(NA))
    expect_null(aNCA:::.add_thr(Inf))
  })

  it("returns a geom_hline layer for a finite numeric threshold", {
    layer <- aNCA:::.add_thr(5)
    expect_true(inherits(layer$geom, "GeomHline"))
    expect_equal(layer$data$yintercept, 5)
  })
})

# ---------------------------------------------------------------------------
# .add_vline
# ---------------------------------------------------------------------------

describe(".add_vline", {
  it("returns NULL when vline_var is NULL", {
    expect_null(aNCA:::.add_vline(line_data, NULL))
  })

  it("returns a geom_vline layer when vline_var is provided", {
    line_data$TIME_DOSE <- c(0, 0, 6, 6)
    layer <- aNCA:::.add_vline(line_data, "TIME_DOSE")
    expect_true(inherits(layer$geom, "GeomVline"))
  })
})

# ---------------------------------------------------------------------------
# .add_axis_limits
# ---------------------------------------------------------------------------

describe(".add_axis_limits", {
  it("returns NULL when both x_limits and y_limits are NULL", {
    expect_null(aNCA:::.add_axis_limits(NULL, NULL))
  })

  it("returns coord_cartesian when x_limits are supplied", {
    layer <- aNCA:::.add_axis_limits(c(1, 10), NULL)
    expect_true(inherits(layer, "CoordCartesian"))
  })

  it("returns coord_cartesian when y_limits are supplied", {
    layer <- aNCA:::.add_axis_limits(NULL, c(0, 100))
    expect_true(inherits(layer, "CoordCartesian"))
  })

  it("returns NULL when limits are non-finite (e.g., c(NA, NA))", {
    expect_null(aNCA:::.add_axis_limits(c(NA, NA), NULL))
  })
})

# ---------------------------------------------------------------------------
# .add_colour_palette
# ---------------------------------------------------------------------------

describe(".add_colour_palette", {
  it("returns NULL for 'default' palette", {
    expect_null(aNCA:::.add_colour_palette("default"))
  })

  it("returns a viridis color scale for 'plasma'", {
    result <- aNCA:::.add_colour_palette("plasma")
    expect_true(inherits(result, "Scale"))
  })

  it("returns a viridis color scale for 'cividis'", {
    result <- aNCA:::.add_colour_palette("cividis")
    expect_true(inherits(result, "Scale"))
  })

  it("returns a viridis color scale for 'inferno'", {
    result <- aNCA:::.add_colour_palette("inferno")
    expect_true(inherits(result, "Scale"))
  })
})

# ---------------------------------------------------------------------------
# .add_mean_layers
# ---------------------------------------------------------------------------

describe(".add_mean_layers", {
  it("returns NULLs when sd_min, sd_max, and ci are all FALSE", {
    layers <- aNCA:::.add_mean_layers(
      sd_min    = FALSE,  sd_max    = FALSE, ci        = FALSE,
      color_by  = "color_var", y_var = "AVAL",
      x_var     = "NFRLT",     group_var = "group_var"
    )
    expect_null(layers[[1]])  # error_bar_layer
    expect_null(layers[[2]])  # ci_ribbon_layer
  })

  it("returns an error bar layer when sd_min is TRUE", {
    layers <- aNCA:::.add_mean_layers(
      sd_min    = TRUE,  sd_max    = FALSE, ci        = FALSE,
      color_by  = "color_var", y_var = "AVAL",
      x_var     = "NFRLT",     group_var = "group_var"
    )
    expect_true(inherits(layers[[1]]$geom, "GeomErrorbar"))
  })

  it("returns an error bar layer when sd_max is TRUE", {
    layers <- aNCA:::.add_mean_layers(
      sd_min    = FALSE, sd_max    = TRUE,  ci        = FALSE,
      color_by  = "color_var", y_var = "AVAL",
      x_var     = "NFRLT",     group_var = "group_var"
    )
    expect_true(inherits(layers[[1]]$geom, "GeomErrorbar"))
  })

  it("returns a CI ribbon layer when ci is TRUE", {
    layers <- aNCA:::.add_mean_layers(
      sd_min    = FALSE, sd_max    = FALSE, ci        = TRUE,
      color_by  = "color_var", y_var = "AVAL",
      x_var     = "NFRLT",     group_var = "group_var"
    )
    # ci_ribbon_layer is a list with a geom_ribbon + guides element
    expect_true(is.list(layers[[2]]))
    expect_true(any(sapply(
      layers[[2]], function(l) inherits(l$geom, "GeomRibbon")
    )))
  })

  it("returns both error bar and CI ribbon when all flags are TRUE", {
    layers <- aNCA:::.add_mean_layers(
      sd_min    = TRUE,  sd_max    = TRUE,  ci        = TRUE,
      color_by  = "color_var", y_var = "AVAL",
      x_var     = "NFRLT",     group_var = "group_var"
    )
    expect_true(inherits(layers[[1]]$geom, "GeomErrorbar"))
    expect_true(is.list(layers[[2]]))
  })
})
