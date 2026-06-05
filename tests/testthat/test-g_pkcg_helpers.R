# Unit tests for internal helper functions in g_pkcg.R

# ---------------------------------------------------------------------------
# Shared test data
# ---------------------------------------------------------------------------

helper_data <- data.frame(
  STUDYID = "STUDY1",
  TRT01A  = "Treatment A",
  ROUTE   = "extravascular",
  PCSPEC  = "SERUM",
  PARAM   = "Analyte A",
  USUBJID = paste0("S", 1:4),
  NFRLT   = rep(c(0, 1, 2, 4), each = 1),
  AVAL    = c(5, 4, 3, 2),
  AVALC   = c("5", "4", "3", "BLQ"),
  stringsAsFactors = FALSE
)
attr(helper_data$TRT01A, "label") <- "Treatment"

# Fixture for pkcg03-based tests (all subjects retained to exercise both DOSEA groups)
adpc_pkcg03 <- FIXTURE_CONC_DATA %>%
  mutate(
    USUBJID = as.character(USUBJID),
    TRT01A  = "Dummy Treatment",
    AVALC   = as.character(AVAL),
    DOSEA   = ifelse(USUBJID %in% c("3", "4", "5", "7", "8"), 50, 100)
  )
attr(adpc_pkcg03$USUBJID, "label") <- "Subject ID"
attr(adpc_pkcg03$AVAL,    "label") <- "Analysis value"

# ---------------------------------------------------------------------------
# generate_title
# ---------------------------------------------------------------------------

describe("generate_title", {
  it("generates a default title for LIN scale", {
    title <- aNCA:::generate_title(
      helper_data, title = NULL, scale = "LIN", studyid = "STUDYID"
    )
    expect_true(grepl("linear", title))
    expect_true(grepl("STUDY1", title))
  })

  it("generates a default title for LOG scale", {
    title <- aNCA:::generate_title(
      helper_data, title = NULL, scale = "LOG", studyid = "STUDYID"
    )
    expect_true(grepl("logarithmic", title))
  })

  it("generates a default title for SBS scale", {
    title <- aNCA:::generate_title(
      helper_data, title = NULL, scale = "SBS", studyid = "STUDYID"
    )
    expect_true(grepl("linear and logarithmic", title))
  })

  it("returns custom title when provided", {
    title <- aNCA:::generate_title(
      helper_data, title = "Custom Title", scale = "LIN",
      studyid = "STUDYID"
    )
    expect_equal(title, "Custom Title")
  })
})

# ---------------------------------------------------------------------------
# generate_subtitle
# ---------------------------------------------------------------------------

describe("generate_subtitle", {
  plotgroup_vars  <- c("ROUTE", "PCSPEC", "PARAM")
  plotgroup_names <- list(
    ROUTE = "Route", PCSPEC = "Specimen", PARAM = "Analyte"
  )

  it("generates a default subtitle containing treatment and N", {
    subtitle <- aNCA:::generate_subtitle(
      helper_data, subtitle = NULL,
      trt_var        = "TRT01A",
      plotgroup_vars  = plotgroup_vars,
      plotgroup_names = plotgroup_names
    )
    expect_true(grepl("Treatment A", subtitle))
    expect_true(grepl("N=", subtitle))
  })

  it("includes plotgroup values in the default subtitle", {
    subtitle <- aNCA:::generate_subtitle(
      helper_data, subtitle = NULL,
      trt_var        = "TRT01A",
      plotgroup_vars  = plotgroup_vars,
      plotgroup_names = plotgroup_names
    )
    expect_true(grepl("extravascular", subtitle))
    expect_true(grepl("SERUM",         subtitle))
    expect_true(grepl("Analyte A",     subtitle))
  })

  it("returns custom subtitle when provided", {
    subtitle <- aNCA:::generate_subtitle(
      helper_data, subtitle = "My Subtitle",
      trt_var        = "TRT01A",
      plotgroup_vars  = plotgroup_vars,
      plotgroup_names = plotgroup_names
    )
    expect_equal(subtitle, "My Subtitle")
  })
})

# ---------------------------------------------------------------------------
# generate_title_mean
# ---------------------------------------------------------------------------

describe("generate_title_mean", {
  it("generates a default title referencing mean_group_var", {
    attr(helper_data$TRT01A, "label") <- "Actual Treatment"
    title <- aNCA:::generate_title_mean(
      helper_data, title = NULL, scale = "LIN",
      studyid = "STUDYID", mean_group_var = "TRT01A"
    )
    expect_true(grepl("STUDY1", title))
    expect_true(grepl("linear", title))
  })

  it("returns custom title when provided", {
    title <- aNCA:::generate_title_mean(
      helper_data, title = "Custom Mean Title", scale = "LOG",
      studyid = "STUDYID", mean_group_var = "TRT01A"
    )
    expect_equal(title, "Custom Mean Title")
  })
})

# ---------------------------------------------------------------------------
# generate_subtitle_mean
# ---------------------------------------------------------------------------

describe("generate_subtitle_mean", {
  plotgroup_vars  <- c("ROUTE", "PCSPEC")
  plotgroup_names <- list(ROUTE = "Route", PCSPEC = "Specimen")

  it("generates a default subtitle from plotgroup values", {
    sub_data <- helper_data[1, ]
    subtitle <- aNCA:::generate_subtitle_mean(
      sub_data, subtitle = NULL,
      plotgroup_vars  = plotgroup_vars,
      plotgroup_names = plotgroup_names
    )
    expect_true(grepl("extravascular", subtitle))
    expect_true(grepl("SERUM", subtitle))
  })

  it("returns custom subtitle when provided", {
    subtitle <- aNCA:::generate_subtitle_mean(
      helper_data, subtitle = "My Mean Sub",
      plotgroup_vars  = plotgroup_vars,
      plotgroup_names = plotgroup_names
    )
    expect_equal(subtitle, "My Mean Sub")
  })
})

# ---------------------------------------------------------------------------
# keep_blq_timepoints
# ---------------------------------------------------------------------------

describe("keep_blq_timepoints", {
  it("removes timepoints where > 50% subjects are BLQ", {
    blq_data <- data.frame(
      USUBJID    = c("S1", "S2", "S3", "S1", "S2", "S3"),
      NFRLT      = c(0, 0, 0, 1, 1, 1),
      mean_group = "G1",
      AVALC      = c("BLQ", "BLQ", "BLQ", "2", "3", "4"),
      stringsAsFactors = FALSE
    )
    result <- aNCA:::keep_blq_timepoints(
      blq_data, xvar = "NFRLT", mean_group_var = "mean_group"
    )
    expect_true(all(result$NFRLT == 1))
    expect_false(0 %in% result$NFRLT)
  })

  it("keeps timepoints where <= 50% subjects are BLQ", {
    blq_data <- data.frame(
      USUBJID    = c("S1", "S2", "S1", "S2"),
      NFRLT      = c(0, 0, 1, 1),
      mean_group = "G1",
      AVALC      = c("BLQ", "5", "3", "4"),
      stringsAsFactors = FALSE
    )
    result <- aNCA:::keep_blq_timepoints(
      blq_data, xvar = "NFRLT", mean_group_var = "mean_group"
    )
    expect_true(0 %in% result$NFRLT)
    expect_true(1 %in% result$NFRLT)
  })

  it("removes timepoints with only 1 subject (n_samples <= 1)", {
    solo_data <- data.frame(
      USUBJID    = c("S1", "S1", "S2"),
      NFRLT      = c(0, 1, 1),
      mean_group = "G1",
      AVALC      = c("5", "3", "4"),
      stringsAsFactors = FALSE
    )
    result <- aNCA:::keep_blq_timepoints(
      solo_data, xvar = "NFRLT", mean_group_var = "mean_group"
    )
    expect_false(0 %in% result$NFRLT)
    expect_true(1  %in% result$NFRLT)
  })

  it("handles data without BLQ column (no AVALC or AVALCAT1)", {
    no_blq <- data.frame(
      USUBJID    = c("S1", "S2"),
      NFRLT      = c(0, 0),
      mean_group = "G1",
      stringsAsFactors = FALSE
    )
    result <- aNCA:::keep_blq_timepoints(
      no_blq, xvar = "NFRLT", mean_group_var = "mean_group"
    )
    expect_true(0 %in% result$NFRLT)
  })
})

# ---------------------------------------------------------------------------
# resolve_whiskers (tested via pkcg03 public API — not in namespace)
# ---------------------------------------------------------------------------

describe("resolve_whiskers (via pkcg03)", {
  it("pkcg03 with 'Both' whiskers renders upper and lower error bars", {
    p <- pkcg03(
      adpc_pkcg03,
      summary_method   = "Mean_sdi",
      whiskers_lwr_upr = "Both",
      plotly           = FALSE
    )[[1]]
    layer_eb <- Find(function(l) inherits(l$geom, "GeomErrorbar"), p$layers)
    expect_false(is.null(layer_eb))
    expect_equal(rlang::as_label(layer_eb$mapping$ymin), "mean_sdi_lwr")
    expect_equal(rlang::as_label(layer_eb$mapping$ymax), "mean_sdi_upr")
  })

  it("pkcg03 with 'Upper' whisker uses mean as lower bound", {
    p <- pkcg03(
      adpc_pkcg03,
      summary_method   = "Mean_sdi",
      whiskers_lwr_upr = "Upper",
      plotly           = FALSE
    )[[1]]
    layer_eb <- Find(function(l) inherits(l$geom, "GeomErrorbar"), p$layers)
    expect_equal(rlang::as_label(layer_eb$mapping$ymin), "mean")
    expect_equal(rlang::as_label(layer_eb$mapping$ymax), "mean_sdi_upr")
  })

  it("pkcg03 with 'Lower' whisker uses mean as upper bound", {
    p <- pkcg03(
      adpc_pkcg03,
      summary_method   = "Mean_sdi",
      whiskers_lwr_upr = "Lower",
      plotly           = FALSE
    )[[1]]
    layer_eb <- Find(function(l) inherits(l$geom, "GeomErrorbar"), p$layers)
    expect_equal(rlang::as_label(layer_eb$mapping$ymin), "mean_sdi_lwr")
    expect_equal(rlang::as_label(layer_eb$mapping$ymax), "mean")
  })
})

# ---------------------------------------------------------------------------
# compute_summary_stats (tested via pkcg03 public API — not in namespace)
# ---------------------------------------------------------------------------

describe("compute_summary_stats (via pkcg03)", {
  it("pkcg03 Mean_ci produces a valid plot with CI error-bar layers", {
    plots <- pkcg03(adpc_pkcg03, summary_method = "Mean_ci", plotly = FALSE)
    expect_true(length(plots) > 0)
    p <- plots[[1]]
    expect_s3_class(p, "ggplot")
    layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
    expect_true("GeomErrorbar" %in% layer_classes)
  })

  it("pkcg03 Median_ci produces a valid plot", {
    plots <- suppressWarnings(
      pkcg03(adpc_pkcg03, summary_method = "Median_ci", plotly = FALSE)
    )
    expect_true(length(plots) > 0)
    expect_s3_class(plots[[1]], "ggplot")
  })

  it("pkcg03 handles data where some timepoints are all NA in AVAL", {
    adpc_na        <- adpc_pkcg03
    adpc_na$AVAL[adpc_na$NFRLT == 0.5] <- NA
    plots <- pkcg03(adpc_na, summary_method = "Mean_ci", plotly = FALSE)
    expect_true(length(plots) > 0)
  })

  it("pkcg03 silently drops a group when all timepoints are BLQ-filtered", {
    # Make all samples at every timepoint BLQ for DOSEA==50 group so that
    # keep_blq_timepoints removes all rows → nrow(plot_data)==0 → return(NULL).
    # In this fixture DOSEA==50 has 5 subjects and DOSEA==100 has only 3, so
    # keep_blq_timepoints also filters DOSEA==100 (n_samples <= 1 guard).
    # Both groups are dropped → 0 plots, which is fewer than the 1 produced by
    # the unmodified adpc_pkcg03 fixture.
    adpc_allblq        <- adpc_pkcg03
    adpc_allblq$AVALC[adpc_allblq$DOSEA == 50] <- "BLQ"
    plots <- pkcg03(adpc_allblq, summary_method = "Mean_ci", plotly = FALSE)
    expect_equal(length(plots), 0)
  })
})

# ---------------------------------------------------------------------------
# req_sbs_pkgs
# ---------------------------------------------------------------------------

describe("req_sbs_pkgs", {
  it("stops with informative message when ggh4x is missing", {
    testthat::with_mocked_bindings(
      code = {
        expect_error(
          aNCA:::req_sbs_pkgs(),
          "Side-by-side view requires `ggh4x`"
        )
      },
      requireNamespace = function(pkg, quietly = FALSE) {
        if (pkg == "ggh4x") FALSE else TRUE
      },
      .package = "base"
    )
  })

  it("stops with informative message when scales is missing", {
    testthat::with_mocked_bindings(
      code = {
        expect_error(
          aNCA:::req_sbs_pkgs(),
          "Side-by-side view requires `scales`"
        )
      },
      requireNamespace = function(pkg, quietly = FALSE) {
        if (pkg == "scales") FALSE else TRUE
      },
      .package = "base"
    )
  })

  it("returns without error when both packages are available", {
    testthat::with_mocked_bindings(
      code = {
        expect_no_error(aNCA:::req_sbs_pkgs())
      },
      requireNamespace = function(pkg, quietly = FALSE) TRUE,
      .package = "base"
    )
  })
})
