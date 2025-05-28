adpc <- FIXTURE_CONC_DATA %>%
  filter(USUBJID %in% unique(USUBJID)[1:3])
attr(adpc$USUBJID, "label") <- "Subject ID"
attr(adpc$AVAL, "label") <- "Analysis value"
attr(adpc$DOSEU, "label") <- "Dose unit"

describe("pkcg01", {
  it("generates valid ggplots with LIN scale", {
    plots_lin <- pkcg01(adpc, scale = "LIN", plotly = FALSE)
    expect_equal(length(plots_lin), 3)

    vdiffr::expect_doppelganger("lin_plot1", plots_lin[[1]])
    vdiffr::expect_doppelganger("lin_plot2", plots_lin[[2]])
    vdiffr::expect_doppelganger("lin_plot3", plots_lin[[3]])
  })

  it("generates plotly plots with LIN scale", {
    plotlys_lin <- pkcg01(adpc, scale = "LIN", plotly = TRUE)
    expect_equal(length(plotlys_lin), 3)
    expect_true(inherits(plotlys_lin[[1]], "plotly"))
  })

  it("generates valid ggplots with LOG scale", {
    plots_log <- pkcg01(adpc, scale = "LOG", plotly = FALSE)
    expect_equal(length(plots_log), 3)

    vdiffr::expect_doppelganger("log_plot1", plots_log[[1]])
    vdiffr::expect_doppelganger("log_plot2", plots_log[[2]])
    vdiffr::expect_doppelganger("log_plot3", plots_log[[3]])
  })

  it("generates plotly plots with LOG scale", {
    plotlys_log <- pkcg01(adpc, scale = "LOG", plotly = TRUE)
    expect_equal(length(plotlys_log), 3)
    expect_true(inherits(plotlys_log[[1]], "plotly"))
  })

  it("generates valid ggplots with SBS scale", {
    plots_sbs <- pkcg01(adpc, scale = "SBS", plotly = FALSE)
    expect_equal(length(plots_sbs), 3)

    vdiffr::expect_doppelganger("sbs_plot1", plots_sbs[[1]])
    vdiffr::expect_doppelganger("sbs_plot2", plots_sbs[[2]])
    vdiffr::expect_doppelganger("sbs_plot3", plots_sbs[[3]])
  })

  it("generates plotly plots with SBS scale", {
    plotlys_sbs <- pkcg01(adpc, scale = "SBS", plotly = TRUE)
    expect_equal(length(plotlys_sbs), 3)
    expect_true(inherits(plotlys_sbs[[1]], "plotly"))
  })

  it("generates plots with custom labels for LIN scale", {
    plots_lin <- pkcg01(
      adpc,
      scale = "LIN",
      xlab = "Custom X Label",
      ylab = "Custom Y Label",
      title = "Custom Title",
      subtitle = "Custom Subtitle",
      footnote = "Custom Footnote",
      plotly = FALSE
    )
    plot <- plots_lin[[1]]

    expect_equal(plot$labels$x, "Custom X Label")
    expect_equal(plot$labels$y, "Custom Y Label")
    expect_equal(plot$labels$title, "Custom Title")
    expect_equal(plot$labels$subtitle, "Custom Subtitle")
    expect_equal(plot$labels$caption, "Custom Footnote")
  })

  it("generates plots with custom multiple colors for LIN scale", {
    plots_lin_colors <- pkcg01(
      adpc,
      scale = "LIN",
      xlab = "Custom X Label",
      ylab = "Custom Y Label",
      title = "Custom Title",
      subtitle = "Custom Subtitle",
      footnote = "Custom Footnote",
      plotly = FALSE,
      color_var = "NCA_PROFILE",
      color = c("red", "blue", "green")
    )
    plot_lin_colors <- plots_lin_colors[[2]]
    vdiffr::expect_doppelganger("lin_plot2_custom_colors", plot_lin_colors)
  })

  it("returns error if missing ggh4x package for SBS scale", {
    # Temporarily mock requireNamespace to simulate ggh4x not being available
    testthat::with_mocked_bindings(
      code = {
        expect_error(
          pkcg01(adpc, scale = "SBS", plotly = FALSE),
          "Side-by-side view requires `ggh4x` package, please install it with"
        )
      },
      `requireNamespace` = function(pkg, quietly = FALSE) {
        if (pkg == "ggh4x") return(FALSE) else TRUE
      },
      .package = "base"
    )
  })
  it("returns error if missing scales package for SBS scale", {
    # Temporarily mock requireNamespace to simulate ggh4x not being available
    testthat::with_mocked_bindings(
      code = {
        expect_error(
          pkcg01(adpc, scale = "SBS", plotly = FALSE),
          "Side-by-side view requires `scales` package, please install it with"
        )
      },
      `requireNamespace` = function(pkg, quietly = FALSE) {
        if (pkg == "scales") return(FALSE) else TRUE
      },
      .package = "base"
    )
  })
})

describe("g_pkconc_ind_lin", {
  it("generates plot with linear scale", {
    plot_lin <- g_pkconc_ind_lin(adpc, plotly = FALSE)[[1]]
    expect_equal(plot_lin$labels$y, "Analysis value [mg/L]")
    vdiffr::expect_doppelganger("g_pkconc_ind_lin_plot", plot_lin)
  })
})

describe("g_pkconc_ind_log", {
  it("generates plot with log scale", {
    plot_log <- g_pkconc_ind_log(adpc, plotly = FALSE)[[1]]
    expect_equal(plot_log$labels$y, "Analysis value [mg/L]")
    vdiffr::expect_doppelganger("g_pkconc_ind_log_plot", plot_log)
  })
})
