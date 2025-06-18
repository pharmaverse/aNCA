# Create a sample boxplotdata
boxplotdata <- FIXTURE_PKNCA_RES$result %>%
  filter(USUBJID %in% 1:7)

describe("flexible_violinboxplot", {
  it("creates a simple plot with minimal arguments", {
    simple_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "NCA_PROFILE",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      columns_to_hover = c("DOSEA", "USUBJID", "NCA_PROFILE", "PARAM"),
      box = TRUE,
      plotly = FALSE
    )

    vdiffr::expect_doppelganger("simple_plot", simple_plot)
  })

  it("creates a plot with additional xvars", {
    xvars_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "CMAX",
      xvars = c("DOSEA", "PARAM"),
      colorvars = "NCA_PROFILE",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      columns_to_hover = c("DOSEA", "USUBJID", "NCA_PROFILE", "PARAM"),
      box = TRUE,
      plotly = FALSE
    )

    vdiffr::expect_doppelganger("xvars_plot", xvars_plot)
  })

  it("creates a plot with additional colorvars", {
    colorvars_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = c("NCA_PROFILE", "PARAM"),
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      columns_to_hover = c("DOSEA", "USUBJID", "NCA_PROFILE", "PARAM"),
      box = TRUE,
      plotly = FALSE
    )

    vdiffr::expect_doppelganger("colorvars_plot", colorvars_plot)
  })

  it("creates a plot with additional varvalstofilter", {
    varvalstofilter_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "NCA_PROFILE",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6", "NCA_PROFILE: 1"),
      columns_to_hover = c("DOSEA", "USUBJID", "NCA_PROFILE", "PARAM"),
      box = TRUE,
      plotly = FALSE
    )

    vdiffr::expect_doppelganger("varvalstofilter_plot", varvalstofilter_plot)
  })

  it("creates a violin plot when box = FALSE", {
    violin_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "NCA_PROFILE",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 3"),
      columns_to_hover = c("DOSEA", "USUBJID", "NCA_PROFILE", "PARAM"),
      box = FALSE,
      plotly = FALSE
    )

    vdiffr::expect_doppelganger("violin_plot", violin_plot)
  })

  it("handles missing data gracefully", {
    boxplotdata_missing <- boxplotdata %>%
      mutate(PPSTRES = NA)
    missing_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata_missing,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "NCA_PROFILE",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      columns_to_hover = c("DOSEA", "USUBJID", "NCA_PROFILE", "PARAM"),
      box = TRUE,
      plotly = FALSE
    )

    vdiffr::expect_doppelganger("missing_plot", missing_plot)
  })

  it("handles axis labels correctly when parameter has no unit", {
    plot_with_param_unit <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "NCA_PROFILE",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      columns_to_hover = c("DOSEA", "USUBJID", "NCA_PROFILE", "PARAM"),
      box = TRUE,
      plotly = TRUE
    )
    expect_true(grepl("\\[ ng/mL", plot_with_param_unit$x$layout$yaxis$title$text))

    plot_wo_param_unit <- flexible_violinboxplot(
      boxplotdata = boxplotdata %>% mutate(PPSTRESU = ""),
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "NCA_PROFILE",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      columns_to_hover = c("DOSEA", "USUBJID", "NCA_PROFILE", "PARAM"),
      box = TRUE,
      plotly = TRUE
    )
    expect_false(grepl("\\[", plot_wo_param_unit$x$layout$yaxis$title$text))
  })

  it("creates a plotly object correctly", {
    simple_plotly <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "NCA_PROFILE",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      columns_to_hover = c("DOSEA", "USUBJID", "NCA_PROFILE", "PARAM"),
      box = TRUE,
      plotly = TRUE
    )
    expect_s3_class(simple_plotly, "plotly")
  })
})
