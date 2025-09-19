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

    expect_equal(simple_plot$labels$x, "DOSEA")
    expect_equal(simple_plot$labels$colour, "NCA_PROFILE")
    expect_true(grepl("CMAX", simple_plot$labels$y))
    expect_true(any("ggplot" %in% class(simple_plot)))
    expect_equal(c(1, 2, 6), unique(simple_plot$data$USUBJID))
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

    expect_equal(xvars_plot$labels$x, "DOSEA, PARAM")
    expect_equal(xvars_plot$labels$colour, "NCA_PROFILE")
    expect_true(grepl("CMAX", xvars_plot$labels$y))
    expect_equal(c(1, 2, 6), unique(xvars_plot$data$USUBJID))
    expect_true(any("ggplot" %in% class(xvars_plot)))
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

    expect_equal(colorvars_plot$labels$x, "DOSEA")
    expect_equal(colorvars_plot$labels$colour, "NCA_PROFILE, PARAM")
    expect_true(grepl("CMAX", colorvars_plot$labels$y))
    expect_equal(c(1, 2, 6), unique(colorvars_plot$data$USUBJID))
    expect_true(any("ggplot" %in% class(colorvars_plot)))
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

    expect_equal(varvalstofilter_plot$labels$x, "DOSEA")
    expect_equal(varvalstofilter_plot$labels$colour, "NCA_PROFILE")
    expect_true(grepl("CMAX", varvalstofilter_plot$labels$y))
    expect_equal(c(1, 2, 6), unique(varvalstofilter_plot$data$USUBJID))
    expect_equal(1, unique(varvalstofilter_plot$data$NCA_PROFILE))
    expect_true(any("ggplot" %in% class(varvalstofilter_plot)))
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

    expect_equal(violin_plot$labels$x, "DOSEA")
    expect_equal(violin_plot$labels$colour, "NCA_PROFILE")
    expect_true(grepl("CMAX", violin_plot$labels$y))
    expect_equal(c(1, 2, 3), unique(violin_plot$data$USUBJID))
    expect_true(any("ggplot" %in% class(violin_plot)))
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

    expect_equal(missing_plot$labels$x, "DOSEA")
    expect_equal(missing_plot$labels$colour, "NCA_PROFILE")
    expect_true(grepl("CMAX", missing_plot$labels$y))
    expect_equal(c(1, 2, 6), unique(missing_plot$data$USUBJID))
    expect_true(any("ggplot" %in% class(missing_plot)))
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
