# Create a sample boxplotdata
boxplotdata <- data.frame(
  PPTESTCD = rep("cmax", 10),
  PPSTRES = 1:10,
  PPSTRESU = rep("ng/mL", 10),
  DOSEA = rep(c("Low", "High"), each = 5),
  DOSNO = rep(1:2, each = 5),
  USUBJID = rep(11101:11105, each = 2),
  AGE = rep(50:54, each = 2),
  SEX = rep(c("M", "F"), each = 5),
  ANALYTE = rep("Analyte01", 10)
)

describe("flexible_violinboxplot", {
  it("creates a simple plot with minimal arguments", {
    simple_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = FALSE
    )

    vdiffr::expect_doppelganger("simple_plot", simple_plot)
  })

  it("creates a plot with additional xvars", {
    xvars_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA", "SEX"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = FALSE
    )

    vdiffr::expect_doppelganger("xvars_plot", xvars_plot)
  })

  it("creates a plot with additional colorvars", {
    colorvars_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO", "SEX"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = FALSE
    )

    vdiffr::expect_doppelganger("colorvars_plot", colorvars_plot)
  })

  it("creates a plot with different varvalstofilter", {
    varvalstofilter_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: High", "DOSNO: 2"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = FALSE
    )

    vdiffr::expect_doppelganger("varvalstofilter_plot", varvalstofilter_plot)
  })

  it("creates a violin plot when box = FALSE", {
    violin_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = "DOSEA",
      colorvars = "DOSNO",
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
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
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = FALSE
    )

    vdiffr::expect_doppelganger("missing_plot", missing_plot)
  })

  it("handles axis labels correctly when parameter has no unit", {
    plot_with_param_unit <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = TRUE
    )
    expect_true(grepl("\\[ ng/mL", plot_with_param_unit$x$layout$yaxis$title$text))

    plot_wo_param_unit <- flexible_violinboxplot(
      boxplotdata = boxplotdata %>% mutate(PPSTRESU = ""),
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = TRUE
    )
    expect_false(grepl("\\[", plot_wo_param_unit$x$layout$yaxis$title$text))
  })

  it("creates a plotly object correctly", {
    simple_plotly <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = TRUE
    )

    expect_s3_class(simple_plotly, "plotly")
  })
})
