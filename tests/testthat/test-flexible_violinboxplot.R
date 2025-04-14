library(testthat)
library(plotly)
library(dplyr)
library(ggplot2)

# Create a sample boxplotdata
boxplotdata <- data.frame(
  PPTESTCD = rep("cmax", 10),
  PPSTRES = rnorm(10, mean = 10, sd = 5),
  PPSTRESU = rep("ng/mL", 10),
  DOSEA = rep(c("Low", "High"), each = 5),
  DOSNO = rep(1:2, each = 5),
  USUBJID = rep(11101:11105, each = 2),
  AGE = rep(50:54, each = 2),
  SEX = rep(c("M", "F"), each = 5),
  ANALYTE = rep("Analyte01", 10)
)

describe("flexible_violinboxplot", {
  
  it("creates a plot with minimal arguments", {
    plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = FALSE
    )
    temp_file <- tempfile(fileext = ".png")
    ggsave(temp_file, plot = plot)
    expect_snapshot_file(temp_file, "flexible_violinboxplot-minargs.png")
  })
  
  it("creates a plot with additional xvars", {
    plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA", "SEX"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = FALSE
    )
    temp_file <- tempfile(fileext = ".png")
    ggsave(temp_file, plot = plot)
    expect_snapshot_file(temp_file, "flexible_violinboxplot-xvars.png")
  })
  
  it("creates a plot with additional colorvars", {
    plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO", "SEX"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = FALSE
    )
    temp_file <- tempfile(fileext = ".png")
    ggsave(temp_file, plot = plot)
    expect_snapshot_file(temp_file, "flexible_violinboxplot-colorvars.png")
  })
  
  it("creates a plot with different varvalstofilter", {
    plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: High", "DOSNO: 2"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = FALSE
    )
    temp_file <- tempfile(fileext = ".png")
    ggsave(temp_file, plot = plot)
    expect_snapshot_file(temp_file, "flexible_violinboxplot-varvalstofilter.png")
  })
  
  it("creates a violin plot when box = FALSE", {
    plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = FALSE,
      plotly = FALSE
    )
    temp_file <- tempfile(fileext = ".png")
    ggsave(temp_file, plot = plot)
    expect_snapshot_file(temp_file, "flexible_violinboxplot-violin.png")
  })
  
  it("handles missing data gracefully", {
    boxplotdata_missing <- boxplotdata %>%
      mutate(PPSTRES = replace(PPSTRES, 10, NA))
    plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata_missing,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = FALSE
    )
    temp_file <- tempfile(fileext = ".png")
    ggsave(temp_file, plot = plot)
    expect_snapshot_file(temp_file, "flexible_violinboxplot-missing_data.png")
  })
  
  it("produces plotly objects when plotly = TRUE", {
    plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = TRUE
    )
    expect_true(inherits(plot, "plotly"))
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
    expect_true(grepl("\\[", plot_with_param_unit$x$layout$yaxis$title$text))
    
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
  
})
