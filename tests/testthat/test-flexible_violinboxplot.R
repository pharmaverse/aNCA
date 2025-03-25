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

test_that("flexible_violinboxplot with minimal arguments", {
  # Define parameters
  parameter <- "cmax"
  xvars <- c("DOSEA")
  colorvars <- c("DOSNO")
  varvalstofilter <- c("DOSEA: Low", "DOSNO: 1")
  columns_to_hover <- c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE")
  box <- TRUE
  
  # Call the function with minimal arguments
  plot <- flexible_violinboxplot(
    boxplotdata = boxplotdata,
    parameter = parameter,
    xvars = xvars,
    colorvars = colorvars,
    varvalstofilter = varvalstofilter,
    columns_to_hover = columns_to_hover,
    box = box,
    plotly = FALSE
  )
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot = plot)
  expect_snapshot_file(temp_file, "flexible_violinboxplot-minargs.png")
})

test_that("flexible_violinboxplot with additional xvars", {
  # Define parameters
  parameter <- "cmax"
  xvars <- c("DOSEA", "SEX")
  colorvars <- c("DOSNO")
  varvalstofilter <- c("DOSEA: Low", "DOSNO: 1")
  columns_to_hover <- c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE")
  box <- TRUE
  
  # Call the function with additional xvars
  plot <- flexible_violinboxplot(
    boxplotdata = boxplotdata,
    parameter = parameter,
    xvars = xvars,
    colorvars = colorvars,
    varvalstofilter = varvalstofilter,
    columns_to_hover = columns_to_hover,
    box = box,
    plotly = FALSE
  )
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot = plot)
  expect_snapshot_file(temp_file, "flexible_violinboxplot-xvars.png")
})

test_that("flexible_violinboxplot with additional colorvars", {
  # Define parameters
  parameter <- "cmax"
  xvars <- c("DOSEA")
  colorvars <- c("DOSNO", "SEX")
  varvalstofilter <- c("DOSEA: Low", "DOSNO: 1")
  columns_to_hover <- c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE")
  box <- TRUE
  
  # Call the function with additional colorvars
  plot <- flexible_violinboxplot(
    boxplotdata = boxplotdata,
    parameter = parameter,
    xvars = xvars,
    colorvars = colorvars,
    varvalstofilter = varvalstofilter,
    columns_to_hover = columns_to_hover,
    box = box,
    plotly = FALSE
  )
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot = plot)
  expect_snapshot_file(temp_file, "flexible_violinboxplot-colorvars.png")
})

test_that("flexible_violinboxplot with different varvalstofilter", {
  # Define parameters
  parameter <- "cmax"
  xvars <- c("DOSEA")
  colorvars <- c("DOSNO")
  varvalstofilter <- c("DOSEA: High", "DOSNO: 2")
  columns_to_hover <- c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE")
  box <- TRUE
  
  # Call the function with different varvalstofilter
  plot <- flexible_violinboxplot(
    boxplotdata = boxplotdata,
    parameter = parameter,
    xvars = xvars,
    colorvars = colorvars,
    varvalstofilter = varvalstofilter,
    columns_to_hover = columns_to_hover,
    box = box,
    plotly = FALSE
  )
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot = plot)
  expect_snapshot_file(temp_file, "flexible_violinboxplot-varvalstofilter.png")
})

test_that("flexible_violinboxplot with violin plot", {
  # Define parameters
  parameter <- "cmax"
  xvars <- c("DOSEA")
  colorvars <- c("DOSNO")
  varvalstofilter <- c("DOSEA: Low", "DOSNO: 1")
  columns_to_hover <- c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE")
  box <- FALSE
  
  # Call the function with violin plot
  plot <- flexible_violinboxplot(
    boxplotdata = boxplotdata,
    parameter = parameter,
    xvars = xvars,
    colorvars = colorvars,
    varvalstofilter = varvalstofilter,
    columns_to_hover = columns_to_hover,
    box = box,
    plotly = FALSE
  )
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot = plot)
  expect_snapshot_file(temp_file, "flexible_violinboxplot-violin.png")
})

test_that("flexible_violinboxplot with missing data", {
  # Create a sample boxplotdata with missing values
  boxplotdata_missing <- data.frame(
    PPTESTCD = rep("cmax", 10),
    PPSTRES = c(rnorm(9, mean = 20, sd = 10), NA),
    PPSTRESU = rep("ng/mL", 10),
    DOSEA = rep(c("Low", "High"), each = 5),
    DOSNO = rep(1:2, each = 5),
    USUBJID = rep(11111:11115, each = 2),
    AGE = rep(60:64, each = 2),
    SEX = rep(c("M", "F"), each = 5),
    ANALYTE = rep("Analyte03", 10)
  )
  
  # Define parameters
  parameter <- "cmax"
  xvars <- c("DOSEA")
  colorvars <- c("DOSNO")
  varvalstofilter <- c("DOSEA: Low", "DOSNO: 1")
  columns_to_hover <- c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE")
  box <- TRUE
  
  # Call the function with missing data
  plot <- flexible_violinboxplot(
    boxplotdata = boxplotdata_missing,
    parameter = parameter,
    xvars = xvars,
    colorvars = colorvars,
    varvalstofilter = varvalstofilter,
    columns_to_hover = columns_to_hover,
    box = box,
    plotly = FALSE
  )
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot = plot)
  expect_snapshot_file(temp_file, "flexible_violinboxplot-missing_data.png")
})
