library(testthat)
library(plotly)
library(dplyr)

test_that("flexible_violinboxplot generates a plotly object with box plot", {
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

  # Define parameters
  parameter <- "cmax"
  xvars <- c("DOSEA")
  colorvars <- c("DOSNO")
  varvalstofilter <- c("DOSEA: Low", "DOSNO: 1")
  columns_to_hover <- c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE")
  box <- TRUE

  # Call the function
  plot <- flexible_violinboxplot(
    boxplotdata = boxplotdata,
    parameter = parameter,
    xvars = xvars,
    colorvars = colorvars,
    varvalstofilter = varvalstofilter,
    columns_to_hover = columns_to_hover,
    box = box
  )

  # Check if the output is a plotly object
  expect_s3_class(plot, "plotly")
})

test_that("flexible_violinboxplot generates a plotly object with violin plot", {
  # Create a sample boxplotdata
  boxplotdata <- data.frame(
    PPTESTCD = rep("cmax", 10),
    PPSTRES = rnorm(10, mean = 15, sd = 7),
    PPSTRESU = rep("ng/mL", 10),
    DOSEA = rep(c("Low", "High"), each = 5),
    DOSNO = rep(1:2, each = 5),
    USUBJID = rep(11106:11110, each = 2),
    AGE = rep(55:59, each = 2),
    SEX = rep(c("F", "M"), each = 5),
    ANALYTE = rep("Analyte02", 10)
  )

  # Define parameters
  parameter <- "cmax"
  xvars <- c("DOSEA")
  colorvars <- c("DOSNO")
  varvalstofilter <- c("DOSEA: High", "DOSNO: 2")
  columns_to_hover <- c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE")
  box <- FALSE

  # Call the function
  plot <- flexible_violinboxplot(
    boxplotdata = boxplotdata,
    parameter = parameter,
    xvars = xvars,
    colorvars = colorvars,
    varvalstofilter = varvalstofilter,
    columns_to_hover = columns_to_hover,
    box = box
  )

  # Check if the output is a plotly object
  expect_s3_class(plot, "plotly")
})

test_that("flexible_violinboxplot handles missing data gracefully", {
  # Create a sample boxplotdata with missing values
  boxplotdata <- data.frame(
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

  # Call the function
  plot <- flexible_violinboxplot(
    boxplotdata = boxplotdata,
    parameter = parameter,
    xvars = xvars,
    colorvars = colorvars,
    varvalstofilter = varvalstofilter,
    columns_to_hover = columns_to_hover,
    box = box
  )

  # Check if the output is a plotly object
  expect_s3_class(plot, "plotly")
})
