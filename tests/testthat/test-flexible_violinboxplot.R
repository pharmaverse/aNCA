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

## Note: If you want to create a new checking plot for a test follow these steps:
#
## 1) Load the object:
# FIXTURE_PLOTS = readRDS("tests/testthat/data/FIXTURE_PLOTS.rds") # nolint
#
## 2) Store in the list your plot:
# FIXTURE_PLOTS$flexible_violinboxplot$<<unique plot name>>
#
## 3) Save the object back:
# saveRDS(
#   FIXTURE_PLOTS,
#   file = "tests/testthat/data/FIXTURE_PLOTS.rds", # nolint
#   compress = "xz" # nolint
# )

# Helper function to compare two plotly objects
expect_equal_plotly <- function(actual, expected, ignore_fields = list()) {
  # Extract relevant parts of the plotly objects
  extract_relevant <- function(plotly_obj) {
    list(
      data = plotly_obj$x$data,
      layout = plotly_obj$x$layout
    )
  }

  # Remove ignored fields from the plotly object
  remove_ignored_fields <- function(obj, ignore_fields) {
    for (field in ignore_fields) {
      # Use eval(parse()) to dynamically access nested fields
      eval(parse(text = paste0("obj$", field, " <- NULL")))
    }
    obj
  }

  # Extract relevant parts
  actual_relevant <- extract_relevant(actual)
  expected_relevant <- extract_relevant(expected)

  # Apply ignore_fields to both actual and expected objects
  actual_relevant <- remove_ignored_fields(actual_relevant, ignore_fields)
  expected_relevant <- remove_ignored_fields(expected_relevant, ignore_fields)

  # Compare the relevant parts
  expect_equal(actual_relevant, expected_relevant)
}

.expect_equal_violinboxplot <- function(act_obj, exp_obj) {
  expect_equal_plotly(
    act_obj,
    exp_obj,
    # Dots in boxplots randomize the X axis position
    ignore_fields = c("data[[2]]$x")
  )
}

describe("flexible_violinboxplot", {

  testing_plots <- FIXTURE_PLOTS$flexible_violinboxplot

  it("creates a simple plot with minimal arguments", {
    simple_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = TRUE
    )
    .expect_equal_violinboxplot(
      simple_plot,
      testing_plots$simple_plot
    )
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
      plotly = TRUE
    )
    .expect_equal_violinboxplot(
      xvars_plot,
      testing_plots$xvars_plot
    )
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
      plotly = TRUE
    )
    .expect_equal_violinboxplot(
      colorvars_plot,
      testing_plots$colorvars_plot
    )
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
      plotly = TRUE
    )
    .expect_equal_violinboxplot(
      varvalstofilter_plot,
      testing_plots$varvalstofilter_plot
    )
  })

  it("creates a violin plot when box = FALSE", {
    violin_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = FALSE,
      plotly = TRUE
    )
    .expect_equal_violinboxplot(
      violin_plot,
      testing_plots$violin_plot
    )
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
      plotly = TRUE
    )
    .expect_equal_violinboxplot(
      missing_plot,
      testing_plots$missing_plot
    )
  })

  it("produces ggplot objects when plotly = FALSE", {
    ggplot_plot <- flexible_violinboxplot(
      boxplotdata = boxplotdata,
      parameter = "cmax",
      xvars = c("DOSEA"),
      colorvars = c("DOSNO"),
      varvalstofilter = c("DOSEA: Low", "DOSNO: 1"),
      columns_to_hover = c("DOSEA", "DOSNO", "USUBJID", "AGE", "SEX", "ANALYTE"),
      box = TRUE,
      plotly = FALSE
    )
    expect_true(inherits(ggplot_plot, "ggplot"))
    expect_equal(
      ggplot_plot,
      testing_plots$ggplot_plot
    )
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
})
