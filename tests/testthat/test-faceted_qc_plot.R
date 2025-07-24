CONC_DATA <- data.frame(
  USUBJID = rep(paste0("S-", 1:2), each = 2),
  ACTUAL_TIME = c(0, 24, 0, 24),
  SAMPLE_TYPE = rep(c("PLASMA", "URINE"), 2),
  COHORT = "A",
  TIME_UNIT = "hr"
)

DOSE_DATA <- data.frame(
  USUBJID = rep(paste0("S-", 1:2), each = 1),
  ACTUAL_TIME = c(0, 0),
  DOSE_LEVEL = c(100, 100),
  COHORT = "A",
  DOSE_UNIT = "mg",
  TIME_UNIT = "hr"
)

DUMMY_LABELS_DF <- data.frame(
  Variable = character(),
  Label = character(),
  Dataset = character()
)

describe("faceted_qc_plot", {

  it("returns a ggplot object when as_plotly is FALSE", {
    p <- faceted_qc_plot(
      data_conc = CONC_DATA, x_var = "ACTUAL_TIME", y_var = "USUBJID",
      colour_var = "DOSE_LEVEL", shape_var = "SAMPLE_TYPE", grouping_vars = "COHORT",
      as_plotly = FALSE
    )
    expect_s3_class(p, "ggplot")
  })

  it("returns a plotly object when as_plotly is TRUE", {
    p <- faceted_qc_plot(
      data_conc = CONC_DATA, data_dose = DOSE_DATA,
      x_var = "ACTUAL_TIME", y_var = "USUBJID",
      colour_var = "DOSE_LEVEL", shape_var = "SAMPLE_TYPE", grouping_vars = "COHORT",
      as_plotly = TRUE
    )
    expect_s3_class(p, "plotly")
  })

  it("plots only concentration data when show_doses is FALSE", {
    p <- faceted_qc_plot(
      data_conc = CONC_DATA, data_dose = DOSE_DATA, show_doses = FALSE,
      x_var = "ACTUAL_TIME", y_var = "USUBJID",
      colour_var = "DOSE_LEVEL", shape_var = "SAMPLE_TYPE", grouping_vars = "COHORT"
    )
    expect_false("100" %in% as.character(p$data$legend_group))
    expect_true("PLASMA" %in% as.character(p$data$legend_group))
  })

  it("plots only dose data when show_pk_samples is FALSE", {
    p <- faceted_qc_plot(
      data_conc = CONC_DATA, data_dose = DOSE_DATA, show_pk_samples = FALSE,
      x_var = "ACTUAL_TIME", y_var = "USUBJID",
      colour_var = "DOSE_LEVEL", shape_var = "SAMPLE_TYPE", grouping_vars = "COHORT"
    )
    expect_false("PLASMA" %in% as.character(p$data$legend_group))
    expect_true("100" %in% as.character(p$data$legend_group))
  })

  it("returns an empty plot when no data is provided or shown", {
    p_null <- faceted_qc_plot(
      data_conc = NULL, data_dose = NULL,
      x_var = "ACTUAL_TIME", y_var = "USUBJID",
      colour_var = "DOSE_LEVEL", shape_var = "SAMPLE_TYPE", grouping_vars = "COHORT"
    )
    expect_length(p_null$layers, 0)

    p_hidden <- faceted_qc_plot(
      data_conc = CONC_DATA, data_dose = DOSE_DATA,
      show_pk_samples = FALSE, show_doses = FALSE,
      x_var = "ACTUAL_TIME", y_var = "USUBJID",
      colour_var = "DOSE_LEVEL", shape_var = "SAMPLE_TYPE", grouping_vars = "COHORT"
    )
    expect_length(p_hidden$layers, 0)
  })

  it("appends units to labels correctly when units are unique", {
    p <- faceted_qc_plot(
      data_conc = CONC_DATA, data_dose = DOSE_DATA,
      x_var = "ACTUAL_TIME", y_var = "USUBJID",
      colour_var = "DOSE_LEVEL", shape_var = "SAMPLE_TYPE", grouping_vars = "COHORT",
      x_var_units = "TIME_UNIT"
    )

    expect_true(grepl("(hr)", p$labels$x, fixed = TRUE))
  })

  it("omits units from labels when units are not unique", {
    conc_data_multi_unit <- CONC_DATA
    conc_data_multi_unit$TIME_UNIT <- c("hr", "min", "hr", "min")

    p <- faceted_qc_plot(
      data_conc = conc_data_multi_unit, data_dose = DOSE_DATA,
      x_var = "ACTUAL_TIME", y_var = "USUBJID",
      colour_var = "DOSE_LEVEL", shape_var = "SAMPLE_TYPE", grouping_vars = "COHORT",
      x_var_units = "TIME_UNIT"
    )

    expect_false(grepl("(", p$labels$x, fixed = TRUE))
  })
})


TEST_DATA <- data.frame(
  TIME_UNIT = c("hr", "hr", "hr"),
  MULTI_UNIT = c("mg", "kg", "mg"),
  NA_UNIT = c("A", NA, "A")
)

describe("format_unit_string", {

  it("returns a formatted string when exactly one unique unit exists", {
    result <- format_unit_string(TEST_DATA, "TIME_UNIT")
    expect_equal(result, " (hr)")
  })

  it("returns an empty string when multiple unique units exist", {
    result <- format_unit_string(TEST_DATA, "MULTI_UNIT")
    expect_equal(result, "")
  })

  it("returns an empty string when a mix of values and NA exist", {
    result <- format_unit_string(TEST_DATA, "NA_UNIT")
    expect_equal(result, "")
  })

  it("returns an empty string when the unit variable is NULL", {
    result <- format_unit_string(TEST_DATA, NULL)
    expect_equal(result, "")
  })

  it("returns an empty string when the unit variable does not exist in the data", {
    result <- format_unit_string(TEST_DATA, "NON_EXISTENT_COLUMN")
    expect_equal(result, "")
  })

  it("returns an empty string for data with zero rows", {
    empty_data <- TEST_DATA[0, ]
    result <- format_unit_string(empty_data, "TIME_UNIT")
    expect_equal(result, "")
  })
})