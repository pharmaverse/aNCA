# Source the Shiny helper to test pure utility functions
source(
  file.path(system.file("shiny", package = "aNCA"), "functions", "build_plot_code.R"),
  local = TRUE
)

describe(".fmt_chr", {
  it("returns NULL string for NULL input", {
    expect_equal(.fmt_chr(NULL), "NULL")
  })

  it("returns NULL string for empty vector", {
    expect_equal(.fmt_chr(character(0)), "NULL")
  })

  it("quotes a single string", {
    expect_equal(.fmt_chr("x"), "\"x\"")
  })

  it("wraps multiple strings in c()", {
    expect_equal(.fmt_chr(c("a", "b")), "c(\"a\", \"b\")")
  })

  it("escapes double quotes in values", {
    result <- .fmt_chr("val\"ue")
    expect_true(grepl("\\\\\"", result))
  })
})

describe(".fmt_lgl", {
  it("returns NULL string for NULL input", {
    expect_equal(.fmt_lgl(NULL), "NULL")
  })

  it("returns TRUE string for TRUE", {
    expect_equal(.fmt_lgl(TRUE), "TRUE")
  })

  it("returns FALSE string for FALSE", {
    expect_equal(.fmt_lgl(FALSE), "FALSE")
  })

  it("returns FALSE for non-logical input", {
    expect_equal(.fmt_lgl("yes"), "FALSE")
  })
})

describe(".fmt_num", {
  it("returns NULL string for NULL input", {
    expect_equal(.fmt_num(NULL), "NULL")
  })

  it("returns NULL string for empty vector", {
    expect_equal(.fmt_num(numeric(0)), "NULL")
  })

  it("returns NULL string for all non-finite values", {
    expect_equal(.fmt_num(c(Inf, -Inf, NaN)), "NULL")
  })

  it("formats a single number", {
    expect_equal(.fmt_num(42), "42")
  })

  it("wraps multiple numbers in c()", {
    expect_equal(.fmt_num(c(1, 2.5)), "c(1, 2.5)")
  })
})

describe(".fmt_obj", {
  it("returns NULL string for NULL input", {
    expect_equal(.fmt_obj(NULL), "NULL")
  })

  it("deparses a list", {
    result <- .fmt_obj(list(a = 1, b = "x"))
    expect_true(grepl("list", result))
    expect_true(grepl("a = 1", result))
  })
})

describe(".build_filter_code", {
  it("returns NULL assignment for NULL input", {
    expect_equal(.build_filter_code(NULL), "filtering_list <- NULL")
  })

  it("returns NULL assignment for empty list", {
    expect_equal(.build_filter_code(list()), "filtering_list <- NULL")
  })

  it("generates valid list code for named filters", {
    result <- .build_filter_code(list(DOSEA = c("10", "20"), TRT = "A"))
    expect_true(grepl("filtering_list <- list", result))
    expect_true(grepl("DOSEA", result))
    expect_true(grepl("TRT", result))
  })
})

describe(".build_individual_call", {
  it("generates exploration_individualplot call", {
    inputs <- list(
      color_by = "USUBJID", facet_by = NULL, show_facet_n = FALSE,
      ylog_scale = FALSE, show_legend = TRUE, show_dose = FALSE,
      threshold_value = NULL, x_limits = NULL, y_limits = NULL,
      use_time_since_last_dose = FALSE, palette = "default",
      y_axis_values = "lines"
    )
    result <- .build_individual_call(inputs)
    expect_true(grepl("aNCA::exploration_individualplot", result$call))
    expect_true(grepl("color_by = \"USUBJID\"", result$call))
    expect_null(result$filter_override)
  })
})

describe(".build_mean_call", {
  it("generates exploration_meanplot call", {
    inputs <- list(
      color_by = "TRT01A", facet_by = "DOSEA", show_facet_n = TRUE,
      ylog_scale = TRUE, show_legend = TRUE, show_dose = FALSE,
      threshold_value = 10, x_limits = c(0, 24), y_limits = NULL,
      sd_min = TRUE, sd_max = TRUE, ci = FALSE,
      use_time_since_last_dose = FALSE, palette = "plasma",
      y_axis_values = "lines"
    )
    result <- .build_mean_call(inputs)
    expect_true(grepl("aNCA::exploration_meanplot", result$call))
    expect_true(grepl("sd_min = TRUE", result$call))
    expect_true(grepl("facet_by = \"DOSEA\"", result$call))
    expect_null(result$filter_override)
  })
})

describe(".build_qc_call", {
  it("generates pk_dose_qc_plot call", {
    inputs <- list(
      colour_var = "DOSEA", group_var = "TRT01A",
      usubjid = NULL, pcspec = NULL,
      show_samples_doses = c("PK Samples", "Doses")
    )
    result <- .build_qc_call(inputs)
    expect_true(grepl("aNCA::pk_dose_qc_plot", result$call))
    expect_true(grepl("show_pk_samples <- TRUE", result$call))
    expect_equal(result$filter_override, "")
  })

  it("includes subject filter when usubjid is provided", {
    inputs <- list(
      colour_var = "DOSEA", group_var = "TRT01A",
      usubjid = c("SUBJ01", "SUBJ02"), pcspec = NULL,
      show_samples_doses = "PK Samples"
    )
    result <- .build_qc_call(inputs)
    expect_true(grepl("subj_col", result$call))
    expect_true(grepl("SUBJ01", result$call))
  })

  it("includes specimen filter when pcspec is provided", {
    inputs <- list(
      colour_var = "DOSEA", group_var = "TRT01A",
      usubjid = NULL, pcspec = c("PLASMA"),
      show_samples_doses = "Doses"
    )
    result <- .build_qc_call(inputs)
    expect_true(grepl("PCSPEC", result$call))
    expect_true(grepl("PLASMA", result$call))
  })
})

describe("build_plot_code", {
  mock_session <- list(
    userData = list(
      mapping = list(conc = "AVAL", time = "AFRLT"),
      applied_filters = NULL,
      time_duplicate_rows = NULL,
      dataset_filename = "my_data.csv"
    )
  )

  it("generates a complete individual plot script", {
    inputs <- list(
      color_by = "USUBJID", facet_by = NULL, show_facet_n = FALSE,
      ylog_scale = FALSE, show_legend = TRUE, show_dose = FALSE,
      threshold_value = NULL, x_limits = NULL, y_limits = NULL,
      filtering_list = NULL, use_time_since_last_dose = FALSE,
      palette = "default", y_axis_values = "lines"
    )
    code <- build_plot_code("individual", inputs, mock_session)
    expect_true(grepl("library\\(aNCA\\)", code))
    expect_true(grepl("read_pk", code))
    expect_true(grepl("PKNCA_create_data_object", code))
    expect_true(grepl("exploration_individualplot", code))
    expect_true(grepl("my_data.csv", code))
    expect_true(grepl("ggsave", code))
  })

  it("generates a complete mean plot script", {
    inputs <- list(
      color_by = "TRT01A", facet_by = NULL, show_facet_n = FALSE,
      ylog_scale = FALSE, show_legend = TRUE, show_dose = FALSE,
      threshold_value = NULL, x_limits = NULL, y_limits = NULL,
      sd_min = FALSE, sd_max = FALSE, ci = FALSE,
      filtering_list = NULL, use_time_since_last_dose = FALSE,
      palette = "default", y_axis_values = "lines"
    )
    code <- build_plot_code("mean", inputs, mock_session)
    expect_true(grepl("exploration_meanplot", code))
  })

  it("generates a complete QC plot script", {
    inputs <- list(
      colour_var = "DOSEA", group_var = "TRT01A",
      usubjid = NULL, pcspec = NULL,
      show_samples_doses = c("PK Samples", "Doses")
    )
    code <- build_plot_code("qc", inputs, mock_session)
    expect_true(grepl("pk_dose_qc_plot", code))
    expect_true(grepl("tooltip = \"text\"", code))
  })

  it("uses fallback filename when dataset_filename is NULL", {
    session_no_file <- mock_session
    session_no_file$userData$dataset_filename <- NULL
    inputs <- list(
      color_by = "USUBJID", facet_by = NULL, show_facet_n = FALSE,
      ylog_scale = FALSE, show_legend = TRUE, show_dose = FALSE,
      threshold_value = NULL, x_limits = NULL, y_limits = NULL,
      filtering_list = NULL, use_time_since_last_dose = FALSE,
      palette = "default", y_axis_values = "lines"
    )
    code <- build_plot_code("individual", inputs, session_no_file)
    expect_true(grepl("input_data.csv", code))
  })

  it("includes filter code when filtering_list is provided", {
    inputs <- list(
      color_by = "USUBJID", facet_by = NULL, show_facet_n = FALSE,
      ylog_scale = FALSE, show_legend = TRUE, show_dose = FALSE,
      threshold_value = NULL, x_limits = NULL, y_limits = NULL,
      filtering_list = list(DOSEA = c("10", "20")),
      use_time_since_last_dose = FALSE,
      palette = "default", y_axis_values = "lines"
    )
    code <- build_plot_code("individual", inputs, mock_session)
    expect_true(grepl("filtering_list <- list", code))
    expect_true(grepl("DOSEA", code))
  })

  it("errors on unknown plot type", {
    expect_error(
      build_plot_code("unknown", list(), mock_session),
      "Unknown plot_type"
    )
  })
})
