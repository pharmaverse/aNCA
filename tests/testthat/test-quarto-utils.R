describe("create_qmd_dose_slides", {
  base_slides <- list(
    list(
      info = data.frame(group = "A"),
      statistics = data.frame(stat = "Mean", value = 1),
      meanplot = ggplot2::ggplot(),
      linplot = ggplot2::ggplot(),
      boxplot = list(AUCIFO = ggplot2::ggplot()),
      ind_params = list(SUBJ01 = data.frame(param = "CMAX", value = 1)),
      ind_plots = list(SUBJ01 = ggplot2::ggplot())
    )
  )

  it("omits individual plots when ind_plots is not in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("meanplot", "statistics", "ind_params")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_false(grepl("ind_plots", content, fixed = TRUE))
  })

  it("includes individual plots when ind_plots is in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("ind_plots", "meanplot", "statistics", "ind_params")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("ind_plots", content, fixed = TRUE))
  })

  it("includes all sections when slide_sections attribute is NULL (backwards compat)", {
    out_file <- tempfile(fileext = ".qmd")
    create_qmd_dose_slides(base_slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("ind_plots", content, fixed = TRUE))
    expect_true(grepl("meanplot", content, fixed = TRUE))
  })

  it("adds additional analysis section generically for any non-empty data frame in the attribute", {
    slides <- base_slides
    attr(slides, "additional_analysis") <- list(
      matrix_ratios = data.frame(Ratio_Type = "A/B", Ratio = 1.2),
      excretion_summary = data.frame(USUBJID = "01", FE = 45)
    )
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("# Additional Analysis Figures", content, fixed = TRUE))
    expect_true(grepl("Matrix Ratios", content, fixed = TRUE))
    expect_true(grepl("Excretion Summary", content, fixed = TRUE))
  })

  it("skips boxplot expression when boxplot data is NULL", {
    slides_null_box <- list(
      list(
        info = data.frame(group = "A"),
        statistics = data.frame(stat = "Mean", value = 1),
        meanplot = ggplot2::ggplot(),
        linplot = ggplot2::ggplot(),
        boxplot = list(),
        ind_params = list(SUBJ01 = data.frame(param = "CMAX", value = 1)),
        ind_plots = list(SUBJ01 = ggplot2::ggplot())
      )
    )
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides_null_box, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_false(grepl("boxplot", content, fixed = TRUE))
  })

  it("omits linplot expression when linplot is not in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("meanplot", "statistics", "ind_plots", "ind_params")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_false(grepl("linplot", content, fixed = TRUE))
  })

  it("includes linplot expression when linplot is in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("meanplot", "statistics", "ind_plots", "ind_params", "linplot")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("linplot", content, fixed = TRUE))
  })

  it("omits boxplot expression when boxplot is not in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("meanplot", "statistics", "ind_plots", "ind_params", "linplot")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_false(grepl("boxplot", content, fixed = TRUE))
  })

  it("includes boxplot expression when boxplot is in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c(
      "meanplot", "statistics", "ind_plots", "ind_params", "linplot", "boxplot"
    )
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("boxplot", content, fixed = TRUE))
  })

  it("includes boxplot when only boxplot is in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("boxplot")
    out_file <- tempfile(fileext = ".qmd")
    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
    expect_true(grepl("boxplot", content, fixed = TRUE))
    expect_false(grepl("meanplot", content, fixed = TRUE))
  })

  it("omits additional analysis entry when its name is not in slide_sections", {
    slides <- base_slides
    attr(slides, "additional_analysis") <- list(
      matrix_ratios = data.frame(Ratio_Type = "A/B", Ratio = 1.2),
      excretion_summary = data.frame(USUBJID = "01", FE = 45)
    )
    attr(slides, "slide_sections") <- c(
      "ind_plots", "meanplot", "statistics", "ind_params", "matrix_ratios"
    )
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("Matrix Ratios", content, fixed = TRUE))
    expect_false(grepl("Excretion Summary", content, fixed = TRUE))
  })

  it("omits additional analysis section entirely when no entries are selected", {
    slides <- base_slides
    attr(slides, "additional_analysis") <- list(
      matrix_ratios = data.frame(Ratio_Type = "A/B", Ratio = 1.2)
    )
    attr(slides, "slide_sections") <- c("ind_plots", "meanplot", "statistics", "ind_params")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_false(grepl("# Additional Analysis Figures", content, fixed = TRUE))
  })

  it("silently omits empty data frame entries in additional_analysis", {
    slides <- base_slides
    attr(slides, "additional_analysis") <- list(
      matrix_ratios = data.frame(Ratio_Type = "A/B", Ratio = 1.2),
      empty_table = data.frame()
    )
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("Matrix Ratios", content, fixed = TRUE))
    expect_false(grepl("Empty Table", content, fixed = TRUE))
  })

  it("emits a NULL plot expression when ind_params is selected but ind_plots is not", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("meanplot", "statistics", "ind_params")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    # ind_params table slide is still written
    expect_true(grepl("ind_params", content, fixed = TRUE))
    # ind_plots expression is absent
    expect_false(grepl("ind_plots", content, fixed = TRUE))
  })
})
