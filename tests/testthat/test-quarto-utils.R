describe("create_qmd_dose_slides", {
  base_slides <- list(
    list(
      info = data.frame(group = "A"),
      statistics = data.frame(stat = "Mean", value = 1),
      meanplot = ggplot2::ggplot(),
      linplot = ggplot2::ggplot(),
      boxplot = list(AUCIFO = ggplot2::ggplot()),
      ind_params = list(SUBJ01 = data.frame(param = "CMAX", value = 1)),
      ind_plots = list(SUBJ01 = ggplot2::ggplot()),
      dose_norm_meanplot   = ggplot2::ggplot(),
      dose_norm_statistics = data.frame(
        stat = "Mean",
        `CMAXD[ng/mL/mg]` = 0.5,
        check.names = FALSE
      ),
      ind_dose_norm_params = list(
        SUBJ01 = data.frame(`CMAXD[ng/mL/mg]` = 0.5, check.names = FALSE)
      )
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
    attr(slides, "slide_sections") <- c(
      "meanplot", "statistics", "ind_plots", "ind_params", "linplot"
    )
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("linplot", content, fixed = TRUE))
  })

  it("omits boxplot expression when boxplot is not in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c(
      "meanplot", "statistics", "ind_plots", "ind_params", "linplot"
    )
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

  it("includes # Group 1 heading when summary sections are selected", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("meanplot", "statistics")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("# Group 1", content, fixed = TRUE))
  })

  it("includes # Group 1 (Individual) heading when individual sections are selected", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("ind_plots", "ind_params")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("# Group 1 (Individual)", content, fixed = TRUE))
  })

  it("includes # Extra Figures when both summary and individual sections are selected", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("meanplot", "ind_plots")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("# Extra Figures", content, fixed = TRUE))
  })

  it("omits # Extra Figures when only individual sections are selected", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("ind_plots", "ind_params")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_false(grepl("# Extra Figures", content, fixed = TRUE))
  })

  it("omits # Extra Figures when only summary sections are selected", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("meanplot", "statistics")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_false(grepl("# Extra Figures", content, fixed = TRUE))
  })

  it("includes toc: true in YAML header", {
    out_file <- tempfile(fileext = ".qmd")
    create_qmd_dose_slides(base_slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("toc: true", content, fixed = TRUE))
  })

  it("includes info table on section header, not repeated per subject", {
    slides <- list(
      list(
        info = data.frame(group = "A"),
        statistics = data.frame(stat = "Mean", value = 1),
        meanplot = ggplot2::ggplot(),
        linplot = ggplot2::ggplot(),
        boxplot = list(AUCIFO = ggplot2::ggplot()),
        ind_params = list(
          SUBJ01 = data.frame(param = "CMAX", value = 1),
          SUBJ02 = data.frame(param = "CMAX", value = 2)
        ),
        ind_plots = list(
          SUBJ01 = ggplot2::ggplot(),
          SUBJ02 = ggplot2::ggplot()
        )
      )
    )
    attr(slides, "slide_sections") <- c("meanplot", "ind_plots", "ind_params")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    # Info table referenced once per section header (summary + individual = 2),
    # NOT once per subject (which would be 3: 1 summary + 2 subjects)
    n_info <- lengths(regmatches(content, gregexpr("res_dose_slides\\[\\[1\\]\\]\\$info", content)))
    expect_equal(n_info, 2L)
  })

  it("does not create an orphan header when group ind_params and ind_plots are both empty", {
    slides <- list(
      list(
        info = data.frame(group = "A"),
        statistics = data.frame(stat = "Mean", value = 1),
        meanplot = ggplot2::ggplot(),
        linplot = ggplot2::ggplot(),
        boxplot = list(),
        ind_params = list(SUBJ01 = data.frame(param = "CMAX", value = 1)),
        ind_plots  = list(SUBJ01 = ggplot2::ggplot())
      ),
      list(
        info = data.frame(group = "B"),
        statistics = data.frame(stat = "Mean", value = 2),
        meanplot = ggplot2::ggplot(),
        linplot = ggplot2::ggplot(),
        boxplot = list(),
        ind_params = list(),  # empty — no subjects for group 2
        ind_plots  = list()
      )
    )
    attr(slides, "slide_sections") <- c("ind_plots", "ind_params", "meanplot")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("Group 1 (Individual)", content, fixed = TRUE))
    expect_false(grepl("Group 2 (Individual)", content, fixed = TRUE))
  })

  it("always has a blank line after slide separator even when plot is NULL", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("statistics", "ind_params")
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    lines <- readLines(out_file, warn = FALSE)

    separator_indices <- which(lines == "---")
    # Skip the YAML header delimiters (first two occurrences)
    slide_separators <- separator_indices[separator_indices > 2]

    expect_true(length(slide_separators) > 0)
    for (idx in slide_separators) {
      expect_equal(
        lines[idx + 1], "",
        info = paste0("'---' at line ", idx, " must be followed by a blank line")
      )
    }
  })

  it("sanitizes newlines in title to prevent YAML parse errors", {
    out_file <- tempfile(fileext = ".qmd")
    create_qmd_dose_slides(base_slides, out_file, "Line1\nLine2\r\nLine3", use_plotly = FALSE)
    lines <- readLines(out_file, warn = FALSE)

    title_line <- lines[grep("^title:", lines)[1]]
    expect_false(grepl("\n", title_line, fixed = TRUE))
    expect_false(grepl("\r", title_line, fixed = TRUE))
    expect_true(grepl("Line1 Line2 Line3", title_line, fixed = TRUE))
  })

  it("includes dose_norm_plot when dose_norm_plot is in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("dose_norm_plot")
    out_file <- tempfile(fileext = ".qmd")
    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
    expect_true(grepl("dose_norm_meanplot", content, fixed = TRUE))
  })

  it("omits dose_norm_plot when dose_norm_plot is not in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("meanplot", "statistics")
    out_file <- tempfile(fileext = ".qmd")
    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
    expect_false(grepl("dose_norm_meanplot", content, fixed = TRUE))
  })

  it("includes dose_norm_ind_params when dose_norm_ind_params is in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("dose_norm_ind_params")
    out_file <- tempfile(fileext = ".qmd")
    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
    expect_true(grepl("ind_dose_norm_params", content, fixed = TRUE))
  })

  it("omits dose_norm_ind_params when not in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("ind_params", "ind_plots")
    out_file <- tempfile(fileext = ".qmd")
    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
    expect_false(grepl("ind_dose_norm_params", content, fixed = TRUE))
  })
})
