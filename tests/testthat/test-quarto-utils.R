describe("create_qmd_dose_slides", {
  base_slides <- list(
    list(
      info = data.frame(group = "A"),
      statistics = data.frame(stat = "Mean", value = 1),
      meanplot = "meanplot_obj",
      linplot = "linplot_obj",
      boxplot = "boxplot_obj",
      ind_params = list(SUBJ01 = data.frame(param = "CMAX", value = 1)),
      ind_plots = list(SUBJ01 = "indplot_obj")
    )
  )

  it("adds additional analysis section when matrix ratios or excretion summary are present", {
    slides <- base_slides
    attr(slides, "additional_analysis") <- list(
      matrix_ratios = data.frame(Ratio_Type = "A/B", Ratio = 1.2),
      excretion_summary = data.frame(USUBJID = "01", FE = 45)
    )
    out_file <- tempfile(fileext = ".qmd")

    create_qmd_dose_slides(slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("# Additional Analysis Figures", content, fixed = TRUE))
    expect_true(grepl("## Matrix Ratios", content, fixed = TRUE))
    expect_true(grepl("## Excretion Summary", content, fixed = TRUE))
    expect_true(grepl("additional_analysis[['matrix_ratios']]", content, fixed = TRUE))
    expect_true(grepl("additional_analysis[['excretion_summary']]", content, fixed = TRUE))
  })

  it("does not add additional analysis section when no additional tables are available", {
    out_file <- tempfile(fileext = ".qmd")
    create_qmd_dose_slides(base_slides, out_file, "NCA Results", use_plotly = FALSE)
    content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

    expect_false(grepl("# Additional Analysis Figures", content, fixed = TRUE))
  })
})
