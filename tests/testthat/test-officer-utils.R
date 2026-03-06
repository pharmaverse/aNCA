describe("create_pptx_dose_slides", {
  template <- system.file("www/templates/template.pptx", package = "aNCA")

  base_slides <- list(
    list(
      info = data.frame(group = "A"),
      group = "A",
      statistics = data.frame(stat = "Mean", value = 1),
      meanplot = ggplot2::ggplot(),
      linplot = ggplot2::ggplot(),
      boxplot = ggplot2::ggplot(),
      ind_params = list(SUBJ01 = data.frame(param = "CMAX", value = 1)),
      ind_plots = list(SUBJ01 = ggplot2::ggplot())
    )
  )

  it("produces fewer slides when ind_plots is excluded from slide_sections", {
    slides_all <- base_slides
    slides_no_ind <- base_slides
    attr(slides_no_ind, "slide_sections") <- c("meanplot", "statistics", "ind_params")

    out_all <- tempfile(fileext = ".pptx")
    out_no_ind <- tempfile(fileext = ".pptx")

    create_pptx_dose_slides(slides_all, out_all, "NCA", template)
    create_pptx_dose_slides(slides_no_ind, out_no_ind, "NCA", template)

    n_all <- length(officer::read_pptx(out_all))
    n_no_ind <- length(officer::read_pptx(out_no_ind))

    expect_lt(n_no_ind, n_all)
  })

  it("includes all slides when slide_sections is NULL (backwards compat)", {
    out <- tempfile(fileext = ".pptx")
    create_pptx_dose_slides(base_slides, out, "NCA", template)
    pptx <- officer::read_pptx(out)
    expect_gte(length(pptx), 8)
  })

  it("adds additional analysis slides generically for any non-empty data frame", {
    slides <- base_slides
    attr(slides, "additional_analysis") <- list(
      matrix_ratios = data.frame(Ratio_Type = "A/B", Ratio = 1.2)
    )
    out_base <- tempfile(fileext = ".pptx")
    out_extra <- tempfile(fileext = ".pptx")

    create_pptx_dose_slides(base_slides, out_base, "NCA", template)
    create_pptx_dose_slides(slides, out_extra, "NCA", template)

    expect_gt(length(officer::read_pptx(out_extra)), length(officer::read_pptx(out_base)))
  })

  it("omits additional analysis entry when its name is not in slide_sections", {
    slides_one <- base_slides
    attr(slides_one, "additional_analysis") <- list(
      matrix_ratios = data.frame(Ratio_Type = "A/B", Ratio = 1.2),
      excretion_summary = data.frame(USUBJID = "01", FE = 45)
    )
    attr(slides_one, "slide_sections") <- c(
      "ind_plots", "meanplot", "statistics", "ind_params", "matrix_ratios"
      # excretion_summary deliberately omitted
    )

    slides_both <- base_slides
    attr(slides_both, "additional_analysis") <- list(
      matrix_ratios = data.frame(Ratio_Type = "A/B", Ratio = 1.2),
      excretion_summary = data.frame(USUBJID = "01", FE = 45)
    )
    attr(slides_both, "slide_sections") <- c(
      "ind_plots", "meanplot", "statistics", "ind_params", "matrix_ratios", "excretion_summary"
    )

    out_one <- tempfile(fileext = ".pptx")
    out_both <- tempfile(fileext = ".pptx")

    create_pptx_dose_slides(slides_one, out_one, "NCA", template)
    create_pptx_dose_slides(slides_both, out_both, "NCA", template)

    expect_lt(length(officer::read_pptx(out_one)), length(officer::read_pptx(out_both)))
  })
})
