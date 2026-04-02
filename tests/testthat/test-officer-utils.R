describe("create_pptx_dose_slides", {
  template <- system.file("www/templates/template.pptx", package = "aNCA")

  base_slides <- list(
    list(
      info = data.frame(group = "A"),
      group = "A",
      statistics = data.frame(stat = "Mean", value = 1),
      meanplot = ggplot2::ggplot(),
      analyte_comparison = ggplot2::ggplot(),
      linplot = ggplot2::ggplot(),
      boxplot = list(AUCIFO = ggplot2::ggplot()),
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

  it("skips boxplot slide when boxplot data is NULL", {
    slides_null_box <- list(
      list(
        info = data.frame(group = "A"),
        group = "A",
        statistics = data.frame(stat = "Mean", value = 1),
        meanplot = ggplot2::ggplot(),
        linplot = ggplot2::ggplot(),
        boxplot = list(AUCIFO = NULL),
        ind_params = list(SUBJ01 = data.frame(param = "CMAX", value = 1)),
        ind_plots = list(SUBJ01 = ggplot2::ggplot())
      )
    )
    out_null <- tempfile(fileext = ".pptx")
    out_full <- tempfile(fileext = ".pptx")

    create_pptx_dose_slides(slides_null_box, out_null, "NCA", template)
    create_pptx_dose_slides(base_slides, out_full, "NCA", template)

    expect_lt(length(officer::read_pptx(out_null)), length(officer::read_pptx(out_full)))
  })

  it("omits linplot slide when linplot is not in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("meanplot", "statistics", "ind_plots", "ind_params")
    # linplot deliberately omitted

    out_with <- tempfile(fileext = ".pptx")
    out_without <- tempfile(fileext = ".pptx")

    create_pptx_dose_slides(base_slides, out_with, "NCA", template)
    create_pptx_dose_slides(slides, out_without, "NCA", template)

    expect_lt(length(officer::read_pptx(out_without)), length(officer::read_pptx(out_with)))
  })

  it("omits boxplot slide when boxplot is not in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c(
      "meanplot", "statistics", "ind_plots", "ind_params", "linplot"
    )
    # boxplot deliberately omitted

    out_with <- tempfile(fileext = ".pptx")
    out_without <- tempfile(fileext = ".pptx")

    create_pptx_dose_slides(base_slides, out_with, "NCA", template)
    create_pptx_dose_slides(slides, out_without, "NCA", template)

    expect_lt(length(officer::read_pptx(out_without)), length(officer::read_pptx(out_with)))
  })

  it("includes boxplot slide when only boxplot is in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("boxplot")
    out_base <- tempfile(fileext = ".pptx")
    out_box  <- tempfile(fileext = ".pptx")
    create_pptx_dose_slides(base_slides, out_base, "NCA", template)
    create_pptx_dose_slides(slides,      out_box,  "NCA", template)
    expect_gt(length(officer::read_pptx(out_box)), 1)   # at least one content slide
    expect_lt(length(officer::read_pptx(out_box)),
              length(officer::read_pptx(out_base)))      # fewer than all sections
  })

  it("does not create an orphan header slide when group ind_params and ind_plots are both empty", {
    # When the second group has empty ind_params (the bug scenario), the current code still
    # adds the group header/covariate table slide via add_pptx_sl_table() before the
    # purrr::reduce loop, leaving an orphan header with no per-subject slides following it.
    # A correct implementation produces 2 fewer slides for the empty group
    # (1 fewer header + 1 fewer subject) vs a group with one subject.
    slides_g2_empty <- list(
      list(
        info = data.frame(group = "A"), group = "Parent",
        statistics = data.frame(stat = "Mean", value = 1),
        meanplot = ggplot2::ggplot(), linplot = ggplot2::ggplot(),
        boxplot = list(),
        ind_params = list(SUBJ01 = data.frame(param = "CMAX", value = 1)),
        ind_plots  = list(SUBJ01 = ggplot2::ggplot())
      ),
      list(
        info = data.frame(group = "B"), group = "Metabolite",
        statistics = data.frame(stat = "Mean", value = 2),
        meanplot = ggplot2::ggplot(), linplot = ggplot2::ggplot(),
        boxplot = list(),
        ind_params = list(),  # empty — no subjects for group 2
        ind_plots  = list()
      )
    )
    slides_g2_with_subj <- slides_g2_empty
    slides_g2_with_subj[[2]]$ind_params <- list(SUBJ01 = data.frame(param = "CMAX", value = 2))
    slides_g2_with_subj[[2]]$ind_plots  <- list(SUBJ01 = ggplot2::ggplot())

    attr(slides_g2_empty,     "slide_sections") <- c("ind_plots", "ind_params", "meanplot")
    attr(slides_g2_with_subj, "slide_sections") <- c("ind_plots", "ind_params", "meanplot")

    out_empty <- tempfile(fileext = ".pptx")
    out_subj  <- tempfile(fileext = ".pptx")
    create_pptx_dose_slides(slides_g2_empty,     out_empty, "NCA", template)
    create_pptx_dose_slides(slides_g2_with_subj, out_subj,  "NCA", template)

    n_empty <- length(officer::read_pptx(out_empty))
    n_subj  <- length(officer::read_pptx(out_subj))
    # Group 2 with one subject = 1 header slide + 1 per-subject slide = 2 more slides
    # Currently FAILS (difference is 1) because the orphan header is still created
    expect_equal(n_subj - n_empty, 2)
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

  it("includes analyte comparison slide when analyte_comparison is in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c(
      "meanplot", "analyte_comparison", "statistics", "ind_plots", "ind_params"
    )
    out_with <- tempfile(fileext = ".pptx")
    create_pptx_dose_slides(slides, out_with, "NCA", template)

    slides_without <- base_slides
    attr(slides_without, "slide_sections") <- c(
      "meanplot", "statistics", "ind_plots", "ind_params"
    )
    out_without <- tempfile(fileext = ".pptx")
    create_pptx_dose_slides(slides_without, out_without, "NCA", template)

    expect_gt(length(officer::read_pptx(out_with)), length(officer::read_pptx(out_without)))
  })

  it("omits analyte comparison slide when analyte_comparison is not in slide_sections", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("meanplot", "statistics", "ind_plots", "ind_params")
    out <- tempfile(fileext = ".pptx")
    create_pptx_dose_slides(slides, out, "NCA", template)

    slides_with <- base_slides
    attr(slides_with, "slide_sections") <- c(
      "meanplot", "analyte_comparison", "statistics", "ind_plots", "ind_params"
    )
    out_with <- tempfile(fileext = ".pptx")
    create_pptx_dose_slides(slides_with, out_with, "NCA", template)

    expect_lt(length(officer::read_pptx(out)), length(officer::read_pptx(out_with)))
  })

  it("includes analyte comparison slide when slide_sections is NULL (all selected)", {
    out <- tempfile(fileext = ".pptx")
    create_pptx_dose_slides(base_slides, out, "NCA", template)

    slides_no_ac <- base_slides
    slides_no_ac[[1]]$analyte_comparison <- NULL
    out_no_ac <- tempfile(fileext = ".pptx")
    create_pptx_dose_slides(slides_no_ac, out_no_ac, "NCA", template)

    expect_gt(length(officer::read_pptx(out)), length(officer::read_pptx(out_no_ac)))
  })

  it("creates summary section when only analyte_comparison is selected", {
    slides <- base_slides
    attr(slides, "slide_sections") <- c("analyte_comparison")
    out <- tempfile(fileext = ".pptx")
    create_pptx_dose_slides(slides, out, "NCA", template)
    # Title slide + group info + analyte comparison + Extra Figures title = at least 4
    expect_gte(length(officer::read_pptx(out)), 4)
  })
})
