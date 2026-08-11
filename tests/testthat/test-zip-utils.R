# Source the Shiny helper to test pure utility functions
source(
  file.path(system.file("shiny", package = "aNCA"), "functions", "zip-utils.R"),
  local = TRUE
)

describe(".build_exploration_allowlist", {
  it("returns defaults when no custom names exist", {
    result <- .build_exploration_allowlist(
      selected_types = c("individual", "mean", "qc"),
      custom_names = character(0)
    )
    base <- c("individualplot", "meanplot", "qcplot")
    expect_equal(result, c(base, paste0(base, "_code")))
  })

  it("replaces default with custom names for a type", {
    custom <- c(my_plot = "individual", spaghetti = "individual")
    result <- .build_exploration_allowlist(
      selected_types = c("individual", "mean", "qc"),
      custom_names = custom
    )
    base <- c("my_plot", "spaghetti", "meanplot", "qcplot")
    expect_equal(result, c(base, paste0(base, "_code")))
  })

  it("excludes custom names for deselected types", {
    custom <- c(indiv1 = "individual", mean1 = "mean")
    result <- .build_exploration_allowlist(
      selected_types = c("mean", "qc"),
      custom_names = custom
    )
    base <- c("mean1", "qcplot")
    expect_equal(result, c(base, paste0(base, "_code")))
    expect_false("indiv1" %in% result)
    expect_false("individualplot" %in% result)
  })

  it("returns empty vector when no types are selected", {
    custom <- c(my_plot = "individual")
    result <- .build_exploration_allowlist(
      selected_types = character(0),
      custom_names = custom
    )
    expect_equal(result, character(0))
  })
})

describe(".export_slides slide_sections threading", {
  it("attaches slide_sections attribute when provided", {
    skip(paste0(
      "Requires full NCA session setup; ",
      "covered by test-quarto-utils.R and test-officer-utils.R unit tests"
    ))
  })
})

describe("get_dose_esc_results", {
  it("stores NULL for boxplot when boxplot_parameters is not in the NCA results", {
    res <- get_dose_esc_results(
      o_nca = FIXTURE_PKNCA_RES,
      group_by_vars = "DOSNOA",
      facet_vars = "ATPTREF",
      boxplot_parameters = "NONEXISTENT_PARAM"
    )

    boxplots <- lapply(res, `[[`, "boxplot")
    expect_true(all(vapply(
      boxplots,
      function(x) is.list(x) && all(vapply(x, is.null, logical(1))),
      logical(1)
    )))
  })

  it("stores a ggplot for boxplot when boxplot_parameters is in the NCA results", {
    res <- get_dose_esc_results(
      o_nca = FIXTURE_PKNCA_RES,
      group_by_vars = "DOSNOA",
      facet_vars = "ATPTREF",
      boxplot_parameters = "CMAX"
    )

    boxplots <- lapply(res, `[[`, "boxplot")
    expect_true(all(vapply(
      boxplots,
      function(x) is.list(x) && all(vapply(x, function(p) inherits(p, "ggplot"), logical(1))),
      logical(1)
    )))
  })

  it("produces no boxplot entries when boxplot_parameters is empty (character(0))", {
    res <- get_dose_esc_results(
      o_nca         = FIXTURE_PKNCA_RES,
      group_by_vars = "DOSNOA",
      facet_vars    = "ATPTREF",
      boxplot_parameters = character(0)
    )
    boxplots <- lapply(res, `[[`, "boxplot")
    expect_true(all(vapply(
      boxplots,
      function(x) is.list(x) && length(x) == 0,
      logical(1)
    )))
  })

  it("produces a boxplot for the explicitly requested parameter", {
    res <- get_dose_esc_results(
      o_nca = FIXTURE_PKNCA_RES,
      group_by_vars = "DOSNOA",
      facet_vars = "ATPTREF",
      boxplot_parameters = "LAMZHL"
    )
    # Every group's boxplot list should contain a ggplot for LAMZHL
    boxplots <- lapply(res, `[[`, "boxplot")
    expect_true(all(vapply(
      boxplots,
      function(x) is.list(x) && inherits(x[["LAMZHL"]], "ggplot"),
      logical(1)
    )))
    # The y-axis label references LAMZHL
    expect_true(any(vapply(
      boxplots,
      function(x) grepl("LAMZHL", x[["LAMZHL"]]$labels$y),
      logical(1)
    )))
  })

  it("ind_params is non-empty for every group when group_by_vars includes the analyte column", {
    # This mirrors the real app call in prepare_export_files() using the analyte
    # grouping column. Previously, groups were derived from intervals (which lacks
    # the analyte column), causing empty ind_params for metabolite groups.
    group_by_vars <- setdiff(
      dplyr::group_vars(FIXTURE_PKNCA_RES),
      FIXTURE_PKNCA_RES$data$conc$columns$subject
    )
    res <- get_dose_esc_results(
      o_nca              = FIXTURE_PKNCA_RES,
      group_by_vars      = group_by_vars,
      facet_vars         = "ATPTREF",
      boxplot_parameters = character(0)
    )
    ind_params_lengths <- vapply(res, function(g) length(g$ind_params), integer(1))
    expect_true(
      all(ind_params_lengths > 0),
      label = paste0(
        "Groups with empty ind_params: ",
        paste(names(ind_params_lengths)[ind_params_lengths == 0], collapse = ", ")
      )
    )
  })

  it("creates slides only for groups present in o_nca$result, not all conc data groups", {
    # Simulates a specimen (URINE) present in conc data but not in NCA intervals.
    # When groups are derived from conc$data, a spurious URINE group is created.
    # When derived from result, only the 4 analyzed groups appear.
    fixture_extra <- FIXTURE_PKNCA_RES
    urine_rows <- FIXTURE_PKNCA_RES$data$conc$data[1:5, ]
    urine_rows$PCSPEC <- "URINE"
    fixture_extra$data$conc$data <- rbind(FIXTURE_PKNCA_RES$data$conc$data, urine_rows)

    group_by_vars <- setdiff(
      dplyr::group_vars(FIXTURE_PKNCA_RES),
      FIXTURE_PKNCA_RES$data$conc$columns$subject
    )
    n_result_groups <- nrow(unique(FIXTURE_PKNCA_RES$result[, c(group_by_vars, "ATPTREF")]))

    res <- get_dose_esc_results(
      o_nca              = fixture_extra,
      group_by_vars      = group_by_vars,
      facet_vars         = "ATPTREF",
      boxplot_parameters = character(0)
    )
    expect_equal(length(res), n_result_groups)
  })
})

# Format handling added for the bulk TLG export (issue #1344): PDF for ggplots, XLSX for
# tables, and raster output for plotly objects that carry their source ggplot.

describe("save_ggplot_format", {
  gg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()

  it("writes a PDF when asked", {
    d <- withr::local_tempdir()
    save_ggplot_format(gg, file.path(d, "p"), "pdf")
    expect_true(file.exists(file.path(d, "p.pdf")))
  })
})

describe("save_table_format", {
  it("writes an XLSX when asked", {
    d <- withr::local_tempdir()
    save_table_format(head(mtcars), file.path(d, "t"), "xlsx")
    expect_true(file.exists(file.path(d, "t.xlsx")))
  })

  it("writes an XLSX from an rlistings listing_df, which is not a plain data.frame", {
    skip_if_not_installed("rlistings")
    d <- withr::local_tempdir()
    lst <- rlistings::as_listing(data.frame(a = 1:3, b = letters[1:3]), key_cols = "a")
    expect_no_error(save_table_format(lst, file.path(d, "l"), "xlsx"))
    expect_true(file.exists(file.path(d, "l.xlsx")))
  })
})

describe("save_plotly_format", {
  gg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()

  it("renders PNG from the stashed source ggplot", {
    d <- withr::local_tempdir()
    save_plotly_format(aNCA:::.with_ggplot(plotly::ggplotly(gg), gg), file.path(d, "p"), "png")
    expect_true(file.exists(file.path(d, "p.png")))
    # Only what was asked for -- HTML drags in a multi-MB dependency folder per plot.
    expect_false(file.exists(file.path(d, "p.html")))
  })

  it("falls back to HTML when there is no stashed ggplot to rasterise", {
    d <- withr::local_tempdir()
    save_plotly_format(plotly::ggplotly(gg), file.path(d, "p"), "png")
    expect_false(file.exists(file.path(d, "p.png")))
    # Never silently produce nothing: HTML is the only thing a bare plotly can yield.
    expect_true(file.exists(file.path(d, "p.html")))
  })
})

# .make_zip_filename gained a `suffix` argument so the TLG bulk download can reuse the
# project/study naming instead of duplicating it (issue #1344).
local({
  library(shiny)
  source(
    file.path(system.file("shiny", package = "aNCA"), "modules", "tab_nca", "zip.R"),
    local = TRUE
  )
},
envir = parent.env(environment()))

describe(".make_zip_filename", {
  fake_session <- function(project = "", label = "") {
    list(userData = list(
      project_name    = function() project,
      study_ids_label = function() label
    ))
  }

  it("defaults to the project name with a .zip extension", {
    expect_equal(.make_zip_filename(fake_session("MyProject")), "MyProject.zip")
  })

  it("honours a custom suffix so exports are distinguishable", {
    expect_equal(
      .make_zip_filename(fake_session("MyProject"), "_TLGs.zip"), "MyProject_TLGs.zip"
    )
  })

  it("falls back to the study label, then to a bare NCA name", {
    expect_equal(.make_zip_filename(fake_session("", "S123"), "_TLGs.zip"),
                 "NCA_S123_TLGs.zip")
    expect_equal(.make_zip_filename(fake_session("", ""), "_TLGs.zip"), "NCA_TLGs.zip")
  })

  it("replaces characters that are not safe in a file name", {
    expect_equal(.make_zip_filename(fake_session("My Project/v2")), "My_Project_v2.zip")
  })
})
