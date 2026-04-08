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
    expect_equal(result, c("individualplot", "meanplot", "qcplot"))
  })

  it("replaces default with custom names for a type", {
    custom <- c(my_plot = "individual", spaghetti = "individual")
    result <- .build_exploration_allowlist(
      selected_types = c("individual", "mean", "qc"),
      custom_names = custom
    )
    expect_equal(result, c("my_plot", "spaghetti", "meanplot", "qcplot"))
  })

  it("excludes custom names for deselected types", {
    custom <- c(indiv1 = "individual", mean1 = "mean")
    result <- .build_exploration_allowlist(
      selected_types = c("mean", "qc"),
      custom_names = custom
    )
    expect_equal(result, c("mean1", "qcplot"))
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
