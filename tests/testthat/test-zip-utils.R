# Source the Shiny helper to test pure utility functions
source(
  file.path(system.file("shiny", package = "aNCA"), "functions", "zip-utils.R"),
  local = TRUE
)

testthat::test_that(".is_exportable returns TRUE when obj_names is NULL", {
  testthat::expect_true(.is_exportable("anything", NULL))
})

testthat::test_that(".is_exportable matches exact names", {
  testthat::expect_true(.is_exportable("foo", c("foo", "bar")))
  testthat::expect_true(.is_exportable("bar", c("foo", "bar")))
  testthat::expect_false(.is_exportable("baz", c("foo", "bar")))
})

testthat::test_that(".is_exportable matches numbered variants", {
  testthat::expect_true(.is_exportable("foo1", c("foo")))
  testthat::expect_true(.is_exportable("foo99", c("foo")))
  testthat::expect_true(.is_exportable("individualplot3", c("individualplot")))
})

testthat::test_that(".is_exportable rejects partial and suffix matches", {
  testthat::expect_false(.is_exportable("foobar", c("foo")))
  testthat::expect_false(.is_exportable("barfoo1", c("foo")))
  testthat::expect_false(.is_exportable("foo1bar", c("foo")))
})

testthat::test_that(".build_exploration_allowlist returns defaults when no customs", {
  result <- .build_exploration_allowlist(
    selected_types = c("individual", "mean", "qc"),
    custom_names = character(0)
  )
  testthat::expect_equal(result, c("individualplot", "meanplot", "qcplot"))
})

testthat::test_that(".build_exploration_allowlist replaces default with customs", {
  custom <- c(my_plot = "individual", spaghetti = "individual")
  result <- .build_exploration_allowlist(
    selected_types = c("individual", "mean", "qc"),
    custom_names = custom
  )
  testthat::expect_equal(result, c("my_plot", "spaghetti", "meanplot", "qcplot"))
})

testthat::test_that(".build_exploration_allowlist excludes deselected types", {
  custom <- c(indiv1 = "individual", mean1 = "mean")
  result <- .build_exploration_allowlist(
    selected_types = c("mean", "qc"),
    custom_names = custom
  )
  testthat::expect_equal(result, c("mean1", "qcplot"))
  testthat::expect_false("indiv1" %in% result)
  testthat::expect_false("individualplot" %in% result)
})

testthat::test_that(".build_exploration_allowlist handles empty selection", {
  custom <- c(my_plot = "individual")
  result <- .build_exploration_allowlist(
    selected_types = character(0),
    custom_names = custom
  )
  testthat::expect_equal(result, character(0))
})
