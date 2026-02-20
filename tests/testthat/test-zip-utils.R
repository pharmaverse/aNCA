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

testthat::test_that(".drop_defaults_with_custom keeps defaults when no custom", {
  exploration <- list(individualplot = "a", meanplot = "b", qcplot = "c")
  result <- .drop_defaults_with_custom(exploration, character(0))
  testthat::expect_equal(names(result), c("individualplot", "meanplot", "qcplot"))
})

testthat::test_that(".drop_defaults_with_custom drops default when custom exists", {
  exploration <- list(
    individualplot = "a", individualplot1 = "b",
    meanplot = "c", meanplot1 = "d", meanplot2 = "e",
    qcplot = "f"
  )
  custom <- c("individualplot1", "meanplot1", "meanplot2")
  result <- .drop_defaults_with_custom(exploration, custom)
  testthat::expect_equal(
    names(result),
    c("individualplot1", "meanplot1", "meanplot2", "qcplot")
  )
})

testthat::test_that(".drop_defaults_with_custom keeps unrelated items", {
  exploration <- list(
    individualplot = "a", meanplot = "b", qcplot = "c",
    custom_thing = "d"
  )
  result <- .drop_defaults_with_custom(exploration, c("meanplot1"))
  testthat::expect_true("individualplot" %in% names(result))
  testthat::expect_false("meanplot" %in% names(result))
  testthat::expect_true("qcplot" %in% names(result))
  testthat::expect_true("custom_thing" %in% names(result))
})
