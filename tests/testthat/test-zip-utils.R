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
