library(testthat)

# Sample metadata for testing
dict_pknca_cdisc <- data.frame(
  PKNCA = c("adj.r.squared", "ae", "aucabove.predose.all"),
  CDISC = c("R2ADJ", "RCAMINT", "AUCABVPA"),
  stringsAsFactors = FALSE
)

test_that("translate_terms translates terms correctly", {
  input_terms <- c("adj.r.squared", "ae", "nonexistent_term")
  expected_output <- c("R2ADJ", "RCAMINT", "nonexistent_term")
  result <- translate_terms(input_terms, metadata = dict_pknca_cdisc)
  expect_equal(result, expected_output)
})

test_that("translate_terms handles missing mapping_col gracefully", {
  expect_error(
    translate_terms(c("adj.r.squared"), mapping_col = "NONEXISTENT", metadata = dict_pknca_cdisc),
    "mapping_col and target_col must exist in metadata."
  )
})

test_that("translate_terms handles missing target_col gracefully", {
  expect_error(
    translate_terms(c("adj.r.squared"), target_col = "NONEXISTENT", metadata = dict_pknca_cdisc),
    "mapping_col and target_col must exist in metadata."
  )
})

test_that("translate_terms handles invalid mapping_col and target_col types", {
  expect_error(
    translate_terms(c("adj.r.squared"), mapping_col = 123, metadata = dict_pknca_cdisc),
    "mapping_col and target_col must be single character strings."
  )
  expect_error(
    translate_terms(c("adj.r.squared"), target_col = TRUE, metadata = dict_pknca_cdisc),
    "mapping_col and target_col must be single character strings."
  )
})

test_that("translate_terms handles missing metadata using the default from dict_pknca_cdisc.rds", {
  input_terms <- c("adj.r.squared", "ae", "nonexistent_term")
  expected_output <- c("R2ADJ", "RCAMINT", "nonexistent_term")
  result <- translate_terms(input_terms)
  expect_equal(result, expected_output)
})
