# Sample metadata for testing
pknca_cdisc_terms <- data.frame(
  PKNCA = c("adj.r.squared", "ae", "aucabove.predose.all"),
  PPTESTCD = c("R2ADJ", "RCAMINT", "AUCABVPA"),
  stringsAsFactors = FALSE
)

describe("translate_terms", {
  it("translates terms correctly", {
    input_terms <- c("adj.r.squared", "ae", "nonexistent_term")
    expected_output <- c("R2ADJ", "RCAMINT", "nonexistent_term")
    result <- translate_terms(input_terms, "PKNCA", "PPTESTCD", metadata = pknca_cdisc_terms)
    expect_equal(result, expected_output)
  })

  it("handles missing mapping_col gracefully", {
    expect_error(
      translate_terms(c("adj.r.squared"), mapping_col = "NONEXISTENT",
                      metadata = pknca_cdisc_terms),
      "mapping_col and target_col must exist in metadata."
    )
  })

  it("handles missing target_col gracefully", {
    expect_error(
      translate_terms(c("adj.r.squared"), target_col = "NONEXISTENT", metadata = pknca_cdisc_terms),
      "mapping_col and target_col must exist in metadata."
    )
  })

  it("handles invalid mapping_col and target_col types", {
    expect_error(
      translate_terms(c("adj.r.squared"), mapping_col = 123, metadata = pknca_cdisc_terms),
      "mapping_col and target_col must be single character strings."
    )
    expect_error(
      translate_terms(c("adj.r.squared"), target_col = TRUE, metadata = pknca_cdisc_terms),
      "mapping_col and target_col must be single character strings."
    )
  })

  it("handles missing metadata using the default from pknca_cdisc_terms.rds", {
    input_terms <- c("adj.r.squared", "ae", "nonexistent_term")
    expected_output <- c("R2ADJ", "RCAMINT", "nonexistent_term")
    result <- translate_terms(input_terms)
    expect_equal(result, expected_output)
  })
})
