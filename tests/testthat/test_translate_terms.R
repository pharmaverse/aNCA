# Sample metadata for testing
metadata_nca_parameters <- data.frame(
  PKNCA = c("adj.r.squared", "ae", "aucabove.predose.all"),
  PPTESTCD = c("R2ADJ", "RCAMINT", "AUCABVPA"),
  stringsAsFactors = FALSE
)

describe("translate_terms", {
  it("translates terms correctly", {
    input_terms <- c("adj.r.squared", "ae", "nonexistent_term")
    expected_output <- c("R2ADJ", "RCAMINT", "nonexistent_term")
    result <- translate_terms(input_terms, "PKNCA", "PPTESTCD", metadata = metadata_nca_parameters)
    expect_equal(result, expected_output)
  })

  it("handles missing mapping_col gracefully", {
    expect_error(
      translate_terms(c("adj.r.squared"), mapping_col = "NONEXISTENT",
                      metadata = metadata_nca_parameters),
      "mapping_col and target_col must exist in metadata."
    )
  })

  it("handles missing target_col gracefully", {
    expect_error(
      translate_terms(
        c("adj.r.squared"),
        target_col = "NONEXISTENT",
        metadata = metadata_nca_parameters
      ),
      "mapping_col and target_col must exist in metadata."
    )
  })

  it("handles invalid mapping_col and target_col types", {
    expect_error(
      translate_terms(c("adj.r.squared"), mapping_col = 123, metadata = metadata_nca_parameters),
      "mapping_col and target_col must be single character strings."
    )
    expect_error(
      translate_terms(c("adj.r.squared"), target_col = TRUE, metadata = metadata_nca_parameters),
      "mapping_col and target_col must be single character strings."
    )
  })

  it("handles missing metadata using the default from metadata_nca_parameters.rds", {
    input_terms <- c("adj.r.squared", "ae", "nonexistent_term")
    expected_output <- c("R2ADJ", "RCAMINT", "nonexistent_term")
    result <- translate_terms(input_terms)
    expect_equal(result, expected_output)
  })
})
