LABELS <- data.frame(
  Variable = c("USUBJID", "AVAL"),
  Label = c("Unique Subject Identifier", "Analysis Value"),
  Dataset = c("ADPC", "ADPC")
)

describe("get_labels", {

  # Test for successful label retrieval
  it("returns label of a heading if it exists in the label file", {
    expect_equal(get_label(LABELS, "USUBJID", "ADPC"), "Unique Subject Identifier")
  })

  # Test for fallback behavior when no label is available
  it("returns 'No label available' if the label does not exist", {
    expect_equal(get_label(LABELS, "USUBJID", "ADP"), "No label available")
  })
})