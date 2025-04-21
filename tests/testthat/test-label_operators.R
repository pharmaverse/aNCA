
LABELS <- data.frame(
   Variable = c("USUBJID", "AVAL"),
   Label = c("Unique Subject Identifier", "Analysis Value")
)
#'   data <- apply_labels(data, labels)
#'   print(attr(data$A, "label"))
   
describe("apply_labels", {
  
  # Test for successful label application
  it("applies labels to the data frame", {
    data <- data.frame(USUBJID = 1:3, AVAL = c(10, 20, 30))
    labeled_data <- apply_labels(data, LABELS)
    expect_equal(attr(labeled_data$USUBJID, "label"), "Unique Subject Identifier")
    expect_equal(attr(labeled_data$AVAL, "label"), "Analysis Value")
  })
}

  # Test for no labels applied when no matching labels exist
  it("does not apply labels if no matching labels exist", {
    data <- data.frame(USUBJID = 1:3, AVAL = c(10, 20, 30))
    labeled_data <- apply_labels(data, data.frame(Variable = character(), Label = character()))
    expect_null(attr(labeled_data$USUBJID, "label"))
    expect_null(attr(labeled_data$AVAL, "label"))
  })
})

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