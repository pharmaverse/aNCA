LABELS <- data.frame(
  Variable = c("USUBJID", "AVAL"),
  Label = c("Unique Subject Identifier", "Analysis Value"),
  Dataset = c("ADPC", "ADPC")
)

describe("get_labels", {
  it("return label of heading if it exists in label file", {
    expect_equal(get_label(LABELS, "USUBJID", "ADPC"), "Unique Subject Identifier")
    expect_error(get_label())
    expect_error(get_label(LABELS, "USUBJID"))
    expect_no_error(get_label(LABELS, "USUBJID", "ADP")) 
    expect_equal(get_label(LABELS, "USUBJID", "ADP"), "No label available")
  })
})
