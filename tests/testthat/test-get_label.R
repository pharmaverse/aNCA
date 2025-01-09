LABELS <- data.frame(
  Variable = c("USUBJID", "AVAL"),
  Label = c("Unique Subject Identifier", "Analysis Value"),
  Dataset = c("ADPC", "ADPC")
)

describe("get_label", {
  it("gets correct label from LABELs dataframe", {

    get_label(LABELS, "USUBJID", "ADPC")  
    get_label(LABELS, "HEIGHT", "ADPC")
    
    expect_equal(get_label(LABELS, "USUBJID", "ADPC"), "Unique Subject Identifier")
    expect_equal(get_label(LABELS, "HEIGHT", "ADPC"), "No label available")
    
  })
})