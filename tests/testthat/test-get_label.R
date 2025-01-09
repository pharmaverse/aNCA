describe("get_label", {
  it("gets correct label from LABELs dataframe", {
    LABELS <- data.frame(
      Variable = c("USUBJID", "AVAL"),
      Label = c("Unique Subject Identifier", "Analysis Value"),
      Dataset = c("ADPC", "ADPC")
      )

    get_label("USUBJID", "ADPC")  
    get_label("HEIGHT", "ADPC")
    
    expect_equal(get_label("USUBJID", "ADPC"), "Unique Subject Identifier")
    expect_equal(get_label("HEIGHT", "ADPC"), "No label available")
    
  })
})