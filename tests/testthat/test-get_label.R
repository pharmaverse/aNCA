describe("get_label", {
  it("gets correct label from LABELs dataframe", {
    
    get_label("USUBJID", "ADPC")  
    get_label("HEIGHT", "ADPC")
    
    expect_equal(get_label("USUBJID", "ADPC"), "Unique Subject Identifier")
    expect_equal(get_label("HEIGHT", "ADPC"), "No label available")
    
  })
})