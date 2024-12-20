describe("apply_labels", {
  it("applies labels to matching data", {
    data <- data.frame(
      USUBJID = c(1, 2, 3),
      AVAL = c(4, 5, 6)
    )
    data <- apply_labels(data)
    expect_equal(attr(data$USUBJID, "label"), "Unique Subject Identifier")
    expect_equal(attr(data$AVAL, "label"), "Analysis Value")

  })
  it("appplies labels to non matching data", {
    data <- data.frame(
      COL1 = c(1, 2, 3),
      COL2 = c(4, 5, 6)
    )
    data <- apply_labels(data)
    expect_equal(attr(data$COL1, "label"), "COL1")
    expect_equal(attr(data$COL2, "label"), "COL2")
  })
})