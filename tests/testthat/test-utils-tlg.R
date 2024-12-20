describe("parse_annotation", {
  mock_data <- tibble(
    GROUP = "XX01",
    DOSE = "10",
    DOSEU = "mg"
  )
  attr(mock_data[["DOSE"]], "label") <- "Administered dose"

  it("parses title string correctly", {
    expect_equal(
      parse_annotation(mock_data, "Group $GROUP\n!DOSE: $DOSE [$DOSEU]"),
      "Group XX01<br>Administered dose: 10 [mg]"
    )
  })

  it("substitutes missing variables with ERR", {
    expect_equal(
      parse_annotation(mock_data, "Column: $INVALID"),
      "Column: ERR"
    )

    expect_equal(
      parse_annotation(mock_data, "Label: !GROUP"),
      "Label: ERR"
    )
  })
})