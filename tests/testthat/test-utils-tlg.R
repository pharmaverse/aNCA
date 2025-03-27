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
      parse_annotation(mock_data, "Label: !GROUP, Dose: !DOSE"),
      "Label: ERR, Dose: Administered dose"
    )
  })
})

describe("parse_tlg_definitions", {
  it("parses TLG definitions correctly", {
    defs <- list(
      TLG1 = list(
        options = list(
          option1 = "value1"
        )
      ),
      TLG2 = list(
        option2 = "value2"
      )
    )

    # If no special keys are provided, the function should return the same list
    expect_equal(parse_tlg_definitions(defs = defs), defs)
  })

  it("deals with templates correctly", {
    defs <- list(
      TLG1 = list(
        is_default = TRUE,
        type = "Graph",
        label = "TLG1",
        options = list(
          xvar = "X",
          yvar = "Y"
        )
      ),
      TLG2 = list(
        template = "TLG1",
        label = "TLG2",
        options = list(
          yvar = "Z"
        )
      )
    )

    expect_equal(
      parse_tlg_definitions(defs = defs),
      list(
        TLG1 = list(
          is_default = TRUE,
          type = "Graph",
          label = "TLG1",
          options = list(
            xvar = "X",
            yvar = "Y"
          )
        ),
        TLG2 = list(
          is_default = TRUE,
          type = "Graph",
          label = "TLG2",
          options = list(
            xvar = "X",
            yvar = "Z"
          )
        )
      )
    )
  })
})