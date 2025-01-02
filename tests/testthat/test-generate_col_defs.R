source(system.file("shiny/functions/generate_col_defs.R", package = "aNCA"))

describe("generate_col_defs", {
  it("generates column definitions with labels", {
    data <- data.frame(
      USUBJID = c(1, 2, 3),
      AVAL = c(4, 5, 6)
    )
    attr(data$USUBJID, "label") <- "Unique Subject Identifier"
    attr(data$AVAL, "label") <- "Analysis Value"
    col_defs <- generate_col_defs(data)
    expect_equal(col_defs$USUBJID$header$attribs$title, "Unique Subject Identifier")
    expect_equal(col_defs$AVAL$header$attribs$title, "Analysis Value")
  })
  it("returns a list of column definitions", {
    data <- data.frame(
      USUBJID = c(1, 2, 3),
      AVAL = c(4, 5, 6)
    )
    col_defs <- generate_col_defs(data)
    expect_type(col_defs, "list")
  })
})