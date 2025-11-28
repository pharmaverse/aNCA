# Tests for clean_deparse function
describe("clean_deparse()", {
  it("formats character single and vector correctly", {
    expect_equal(clean_deparse("hello"), '"hello"')
    expect_equal(clean_deparse(c("a", "b")), 'c("a", "b")')
  })

  it("formats numeric and integer values correctly", {
    expect_equal(clean_deparse(1.23), "1.23")
    expect_equal(clean_deparse(c(1, 2)), "c(1, 2)")
    expect_equal(clean_deparse(as.integer(3)), "3")
    expect_equal(clean_deparse(as.integer(c(4L,5L))), "c(4, 5)")
  })

  it("formats logical values correctly", {
    expect_equal(clean_deparse(TRUE), "TRUE")
    expect_equal(clean_deparse(c(TRUE, FALSE)), "c(TRUE, FALSE)")
  })

  it("formats named lists correctly", {
    l <- list(a = 1, b = "x")
    exp_named <- "list(\n  a = 1,\n  b = \"x\"\n)"
    expect_equal(clean_deparse(l), exp_named)
  })

  it("renders data.frame as data.frame(...) with per-column vectors", {
    df <- data.frame(x = c(1,2), y = c("a", "b"), stringsAsFactors = FALSE)
    exp_df <- paste0(
      "data.frame(\n",
      "  x = c(1, 2),\n",
      "  y = c(\"a\", \"b\")\n",
      ")"
    )
    expect_equal(clean_deparse(df), exp_df)
  })
})
