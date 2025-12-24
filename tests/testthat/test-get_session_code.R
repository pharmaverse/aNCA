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
    expect_equal(clean_deparse(as.integer(c(4L, 5L))), "c(4, 5)")
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

  it("formats named lists with non-syntactic names correctly", {
    l <- list("first item" = 1, "second-item" = "x")
    exp_named <- "list(\n  \"first item\" = 1,\n  \"second-item\" = \"x\"\n)"
    expect_equal(clean_deparse(l), exp_named)
  })

  it("renders data.frame as data.frame(...) with per-column vectors", {
    df <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE)
    exp_df <- paste0(
      "data.frame(\n",
      "  x = c(1, 2),\n",
      "  y = c(\"a\", \"b\")\n",
      ")"
    )
    expect_equal(clean_deparse(df), exp_df)
  })

  it("renders tbl_df as data.frame(...)", {
    df <- dplyr::tibble(x = c(1, 2), y = c("a", "b"))
    exp_df <- paste0(
      "data.frame(\n",
      "  x = c(1, 2),\n",
      "  y = c(\"a\", \"b\")\n",
      ")"
    )
    expect_equal(clean_deparse(df), exp_df)
  })

  it("renders a NULL object correctly", {
    expect_equal(clean_deparse(NULL), "NULL")
  })

  it("renders empty classes correctly", {
    expect_equal(clean_deparse(list()), "list()")
    expect_equal(clean_deparse(data.frame()), "data.frame()")
    expect_equal(clean_deparse(character(0)), "character()")
    expect_equal(clean_deparse(numeric(0)), "numeric()")
  })

  it("renders data.frame with long columns using newlines and indentation", {
    long_vec <- as.character(1:25)
    df <- data.frame(x = 1:25, y = long_vec, stringsAsFactors = FALSE)
    out <- clean_deparse(df)
    expect_true(grepl("c\\(\n", out)) # Should use newlines for long columns
    expect_true(grepl("    1, 2, 3", out)) # Indentation present
    expect_true(grepl("x = c\\(", out)) # Column x uses c( ... )
    expect_true(grepl("y = c\\(", out)) # Column y uses c( ... )
  })

  it("respects max_per_line for character vectors", {
    v <- as.character(1:6)
    out <- clean_deparse(v, max_per_line = 2)
    exp_out <- "c(\n  \"1\", \"2\",\n  \"3\", \"4\",\n  \"5\", \"6\"\n)"
    expect_equal(out, exp_out)
  })

  it("respects max_per_line for numeric vectors", {
    v <- 1:6
    out <- clean_deparse(v, max_per_line = 2)
    exp_out <- "c(\n  1, 2,\n  3, 4,\n  5, 6\n)"
    expect_equal(out, exp_out)
  })

  it("respects max_per_line for lists of vectors", {
    l <- list(a = 1:4, b = letters[1:4])
    out <- clean_deparse(l, max_per_line = 2)
    exp_out <- paste0(
      "list(\n",
      "  a = c(\n",
      "    1, 2,\n",
      "    3, 4\n",
      "  ),\n",
      "  b = c(\n",
      '    "a", "b",\n',
      '    "c", "d"\n',
      "  )\n",
      ")"
    )
    expect_equal(out, exp_out)
  })

  it("respects max_per_line for data.frames", {
    df <- data.frame(x = 1:4, y = letters[1:4], stringsAsFactors = FALSE)
    out <- clean_deparse(df, max_per_line = 2)
    exp_out <- paste0(
      "data.frame(\n",
      "  x = c(\n",
      "    1, 2,\n",
      "    3, 4\n",
      "  ),\n",
      "  y = c(\n",
      '    "a", "b",\n',
      '    "c", "d"\n',
      "  )\n",
      ")"
    )
    expect_equal(out, exp_out)
  })
})
