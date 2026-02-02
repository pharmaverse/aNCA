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
    expect_equal(clean_deparse(logical()), "logical()")
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
    expect_match(out, "c\\(\n") # Should use newlines for long columns
    expect_match(out, "    1, 2, 3") # Indentation present
    expect_match(out, "x = c\\(") # Column x uses c( ... )
    expect_match(out, "y = c\\(") # Column y uses c( ... )
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

  it("uses rep(...) when values are repeated at least min_to_rep times", {
    # Test character
    char_vec <- c(rep("apple", 5), "banana", "cherry", rep("date", 3))
    out_char <- clean_deparse.character(char_vec, max_per_line = 3, min_to_rep = 2)
    exp_out_char <- paste0(
      "c(\n",
      "  rep(\"apple\", 5), \"banana\", \"cherry\",\n",
      "  rep(\"date\", 3)\n",
      ")"
    )
    expect_equal(out_char, exp_out_char)

    # Test numeric
    vec <- c(rep(5, 10), 6, 7, rep(8, 5))
    out_vec <- clean_deparse(vec, max_per_line = 3, min_to_rep = 2)
    exp_out_vec <- paste0(
      "c(\n",
      "  rep(5, 10), 6, 7,\n",
      "  rep(8, 5)\n",
      ")"
    )
    expect_equal(out_vec, exp_out_vec)

    # Test min_to_rep greater than any repetition
    vec <- c(1, rep(2, 2), rep(3, 3), rep(4, 4))

    out_no_rep <- clean_deparse(vec, max_per_line = 10, min_to_rep = Inf)
    out_1_to_rep <- clean_deparse(vec, max_per_line = 10, min_to_rep = 1)
    out_2_to_rep <- clean_deparse(vec, max_per_line = 10, min_to_rep = 2)
    out_3_to_rep <- clean_deparse(vec, max_per_line = 10, min_to_rep = 3)
    out_4_to_rep <- clean_deparse(vec, max_per_line = 10, min_to_rep = 4)

    exp_out_no_rep <- "c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)"
    exp_out_1_to_rep <- "c(rep(1, 1), rep(2, 2), rep(3, 3), rep(4, 4))"
    exp_out_2_to_rep <- "c(1, rep(2, 2), rep(3, 3), rep(4, 4))"
    exp_out_3_to_rep <- "c(1, 2, 2, rep(3, 3), rep(4, 4))"
    exp_out_4_to_rep <- "c(1, 2, 2, 3, 3, 3, rep(4, 4))"

    expect_equal(out_no_rep, exp_out_no_rep)
    expect_equal(out_2_to_rep, exp_out_2_to_rep)
    expect_equal(out_3_to_rep, exp_out_3_to_rep)
    expect_equal(out_4_to_rep, exp_out_4_to_rep)
  })

  it("interprets tbl_df as data.frame", {
    # check for filled object
    df <- dplyr::tibble(a = 1:3, b = c("x", "y", "z"))
    out <- clean_deparse(df)
    exp_out <- clean_deparse(as.data.frame(df))
    expect_equal(out, exp_out)

    # check for empty object
    df <- dplyr::tibble()
    out <- clean_deparse(df)
    exp_out <- clean_deparse(data.frame())
    expect_equal(out, exp_out)
  })
})
