describe(".eval_range", {
  it("should generate simple range correctly", {
    expect_equal(.eval_range("1:5"), c(1, 2, 3, 4, 5))
    expect_equal(.eval_range("4:6"), c(4, 5, 6))
  })
  it("should parse sequences of singular numbers correctly", {
    expect_equal(.eval_range("1,5,6"), c(1, 5, 6))
    expect_equal(.eval_range("3,4,2"), c(3, 4, 2))
  })
  it("should parse mixed sequences correctly", {
    expect_equal(.eval_range("1:4,9"), c(1, 2, 3, 4, 9))
    expect_equal(.eval_range("5,3:1,15"), c(5, 3, 2, 1, 15))
  })
  it("should return NA when input is not parsed correctly", {
    expect_identical(.eval_range("test"), NA)
    expect_identical(.eval_range("1::33;"), NA)
  })
})

describe(".compress_range", {
  it("compreses simple range correctly", {
    expect_equal(.compress_range(c(1, 2, 3, 4)), "1:4")
    expect_equal(.compress_range(c(1, 5, 10, 15)), "1,5,10,15")
  })

  it("compreses range with breaks correctly", {
    expect_equal(.compress_range(c(1, 2, 3, 4, 5, 10, 11, 12, 15)), "1:5,10:12,15")
  })

  it("handles unsorted vectors correctly", {
    expect_equal(.compress_range(c(15, 1, 11, 4, 5, 10, 2, 12, 3)), "1:5,10:12,15")
  })

  it("coerces character vectors to numeric if possible", {
    expect_equal(.compress_range(c("1", "2", "03")), "1:3")
  })

  it("returns NA when empty vector is provided", {
    expect_true(is.na(.compress_range(c())))
  })

  it("throws an error if any values in the vector cannot be coerced to numeric", {
    expect_error(.compress_range(c(1, 2, "A", 4)), "Error: only numeric values allowed")
  })
})

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

describe(".plotly_empty_plot", {
  it("returns a plotly object with default message if no argument is not provided", {
    plot <- .plotly_empty_plot()
    expect_s3_class(plot, "plotly")
    annotations <- plot$x$layoutAttrs[[1]]$annotations
    expect_equal(annotations$text, "No data available")
  })

  it("displays a custom message when provided", {
    custom_message <- "Custom message"
    plot <- .plotly_empty_plot(custom_message)
    annotations <- plot$x$layoutAttrs[[1]]$annotations
    expect_equal(annotations$text, "Custom message")
  })

  it("hides axes in the plot", {
    plot <- .plotly_empty_plot()
    expect_false(plot$x$layoutAttrs[[1]]$xaxis$visible)
    expect_false(plot$x$layoutAttrs[[1]]$yaxis$visible)
  })
})

describe(".concatenate_list", {
  it("formats a simple named list correctly", {
    l <- list(a = 1, b = 2:3)
    result <- .concatenate_list("Test List", l)
    expect_true(grepl("Test List", result))
    expect_true(grepl("* a -> 1", result))
    expect_true(grepl("* b -> 2, 3", result))
  })

  it("formats a nested list correctly", {
    l <- list(main = list(sub1 = 1, sub2 = c(2,3)))
    result <- .concatenate_list("Nested List", l)
    expect_true(grepl("Nested List", result))
    expect_true(grepl("sub1", result))
    expect_true(grepl("sub2", result))
    expect_true(grepl("1", result))
    expect_true(grepl("2, 3", result))
  })

  it("formats a data.frame as a list of rows", {
    df <- data.frame(x = 1:2, y = c("A", "B"))
    result <- .concatenate_list("Data Frame", df)
    expect_true(grepl("Data Frame", result))
    expect_true(grepl("* 1 -> 1, A", result))
    expect_true(grepl("* 2 -> 2, B", result))
  })

  it("handles empty list", {
    l <- list()
    result <- .concatenate_list("Empty List", l)
    expect_true(grepl("Empty List", result))
  })
})
