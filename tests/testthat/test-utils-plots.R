describe(".handle_tooltips", {
  it("adds tooltip_text column to data", {
    df <- data.frame(A = c(1, 2), B = c("x", "y"))
    result <- .handle_tooltips(df, tooltip_vars = "A", labels_df = NULL)
    expect_true("tooltip_text" %in% names(result))
  })

  it("rounds numeric tooltip variables to 2 digits", {
    df <- data.frame(A = 1.23456, B = "x")
    result <- .handle_tooltips(df, tooltip_vars = "A", labels_df = NULL)
    expect_equal(result$A, 1.23)
  })

  it("errors when data has zero rows", {
    df <- data.frame(A = numeric(0), B = character(0))
    expect_error(.handle_tooltips(df, tooltip_vars = "A", labels_df = NULL))
  })

  it("creates simple tooltip text when labels_df is NULL", {
    df <- data.frame(A = 1, B = "x")
    result <- .handle_tooltips(df, tooltip_vars = c("A", "B"), labels_df = NULL)
    expect_true(grepl("A: 1", result$tooltip_text))
    expect_true(grepl("B: x", result$tooltip_text))
  })

  it("uses generate_tooltip_text when labels_df is provided", {
    df <- data.frame(USUBJID = "S1-1", AVAL = 10.5)
    labels_df <- data.frame(
      Variable = c("USUBJID", "AVAL"),
      Label = c("Subject ID", "Analysis Value"),
      Dataset = c("ADNCA", "ADNCA")
    )
    result <- .handle_tooltips(df, tooltip_vars = c("USUBJID", "AVAL"), labels_df = labels_df)
    expect_true(grepl("Subject ID", result$tooltip_text))
    expect_true(grepl("10.5", result$tooltip_text))
  })
})

describe("error_plot", {
  it("returns a ggplot object", {
    p <- error_plot("Something went wrong")
    expect_s3_class(p, "ggplot")
  })

  it("has title 'Error'", {
    p <- error_plot("Something went wrong")
    expect_equal(p$labels$title, "Error")
  })
})
