describe("generate_pre_specs", {
  it("returns a named list with one entry per requested dataset", {
    result <- generate_pre_specs(c("ADNCA", "ADPP", "PP"))
    expect_type(result, "list")
    expect_named(result, c("ADNCA", "ADPP", "PP"))
  })

  it("each entry contains exactly the specification columns", {
    result <- generate_pre_specs()
    expected_cols <- c("Dataset", "Order", "Variable", "Label", "Type", "Role", "Core", "Length")
    for (ds in names(result)) {
      expect_named(result[[ds]], expected_cols)
    }
  })

  it("rows are sorted by Order within each dataset", {
    result <- generate_pre_specs()
    for (ds in names(result)) {
      orders <- result[[ds]]$Order
      expect_equal(orders, sort(orders))
    }
  })

  it("filters to only the requested datasets", {
    result <- generate_pre_specs("ADPP")
    expect_named(result, "ADPP")
    expect_true(all(result$ADPP$Dataset == "ADPP"))
  })

  it("returns non-empty data frames for each dataset", {
    result <- generate_pre_specs()
    for (ds in names(result)) {
      expect_gt(nrow(result[[ds]]), 0)
    }
  })

  it("narrows specs to columns present in cdisc_data when provided", {
    mock_data <- list(
      pp = data.frame(STUDYID = "S1", USUBJID = "U1", PPSEQ = 1),
      adpp = data.frame(STUDYID = "S1", AVAL = 1.0),
      adnca = data.frame(STUDYID = "S1", PARAM = "A", AVAL = 1.0)
    )
    result <- generate_pre_specs(cdisc_data = mock_data)
    for (ds in names(result)) {
      key <- c(PP = "pp", ADPP = "adpp", ADNCA = "adnca")[[ds]]
      expect_true(all(result[[ds]]$Variable %in% names(mock_data[[key]])))
    }
  })

  it("returns all specs when cdisc_data is NULL", {
    full <- generate_pre_specs("PP", cdisc_data = NULL)
    expect_gt(nrow(full$PP), 3)
  })
})
