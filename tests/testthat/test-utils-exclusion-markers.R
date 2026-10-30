describe("TLG exclusion reason dictionaries", {
  it("separates combined reasons from individual and unspecified exclusions", {
    data <- data.frame(
      AVAL = c(1, 2, NA, 4), PKSUMXF = "Y",
      PKSUM1RS = c("B; A; A", "C", "", "B")
    )
    marked <- .mark_sum_excl_vals(data, "PKSUMXF", "AVAL")
    expect_equal(marked$AVAL, c("1**, ***", "2****", "NA*", "4***"))
    expect_equal(.add_sum_excl_footnote(NULL, data, "PKSUMXF"), c(
      '** Summary and mean plots excluded: PKSUM1RS = "A".',
      '*** Summary and mean plots excluded: PKSUM1RS = "B".',
      '**** Summary and mean plots excluded: PKSUM1RS = "C".',
      '* Summary and mean plots excluded (PKSUMXF = "Y").'
    ))
    expect_equal(data$AVAL, c(1, 2, NA, 4))
  })

  it("does not mislabel raw NCA reasons as summary reasons", {
    data <- data.frame(AVAL = 2, PKSUMXF = "Y", exclude = "NCA only")
    expect_equal(.mark_sum_excl_vals(data, "PKSUMXF", "AVAL")$AVAL, "2*")
    expect_equal(.add_sum_excl_footnote(NULL, data, "PKSUMXF"),
                 '* Summary and mean plots excluded (PKSUMXF = "Y").')
    expect_equal(.mark_nca_excl_vals(data, "NCAXFL", "AVAL")$AVAL, "2#")
    expect_equal(.add_nca_excl_footnote(NULL, data, "NCAXFL"),
                 '# NCA excluded record: exclude = "NCA only".')
  })

  it("uses only flagged rows when choosing footnotes and their source columns", {
    data <- data.frame(
      PKSUMXF = c("Y", "Y", ""), PKSUM1RS = c("A", "B", "A")
    )
    dictionary <- .exclusion_details(data, "PKSUMXF", "*", TRUE)$dictionary
    expect_equal(.add_sum_excl_footnote(NULL, data[-1, ], "PKSUMXF", dictionary),
                 '** Summary and mean plots excluded: PKSUM1RS = "B".')
    expect_null(.add_sum_excl_footnote(NULL, data[3, ], "PKSUMXF", dictionary))
  })

  it("handles empty and missing reasons without inventing markers", {
    data <- data.frame(AVAL = c(1, NA), PKSUMXF = c("", NA), PKSUM1RS = c("", NA))
    expect_identical(.mark_sum_excl_vals(data, "PKSUMXF", "AVAL"), data)
    expect_null(.add_sum_excl_footnote(NULL, data, "PKSUMXF"))
    expect_identical(.exclusion_details(data[FALSE, ], "PKSUMXF", "*", TRUE)$markers,
                     character())
  })
})
