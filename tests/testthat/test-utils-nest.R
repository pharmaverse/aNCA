describe("NEST dependency and metadata boundary", {
  it("explains how to install an unavailable optional dependency", {
    expect_error(.nest_require("anca1540_missing_package"),
                 "NEST pilot requires: anca1540_missing_package.*install.packages")
  })

  it("falls back to column names when imported label metadata is blank", {
    d <- data.frame(x = 1)
    attr(d$x, "label") <- ""
    expect_equal(.nest_label(d, "x"), "x")
    attr(d$x, "label") <- NA_character_
    expect_equal(.nest_label(d, "x"), "x")
  })

  it("formats rounded results without session significant digits", {
    withr::local_options(list(digits = 1))
    expect_identical(.nest_format_number(c(123456.789, 12808.737, 0.5, -0, NA)),
                     c("123456.789", "12808.737", "0.5", "0", ""))
  })

  it("names each interval of interval parameters and keeps the label", {
    d <- nest_interval_fixture()
    attr(d$PARAM, "label") <- "Parameter"
    out <- .nest_interval_param(d, "PARAM")
    expect_setequal(unique(out$PARAM), c("Cmax", "AUC (PT0H-PT2H)", "AUC (PT0H-PT6H)"))
    expect_equal(attr(out$PARAM, "label"), "Parameter")
    expect_identical(.nest_interval_param(nest_adpp_fixture(), "PARAM"), nest_adpp_fixture())
  })

  it("names the reporting key in conflict errors", {
    d <- data.frame(USUBJID = NA, PARAM = "AUC", AVAL = c(1, 2))
    expect_error(.nest_check_cells(d, c("USUBJID", "PARAM"), "AVAL"),
                 "key (USUBJID, PARAM). Check for missing key values", fixed = TRUE)
  })
})
