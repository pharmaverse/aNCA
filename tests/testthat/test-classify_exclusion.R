describe("classify_exclusion: category assignment", {
  it("assigns flag / manual / both / none correctly", {
    is_flag <- c(TRUE, FALSE, TRUE, FALSE)
    is_manual <- c(FALSE, TRUE, TRUE, FALSE)
    expect_equal(
      aNCA:::.classify_exclusion(is_flag, is_manual),
      c("flag", "manual", "both", "none")
    )
  })

  it("treats NA as FALSE", {
    expect_equal(
      aNCA:::.classify_exclusion(c(NA, TRUE), c(NA, NA)),
      c("none", "flag")
    )
  })

  it("recycles inputs of different length", {
    expect_equal(
      aNCA:::.classify_exclusion(TRUE, c(FALSE, TRUE)),
      c("flag", "both")
    )
  })

  it("returns empty character for empty input", {
    expect_equal(
      aNCA:::.classify_exclusion(logical(0), logical(0)),
      character(0)
    )
  })
})

describe("exclusion_type_color: palette mapping", {
  it("maps categories to background colours", {
    expect_equal(
      aNCA:::.exclusion_type_color(c("flag", "manual", "both")),
      unname(aNCA:::EXCL_TYPE_COLORS[c("flag", "manual", "both")])
    )
  })

  it("returns NA for none / unknown", {
    expect_true(is.na(aNCA:::.exclusion_type_color("none")))
    expect_true(is.na(aNCA:::.exclusion_type_color("bogus")))
  })

  it("uses the saturated palette for points", {
    expect_equal(
      aNCA:::.exclusion_type_color("flag", point = TRUE),
      unname(aNCA:::EXCL_TYPE_POINT_COLORS[["flag"]])
    )
  })
})
