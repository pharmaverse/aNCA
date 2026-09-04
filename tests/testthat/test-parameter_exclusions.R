# Source shared exclusion helpers and the parameter exclusions module
source(
  file.path(
    system.file("shiny", package = "aNCA"),
    "functions", "utils-exclusions.R"
  ),
  local = TRUE
)
source(
  file.path(
    system.file("shiny", package = "aNCA"),
    "modules", "tab_nca", "setup", "parameter_exclusions.R"
  ),
  local = TRUE
)

describe(".build_exclusion_reasons", {
  it("returns empty indices and reasons for an empty list", {
    result <- .build_exclusion_reasons(list(), n_rows = 5)
    expect_equal(result$indices, integer(0))
    expect_equal(result$reasons, character(0))
  })

  it("returns correct indices and reasons for a single exclusion", {
    lst <- list(list(rows = c(1, 3), reason = "Outlier"))
    result <- .build_exclusion_reasons(lst, n_rows = 5)
    expect_equal(result$indices, c(1, 3))
    expect_equal(result$reasons, c("Outlier", "Outlier"))
  })

  it("concatenates reasons when multiple exclusions cover the same row", {
    lst <- list(
      list(rows = c(1, 2), reason = "Outlier"),
      list(rows = c(2, 3), reason = "Protocol deviation")
    )
    result <- .build_exclusion_reasons(lst, n_rows = 5)
    expect_equal(result$indices, c(1, 2, 3))
    expect_equal(result$reasons[1], "Outlier")
    expect_equal(result$reasons[2], "Outlier; Protocol deviation")
    expect_equal(result$reasons[3], "Protocol deviation")
  })

  it("ignores indices beyond n_rows", {
    lst <- list(list(rows = c(1, 10), reason = "Outlier"))
    result <- .build_exclusion_reasons(lst, n_rows = 5)
    expect_equal(result$indices, 1)
    expect_equal(result$reasons, "Outlier")
  })

  it("works without n_rows constraint", {
    lst <- list(list(rows = c(1, 100), reason = "Outlier"))
    result <- .build_exclusion_reasons(lst)
    expect_equal(result$indices, c(1, 100))
    expect_length(result$reasons, 2)
  })
})

describe(".build_param_display", {
  it("derives ANL01FL and ANL01FN from exclude column", {
    df <- data.frame(
      PPTESTCD = c("cmax", "tmax", "auclast"),
      PPTEST = c("Cmax", "Tmax", "AUClast"),
      PPORRES = c("10", "2", "50"),
      exclude = c("R2ADJ < 0.8", NA, ""),
      stringsAsFactors = FALSE
    )
    result <- .build_param_display(df, group_cols = character(0), manual_exclusions = list())
    expect_true("ANL01FL" %in% names(result))
    expect_true("ANL01FN" %in% names(result))
    expect_equal(result$ANL01FL, c("", "Y", "Y"), ignore_attr = TRUE)
    expect_equal(result$ANL01FN, c(NA_integer_, 1L, 1L), ignore_attr = TRUE)
  })

  it("layers manual exclusions on top of auto-populated values", {
    df <- data.frame(
      PPTESTCD = c("cmax", "tmax", "auclast"),
      PPTEST = c("Cmax", "Tmax", "AUClast"),
      PPORRES = c("10", "2", "50"),
      exclude = c("R2ADJ < 0.8", NA, ""),
      stringsAsFactors = FALSE
    )
    manual <- list(list(rows = c(1, 3), reason = "Manual reason"))
    result <- .build_param_display(df, group_cols = character(0), manual_exclusions = manual)
    expect_equal(result$ANL01FL, c("", "Y", ""), ignore_attr = TRUE)
    expect_equal(result$ANL01FN, c(NA_integer_, 1L, NA_integer_), ignore_attr = TRUE)
  })

  it("handles missing exclude column", {
    df <- data.frame(
      PPTESTCD = c("cmax", "tmax"),
      PPTEST = c("Cmax", "Tmax"),
      stringsAsFactors = FALSE
    )
    result <- .build_param_display(df, group_cols = character(0), manual_exclusions = list())
    expect_equal(result$ANL01FL, c("Y", "Y"), ignore_attr = TRUE)
    expect_equal(result$ANL01FN, c(1L, 1L), ignore_attr = TRUE)
  })

  it("selects only display columns plus group columns", {
    df <- data.frame(
      USUBJID = c("S1", "S2"),
      PPTESTCD = c("cmax", "tmax"),
      PPTEST = c("Cmax", "Tmax"),
      PPORRES = c("10", "2"),
      exclude = c(NA, NA),
      some_internal_col = c(1, 2),
      stringsAsFactors = FALSE
    )
    result <- .build_param_display(df, group_cols = "USUBJID", manual_exclusions = list())
    expect_true("USUBJID" %in% names(result))
    expect_true("PPTESTCD" %in% names(result))
    expect_false("some_internal_col" %in% names(result))
    expect_false("exclude" %in% names(result))
  })
})
