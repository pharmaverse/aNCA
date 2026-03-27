# Source exclusion helpers (extracted to utils-exclusions.R) and the module
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
    "modules", "tab_nca", "setup", "general_exclusions.R"
  ),
  local = TRUE
)

describe(".row_exclusion_flags", {
  it("returns FALSE/FALSE when row is not in any exclusion", {
    exclusions <- list(
      list(rows = c(1, 2), exclude_nca = TRUE, exclude_tlg = FALSE)
    )
    result <- .row_exclusion_flags(3, exclusions)
    expect_false(result$nca)
    expect_false(result$tlg)
  })

  it("detects NCA exclusion", {
    exclusions <- list(
      list(rows = c(1, 2), exclude_nca = TRUE, exclude_tlg = FALSE)
    )
    result <- .row_exclusion_flags(1, exclusions)
    expect_true(result$nca)
    expect_false(result$tlg)
  })

  it("detects TLG exclusion", {
    exclusions <- list(
      list(rows = c(3), exclude_nca = FALSE, exclude_tlg = TRUE)
    )
    result <- .row_exclusion_flags(3, exclusions)
    expect_false(result$nca)
    expect_true(result$tlg)
  })

  it("detects both NCA and TLG from separate entries", {
    exclusions <- list(
      list(rows = c(1), exclude_nca = TRUE, exclude_tlg = FALSE),
      list(rows = c(1), exclude_nca = FALSE, exclude_tlg = TRUE)
    )
    result <- .row_exclusion_flags(1, exclusions)
    expect_true(result$nca)
    expect_true(result$tlg)
  })

  it("returns FALSE/FALSE for empty exclusion list", {
    result <- .row_exclusion_flags(1, list())
    expect_false(result$nca)
    expect_false(result$tlg)
  })
})

describe(".exclusion_row_color", {
  base_data <- data.frame(
    x = 1:3,
    nca_exclude = c("", "default reason", ""),
    stringsAsFactors = FALSE
  )

  it("returns NULL for rows with no exclusion", {
    result <- .exclusion_row_color(1, base_data, list())
    expect_null(result)
  })

  it("returns NCA color for default NCA exclusion from data", {
    result <- .exclusion_row_color(2, base_data, list())
    expect_equal(result$background, EXCL_COLOR_NCA)
  })

  it("returns NCA color for manual NCA exclusion", {
    exclusions <- list(
      list(rows = 1, exclude_nca = TRUE, exclude_tlg = FALSE)
    )
    result <- .exclusion_row_color(1, base_data, exclusions)
    expect_equal(result$background, EXCL_COLOR_NCA)
  })

  it("returns TLG color for TLG-only exclusion", {
    exclusions <- list(
      list(rows = 1, exclude_nca = FALSE, exclude_tlg = TRUE)
    )
    result <- .exclusion_row_color(1, base_data, exclusions)
    expect_equal(result$background, EXCL_COLOR_TLG)
  })

  it("returns BOTH color for NCA + TLG exclusion", {
    exclusions <- list(
      list(rows = 1, exclude_nca = TRUE, exclude_tlg = TRUE)
    )
    result <- .exclusion_row_color(1, base_data, exclusions)
    expect_equal(result$background, EXCL_COLOR_BOTH)
  })

  it("returns BOTH color for default NCA + TLG exclusion", {
    exclusions <- list(
      list(rows = 2, exclude_nca = FALSE, exclude_tlg = TRUE)
    )
    # Row 2 has default NCA exclusion from data + TLG from manual
    result <- .exclusion_row_color(2, base_data, exclusions)
    expect_equal(result$background, EXCL_COLOR_BOTH)
  })
})

describe(".exclusion_type_label", {
  it("returns NCA for NCA-only", {
    expect_equal(.exclusion_type_label(TRUE, FALSE), "NCA")
  })

  it("returns TLG for TLG-only", {
    expect_equal(.exclusion_type_label(FALSE, TRUE), "TLG")
  })

  it("returns NCA + TLG for both", {
    expect_equal(.exclusion_type_label(TRUE, TRUE), "NCA + TLG")
  })

  it("returns empty string for neither", {
    expect_equal(.exclusion_type_label(FALSE, FALSE), "")
  })
})
