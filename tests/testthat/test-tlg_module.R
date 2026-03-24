# Source the TLG module to test pure utility functions
source(
  file.path(
    system.file("shiny", package = "aNCA"),
    "modules", "tab_tlg", "tlg_module.R"
  ),
  local = TRUE
)

describe("filter_tlg_excluded", {
  it("removes rows where PKSUM1F is '1'", {
    df <- data.frame(
      x = 1:5,
      PKSUM1F = c(NA, "1", NA, "1", NA),
      stringsAsFactors = FALSE
    )
    result <- filter_tlg_excluded(df)
    expect_equal(nrow(result), 3)
    expect_equal(result$x, c(1, 3, 5))
  })

  it("returns all rows when PKSUM1F is absent", {
    df <- data.frame(x = 1:3)
    result <- filter_tlg_excluded(df)
    expect_equal(nrow(result), 3)
    expect_equal(result$x, 1:3)
  })

  it("returns all rows when PKSUM1F is all NA", {
    df <- data.frame(
      x = 1:3,
      PKSUM1F = rep(NA_character_, 3),
      stringsAsFactors = FALSE
    )
    result <- filter_tlg_excluded(df)
    expect_equal(nrow(result), 3)
  })

  it("returns empty data frame when all rows are excluded", {
    df <- data.frame(
      x = 1:2,
      PKSUM1F = c("1", "1"),
      stringsAsFactors = FALSE
    )
    result <- filter_tlg_excluded(df)
    expect_equal(nrow(result), 0)
  })
})
