# Source the descriptive statistics module to test its pure helper functions
local({
  library(shiny)
  shiny_dir <- system.file("shiny", package = "aNCA")
  source(
    file.path(shiny_dir, "modules", "tab_nca", "descriptive_statistics.R"),
    local = TRUE
  )
},
envir = parent.env(environment()))

describe("default_summary_groupby", {
  # Minimal res_nca() stand-in: only $data$conc$data$ROUTE is read by the helper
  make_res_nca <- function(route_values) {
    list(data = list(conc = list(data = data.frame(
      ROUTE = route_values,
      stringsAsFactors = FALSE
    ))))
  }

  it("defaults to group columns plus ATPTREF", {
    res_nca <- make_res_nca(rep("IV", 4))
    default <- default_summary_groupby(
      res_nca,
      group_cols = c("STUDYID", "PCSPEC"),
      classification_cols = c("ATPTREF", "DOSEA")
    )
    expect_equal(default, c("STUDYID", "PCSPEC", "ATPTREF"))
  })

  it("adds ROUTE by default when more than one distinct route is present", {
    res_nca <- make_res_nca(c("IV", "IV", "ORAL", "ORAL"))
    default <- default_summary_groupby(
      res_nca,
      group_cols = "STUDYID",
      classification_cols = c("ATPTREF", "ROUTE")
    )
    expect_true("ROUTE" %in% default)
    expect_equal(default, c("STUDYID", "ATPTREF", "ROUTE"))
  })

  it("does not add ROUTE by default when only one distinct route is present", {
    res_nca <- make_res_nca(rep("IV", 4))
    default <- default_summary_groupby(
      res_nca,
      group_cols = "STUDYID",
      classification_cols = c("ATPTREF", "ROUTE")
    )
    expect_false("ROUTE" %in% default)
  })

  it("ignores NA when counting distinct routes", {
    # A single real route plus NAs should not count as multiple routes
    res_nca <- make_res_nca(c("IV", NA, "IV", NA))
    default <- default_summary_groupby(
      res_nca,
      group_cols = "STUDYID",
      classification_cols = c("ATPTREF", "ROUTE")
    )
    expect_false("ROUTE" %in% default)
  })

  it("does not add ROUTE when it is not an available classification column", {
    # More than one route in the data, but ROUTE not offered as a column
    res_nca <- make_res_nca(c("IV", "ORAL"))
    default <- default_summary_groupby(
      res_nca,
      group_cols = "STUDYID",
      classification_cols = "ATPTREF"
    )
    expect_false("ROUTE" %in% default)
  })
})
