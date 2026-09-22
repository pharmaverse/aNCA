# Tests for the "Add TLGs to order" modal helpers (issue #1335).
local({
  library(shiny)
  shiny_dir <- system.file("shiny", package = "aNCA")
  source(file.path(shiny_dir, "functions", "tlg_add_picker.R"), local = TRUE)
},
envir = parent.env(environment()))

avail_fixture <- function() {
  dplyr::tibble(
    id          = c(2L, 5L, 9L),
    Type        = c("Table", "Listing", "Graph"),
    Dataset     = c("PK Concentrations", "PK Parameters", "PK Concentrations"),
    PKid        = c("pkct01", "pkcl01", "pkpg01"),
    Description = c("Conc summary table", "Listing of parameters", "Cumulative plot"),
    Link        = c("http://spec/1", NA_character_, "http://spec/3")
  )
}

describe("checked_tlg_ids", {
  it("returns integer ids from the checked group inputs", {
    input <- list(g1 = c("2", "9"), g2 = character(0))
    expect_identical(checked_tlg_ids(input, c("g1", "g2")), c(2L, 9L))
  })

  it("returns an empty integer vector when nothing is checked", {
    expect_identical(checked_tlg_ids(list(g1 = NULL), "g1"), integer(0))
  })
})

describe("tlg_modal_dl_data", {
  it("selects the catalog columns", {
    out <- tlg_modal_dl_data(avail_fixture())
    expect_equal(names(out), c("Type", "Dataset", "PKid", "Description"))
    expect_equal(nrow(out), 3)
  })

  it("returns an empty typed frame for NULL or empty input", {
    for (df in list(NULL, avail_fixture()[0, ])) {
      empty <- tlg_modal_dl_data(df)
      expect_equal(names(empty), c("Type", "Dataset", "PKid", "Description"))
      expect_equal(nrow(empty), 0)
    }
  })
})

describe("build_add_checklist", {
  it("creates one checkbox group per Type/Dataset pair and returns their ids", {
    res <- build_add_checklist(avail_fixture(), ns = identity)

    # 3 distinct Type/Dataset pairs -> 3 checkbox groups
    expect_length(res$group_ids, 3)
    expect_true(all(grepl("^modal_check_", res$group_ids)))

    html <- as.character(res$ui)
    # every output description is rendered
    expect_true(grepl("Conc summary table", html))
    expect_true(grepl("Listing of parameters", html))
    expect_true(grepl("Cumulative plot", html))
    # dataset tab bar + catalog checklist scaffolding present
    expect_true(grepl("tlg-tabs", html))
    expect_true(grepl("tlg-add-checklist", html))
    # spec link only where Link is non-NA
    expect_true(grepl("http://spec/1", html, fixed = TRUE))
    expect_true(grepl("http://spec/3", html, fixed = TRUE))
  })
})
