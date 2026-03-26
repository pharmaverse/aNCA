# Source the shared exclusion utilities and the parameter exclusions module
source(
  file.path(
    system.file("shiny", package = "aNCA"),
    "functions", "exclusion-utils.R"
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
    result <- .build_exclusion_reasons(list())
    expect_equal(result$indices, integer(0))
  })

  it("returns correct indices and reasons for a single exclusion", {
    lst <- list(
      list(reason = "Outlier", rows = c(1L, 3L))
    )
    result <- .build_exclusion_reasons(lst)
    expect_equal(result$indices, c(1L, 3L))
    expect_equal(result$reasons, c("Outlier", "Outlier"))
  })

  it("concatenates reasons when multiple exclusions cover the same row", {
    lst <- list(
      list(reason = "Outlier", rows = c(1L, 2L)),
      list(reason = "Protocol deviation", rows = c(2L, 4L))
    )
    result <- .build_exclusion_reasons(lst)
    expect_equal(result$indices, c(1L, 2L, 4L))
    expect_equal(result$reasons[1], "Outlier")
    expect_equal(result$reasons[2], "Outlier; Protocol deviation")
    expect_equal(result$reasons[3], "Protocol deviation")
  })

  it("handles non-contiguous row indices", {
    lst <- list(
      list(reason = "Bad sample", rows = c(5L, 10L))
    )
    result <- .build_exclusion_reasons(lst)
    expect_equal(result$indices, c(5L, 10L))
    expect_equal(result$reasons, c("Bad sample", "Bad sample"))
  })
})

describe("rehydrate_exclusions", {
  it("rehydrates overrides with fresh button IDs", {
    excl_list <- shiny::reactiveVal(list())
    counter <- shiny::reactiveVal(0L)

    overrides <- list(
      list(reason = "Outlier", rows = c(1L, 3L)),
      list(reason = "Protocol deviation", rows = c(2L))
    )

    shiny::isolate({
      rehydrate_exclusions(overrides, excl_list, counter,
                           prefix = "remove_param_excl_")

      result <- excl_list()
      expect_length(result, 2)
      expect_equal(result[[1]]$reason, "Outlier")
      expect_equal(result[[1]]$rows, c(1L, 3L))
      expect_equal(result[[1]]$xbtn_id, "remove_param_excl_1")
      expect_equal(result[[2]]$reason, "Protocol deviation")
      expect_equal(result[[2]]$rows, 2L)
      expect_equal(result[[2]]$xbtn_id, "remove_param_excl_2")
      expect_equal(counter(), 2L)
    })
  })

  it("continues counter from existing value", {
    excl_list <- shiny::reactiveVal(list())
    counter <- shiny::reactiveVal(5L)

    overrides <- list(
      list(reason = "Reason A", rows = c(1L))
    )

    shiny::isolate({
      rehydrate_exclusions(overrides, excl_list, counter,
                           prefix = "remove_param_excl_")

      result <- excl_list()
      expect_equal(result[[1]]$xbtn_id, "remove_param_excl_6")
      expect_equal(counter(), 6L)
    })
  })

  it("works with different prefixes", {
    excl_list <- shiny::reactiveVal(list())
    counter <- shiny::reactiveVal(0L)

    overrides <- list(
      list(reason = "Test", rows = c(1L))
    )

    shiny::isolate({
      rehydrate_exclusions(overrides, excl_list, counter,
                           prefix = "remove_exclusion_reason_")

      result <- excl_list()
      expect_equal(result[[1]]$xbtn_id, "remove_exclusion_reason_1")
    })
  })
})

describe("clean_exclusion_list", {
  it("strips xbtn_id and preserves reason and rows", {
    raw_list <- list(
      list(reason = "Outlier", rows = c(1L, 3L), xbtn_id = "remove_param_excl_1"),
      list(reason = "PD", rows = c(2L), xbtn_id = "remove_param_excl_2")
    )
    clean <- clean_exclusion_list(raw_list)

    expect_length(clean, 2)
    expect_null(clean[[1]]$xbtn_id)
    expect_null(clean[[2]]$xbtn_id)
    expect_equal(clean[[1]]$reason, "Outlier")
    expect_equal(clean[[1]]$rows, c(1L, 3L))
    expect_equal(clean[[2]]$reason, "PD")
  })

  it("returns empty list for empty input", {
    expect_equal(clean_exclusion_list(list()), list())
  })
})

describe("exclusion list round-trip", {
  it("clean → rehydrate → build_reasons preserves data", {
    raw_list <- list(
      list(reason = "Outlier", rows = c(1L, 3L), xbtn_id = "remove_param_excl_1"),
      list(reason = "PD", rows = c(2L), xbtn_id = "remove_param_excl_2")
    )
    clean <- clean_exclusion_list(raw_list)

    excl_list <- shiny::reactiveVal(list())
    counter <- shiny::reactiveVal(0L)

    shiny::isolate({
      rehydrate_exclusions(clean, excl_list, counter,
                           prefix = "remove_param_excl_")
      result <- excl_list()
      expect_length(result, 2)
      expect_equal(result[[1]]$reason, "Outlier")
      expect_equal(result[[1]]$rows, c(1L, 3L))
      expect_true(!is.null(result[[1]]$xbtn_id))

      info <- .build_exclusion_reasons(result)
      expect_equal(info$indices, c(1L, 2L, 3L))
      expect_equal(info$reasons[1], "Outlier")
      expect_equal(info$reasons[2], "PD")
      expect_equal(info$reasons[3], "Outlier")
    })
  })
})
