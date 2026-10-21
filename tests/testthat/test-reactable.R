# Source the common reactable module to test the pure define_cols() helper.
local({
  library(shiny)
  shiny_dir <- system.file("shiny", package = "aNCA")
  source(
    file.path(shiny_dir, "modules", "common", "reactable.R"),
    local = TRUE
  )
},
envir = parent.env(environment()))

describe("define_cols", {
  labelled_df <- function() {
    df <- data.frame(GeoMean = 1.0, stringsAsFactors = FALSE)
    attr(df$GeoMean, "label") <- "Geometric Mean"
    df
  }

  it("uses the label as the header and the column name as tooltip when header_from_label = TRUE", {
    defs <- define_cols(labelled_df(), header_from_label = TRUE)
    header <- as.character(defs$GeoMean$header)
    expect_match(header, "Geometric Mean")       # visible header = label
    expect_match(header, 'title="GeoMean"')      # raw name demoted to tooltip
  })

  it("keeps the column name as the header by default (unchanged app behaviour)", {
    defs <- define_cols(labelled_df(), header_from_label = FALSE)
    header <- as.character(defs$GeoMean$header)
    expect_match(header, ">GeoMean<")            # visible header = column name
    expect_match(header, 'title="Geometric Mean"')  # label is the tooltip
  })

  it("falls back to the column name when a column has no label", {
    defs <- define_cols(data.frame(x = 1:3), header_from_label = TRUE)
    expect_equal(defs$x$name, "x")
  })

  it("returns NULL for NULL input", {
    expect_null(define_cols(NULL))
  })
})

describe("define_col_groups", {
  it("returns NULL for an ordinary (flat) table with no col_groups attribute", {
    expect_null(define_col_groups(data.frame(TRT01A = "A", n = 1L)))
  })

  it("builds one colGroup per level mapping to the stored leaf columns", {
    df <- data.frame(
      TRT01A = "A",
      "M_n" = 1L, "M_Mean" = 2, "F_n" = 3L, "F_Mean" = 4,
      check.names = FALSE
    )
    attr(df, "col_groups") <- list(
      M = c("M_n", "M_Mean"),
      F = c("F_n", "F_Mean")
    )
    groups <- define_col_groups(df)

    expect_length(groups, 2)
    expect_equal(vapply(groups, `[[`, character(1), "name"), c("M", "F"))
    # reactable::colGroup() stores `columns` as a list of scalars.
    expect_equal(unlist(groups[[1]]$columns), c("M_n", "M_Mean"))
    expect_equal(unlist(groups[[2]]$columns), c("F_n", "F_Mean"))
    # Row-key columns are never grouped.
    expect_false("TRT01A" %in% unlist(lapply(groups, `[[`, "columns")))
  })

  it("drops groups with no columns instead of emitting an invalid colGroup", {
    # reactable aborts on a colGroup with zero columns; define_col_groups must
    # skip such entries (regression for empty-string / degenerate group levels).
    df <- data.frame(TRT01A = "A", "M_n" = 1L, check.names = FALSE)
    attr(df, "col_groups") <- list(M = "M_n", Empty = character(0))
    groups <- define_col_groups(df)
    expect_length(groups, 1)
    expect_equal(groups[[1]]$name, "M")
  })

  it("returns NULL when every group is empty", {
    df <- data.frame(x = 1)
    attr(df, "col_groups") <- list(A = character(0))
    expect_null(define_col_groups(df))
  })
})
