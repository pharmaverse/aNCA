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
