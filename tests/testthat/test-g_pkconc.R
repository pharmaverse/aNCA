adpc <- read.csv(system.file("shiny/data/DummyRO_ADNCA.csv", package = "aNCA"))
attr(adpc$USUBJID, "label") <- "Subject ID"
attr(adpc$DOSEU, "label") <- "Dose unit"
adpc_single <- dplyr::filter(adpc, USUBJID == "11101")

#' Converts plotly object to JSON
#' @param p plotly object
#' @returns a list with plotly object data
.get_plotly_json <- function(p) {
  jsonlite::fromJSON(plotly::plotly_json(p, FALSE))
}

#' Checks basic expectations for plot list:
#' - is a list
#' - length is as expected
#' - each element is a plotly object
#' @param p_list plot list
#' @param list_length expected length of the list
.expect_plotlist <- function(p_list, list_length) {
  expect_type(p_list, "list")
  expect_equal(length(p_list), list_length)
  purrr::walk(p_list, \(plot) expect_identical(class(plot), c("plotly", "htmlwidget")))
}

describe("pkcg02", {
  it("generates valid plot list with default settings", {
    p_list <- pkcg02(adpc)
    .expect_plotlist(p_list, 1)
  })


  it("generates plot with custom labels", {
    p_list <- pkcg02(
      adpc,
      "xlab" = "Test custom xlab",
      "ylab" = "Test custom ylab",
      "title" = "Test custom title",
      "subtitle" = "Test custom subtitle",
      "footnote" = "Test custom footnote"
    )

    .expect_plotlist(p_list, 1)
    p_json <- .get_plotly_json(p_list[[1]])
    expect_equal(p_json$layout$xaxis$title$text, "Test custom xlab")
    expect_equal(p_json$layout$yaxis$title$text, "Test custom ylab")
    expect_equal(p_json$layout$title$text, "Test custom title<br><sup>Test custom subtitle</sup>")
    expect_equal(p_json$layout$annotations[1, ]$text, "Test custom footnote")
  })

  it("generates plot with custom labels that include special syntax", {
    p_list <- pkcg02(
      adpc_single,
      "xlab" = "Test $USUBJID",
      "ylab" = "Test !USUBJID: $USUBJID",
      "title" = "Test !DOSEU: $DOSEU",
      "subtitle" = "Test $DOSEU\n$DOSEU",
      "footnote" = "Test !USUBJID"
    )

    .expect_plotlist(p_list, 1)
    p_json <- .get_plotly_json(p_list[[1]])
    expect_equal(p_json$layout$xaxis$title$text, "Test 11101")
    expect_equal(p_json$layout$yaxis$title$text, "Test Subject ID: 11101")
    expect_equal(p_json$layout$title$text, "Test Dose unit: mg<br><sup>Test mg<br>mg</sup>")
    expect_equal(p_json$layout$annotations[1, ]$text, "Test Subject ID")
  })

  it("generates plot list in respect to grouping variables", {
    p_list <- pkcg02(adpc, plotgroup_vars = c("ROUTE", "PCSPEC", "PARAM"))
    .expect_plotlist(p_list, 1)
  })

  it("generates plots in respect to provided limits", {
    p_list <- pkcg02(
      adpc,
      xmin = 10,
      xmax = 150,
      ymin = 1,
      ymax = 3
    )

    .expect_plotlist(p_list, 1)
    p_json <- .get_plotly_json(p_list[[1]])

    #' NOTE:
    #'     #' Actual range expectations differ from values provided in function call since plotly
    #' adds some margin.
    expect_equal(p_json$layout$xaxis$range, c(3, 157))
    expect_equal(p_json$layout$yaxis$range, c(0.9, 3.1))
  })
})

describe("g_pkconc_lin", {
  it("generates plot with linear scale", {
    p_list <- g_pkconc_lin(adpc)
    p_json <- .get_plotly_json(p_list[[1]])
    expect_equal(p_json$layout$yaxis$type, "linear")
  })
})

describe("g_pkconc_log", {
  it("generates plot with log scale", {
    p_list <- g_pkconc_log(adpc)
    p_json <- .get_plotly_json(p_list[[1]])
    expect_equal(p_json$layout$yaxis$type, "log")
  })
})
