adpc <- read.csv(system.file("shiny/data/Dummy_complex_data.csv", package = "aNCA"))
attr(adpc$USUBJID, "label") <- "Subject ID"
attr(adpc$DOSEU, "label") <- "Dose unit"
adpc_single <- dplyr::filter(adpc, USUBJID == "XX01-11101")

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

describe("pkcg01", {
  it("generates valid plot list with default settings", {
    p_list <- pkcg01(adpc)
    .expect_plotlist(p_list, 92)
  })

  it("generates plot with custom labels", {
    p_list <- pkcg01(
      adpc_single,
      "xlab" = "Test custom xlab",
      "ylab" = "Test custom ylab",
      "title" = "Test custom title",
      "subtitle" = "Test custom subtitle",
      "footnote" = "Test custom footnote"
    )

    .expect_plotlist(p_list, 4)

    p_json <- .get_plotly_json(p_list[[1]])
    expect_equal(p_json$layout$xaxis$title$text, "Test custom xlab")
    expect_equal(p_json$layout$yaxis$title$text, "Test custom ylab")
    expect_equal(p_json$layout$title$text, "Test custom title<br><sup>Test custom subtitle</sup>")
    expect_equal(p_json$layout$annotations[1, ]$text, "Test custom footnote")
  })

  it("generates plot with custom labels that include special syntax", {
    p_list <- pkcg01(
      adpc_single,
      "xlab" = "Test $USUBJID",
      "ylab" = "Test !USUBJID: $USUBJID",
      "title" = "Test !DOSEU: $DOSEU",
      "subtitle" = "Test $DOSEU\n$DOSEU",
      "footnote" = "Test !USUBJID"
    )

    .expect_plotlist(p_list, 4)

    p_json <- .get_plotly_json(p_list[[1]])
    expect_equal(p_json$layout$xaxis$title$text, "Test XX01-11101")
    expect_equal(p_json$layout$yaxis$title$text, "Test Subject ID: XX01-11101")
    expect_equal(p_json$layout$title$text, "Test Dose unit: mg<br><sup>Test mg<br>mg</sup>")
    expect_equal(p_json$layout$annotations[1, ]$text, "Test Subject ID")
  })

  it("generates plot list in respect to grouping variables", {
    p_list <- pkcg01(adpc, plotgroup_vars = c("ROUTE", "PCSPEC", "PARAM"))
    .expect_plotlist(p_list, 4)
  })

  it("generates plots in respect to provided limits", {
    p_list <- pkcg01(
      adpc_single,
      xmin = 10,
      xmax = 150,
      ymin = 1,
      ymax = 3
    )

    .expect_plotlist(p_list, 4)

    p_json <- .get_plotly_json(p_list[[1]])

    #' NOTE:
    #' Actual range expectations differ from values provided in function call since plotly
    #' adds some margin.
    expect_equal(p_json$layout$xaxis$range, c(3, 157))
    expect_equal(p_json$layout$yaxis$range, c(0.9, 3.1))
  })
  
  it("generates plots with color_var and color_var_label", {
    adpc <- adpc_single
    
    p_list <- pkcg01(
      adpc = adpc,
      color_var = "DOSNO",
      color_var_label = "Dose Number",
      xvar = "AFRLT",
      yvar = "AVAL",
      xlab = "Time (hours)",
      ylab = "Concentration (ng/mL)",
      title = "PK Concentration-Time Profile",
      subtitle = "By Treatment Group",
      footnote = "Generated for testing purposes"
    )

    expect_type(p_list, "list")
    expect_s3_class(p_list[[1]], "plotly")
    
    p_json <- .get_plotly_json(p_list[[1]])
    
    # Check that the color legend is present and labeled correctly
    expect_equal(p_json$data$legendgroup, as.character(1:5))
    expect_true(all(p_json$data$showlegend))
    expect_equal(p_json$layout$legend$title$text, "Dose Number")
  })

  # it("generates plots with side-by-side (SBS) scale", {
  #   p_list <- pkcg01(
  #     adpc_single,
  #     scale = "SBS",
  #     xlab = "Test SBS xlab",
  #     ylab = "Test SBS ylab",
  #     title = "Test SBS title",
  #     subtitle = "Test SBS subtitle",
  #     footnote = "Test SBS footnote"
  #   )
  #
  #   .expect_plotlist(p_list, 4)
  #
  #   p_json <- .get_plotly_json(p_list[[1]])
  #
  #   expect_equal(p_json$layout$title$text, "Test SBS title<br><sup>Test SBS subtitle</sup>")
  #   expect_equal(p_json$layout$annotations[1, ]$text, "Test SBS xlab")
  #   # Check for multiple x and y axes (facet_wrap)
  #   expect_true(!is.null(p_json$layout$xaxis2))
  #   expect_true(!is.null(p_json$layout$yaxis2))
  # })
})

describe("g_pkconc_ind_lin", {
  it("generates plot with linear scale", {
    p_list <- g_pkconc_ind_lin(adpc_single)
    p_json <- .get_plotly_json(p_list[[1]])
    expect_equal(p_json$layout$yaxis$type, "linear")
  })
})

describe("g_pkconc_ind_log", {
  it("generates plot with log scale", {
    p_list <- g_pkconc_ind_log(adpc_single)
    p_json <- .get_plotly_json(p_list[[1]])
    expect_equal(p_json$layout$yaxis$type, "log")
  })
})
