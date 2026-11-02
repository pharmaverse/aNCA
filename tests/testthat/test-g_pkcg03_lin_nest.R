describe("g_pkcg03_lin_nest", {
  it("preserves means and SD error bars with BLQ eligibility and exclusions", {
    d <- data.frame(USUBJID = rep(c("1", "2", "3"), 3),
                    NFRLT = rep(c(0, 1, 2), each = 3), AVAL = c(1, 3, 5, 0, 0, 9, 2, 4, 6),
                    AVALC = c("BLQ", "3", "5", "BLQ", "BLQ", "9", "2", "4", "6"),
                    DOSEA = 10, ROUTE = "IV", PCSPEC = "SERUM", PARAM = "Drug", STUDYID = "S1",
                    RRLTU = "h", AVALU = "mg/L", PKSUMXF = "")
    d$PKSUMXF[9] <- "Y"
    plot <- g_pkcg03_lin_nest(d, plotly = FALSE)[[1]]
    legacy <- g_pkcg03_lin(d, plotly = FALSE)[[1]]
    expect_equal(plot$data, legacy$data)
    expect_equal(as.numeric(plot$data$NFRLT), c(0, 2))
    expect_equal(plot$data$mean, c(3, 3))
    expect_equal(plot$data$sd, c(2, sqrt(2)))
    layers <- ggplot2::ggplot_build(plot)$data
    expect_equal(layers[[3]]$ymin, c(1, 3 - sqrt(2)))
    expect_equal(layers[[3]]$ymax, c(5, 3 + sqrt(2)))
    expect_match(plot$labels$y, "mg/L", fixed = TRUE)
    expect_match(plot$labels$caption, "<=50% BLQ", fixed = TRUE)
    custom <- g_pkcg03_lin_nest(d, plotly = FALSE, footnote = "Study $STUDYID")[[1]]
    expect_match(custom$labels$caption, "Study S1", fixed = TRUE)
    expect_match(custom$labels$caption, "no new imputation", fixed = TRUE)
    expect_match(g_pkcg03_lin_nest(d[1, ])[[1]], "No timepoints")
    d$PKSUMXF <- "Y"
    expect_match(g_pkcg03_lin_nest(d)[[1]], "No timepoints")
  })
})

describe("NEST graph edge cases", {
  it("resolves annotations per split and explains all-missing concentrations", {
    d <- data.frame(USUBJID = rep(c("1", "2"), 2), NFRLT = 0,
                    AVAL = c(1, 3, 2, 6), DOSEA = 10, ROUTE = "IV", PCSPEC = "SERUM",
                    PARAM = rep(c("Drug", "Other"), each = 2), RRLTU = "h", AVALU = "mg/L")
    plots <- g_pkcg03_lin_nest(d, plotly = FALSE, title = "$PARAM", footnote = "$PARAM")
    expect_match(plots[[1]]$labels$title, "Drug")
    expect_match(plots[[2]]$labels$title, "Other")
    expect_match(plots[[2]]$labels$caption, "Other")
    expect_match(plots[[1]]$labels$caption, "no BLQ flag, so no BLQ rule", fixed = TRUE)
    expect_false(grepl("<=50% BLQ", plots[[1]]$labels$caption, fixed = TRUE))
    widget <- g_pkcg03_lin_nest(d)[[1]]
    expect_equal(attr(widget, "ggplot")$data, plots[[1]]$data)
    d$AVAL <- NA_real_
    expect_match(g_pkcg03_lin_nest(d, plotly = FALSE)[[1]], "No nonmissing")
    expect_match(g_pkcg03_lin_nest(d[0, ])[[1]], "No data")
  })
})

describe("NEST graph annotation layout", {
  it("reserves space below the axis and legend for methods in interactive exports", {
    d <- data.frame(USUBJID = c("1", "2"), NFRLT = 0, AVAL = c(1, 3), DOSEA = 10,
                    ROUTE = "IV", PCSPEC = "SERUM", PARAM = "Drug", RRLTU = "h", AVALU = "mg/L")
    widget <- g_pkcg03_lin_nest(d)[[1]]
    layout <- plotly::plotly_build(widget)$x$layout
    expect_gte(layout$margin$b, 162)
    expect_equal(layout$annotations[[1]]$yshift, -85)
    expect_equal(layout$annotations[[1]]$yanchor, "top")
    expect_match(layout$annotations[[1]]$text, "<br>", fixed = TRUE)
    expect_equal(attr(widget, "ggplot")$data$mean, 2)
    expect_equal(attr(widget, "ggplot")$data$sd, sqrt(2))
  })
})
