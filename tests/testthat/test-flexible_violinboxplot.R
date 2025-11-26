# Create a sample boxplotdata
boxplotdata <- FIXTURE_PKNCA_RES %>%
  filter(USUBJID %in% 1:7)

describe("flexible_violinboxplot", {
  it("creates a simple plot with minimal arguments", {
    simple_plot <- flexible_violinboxplot(
      res_nca = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      tooltip_vars = c("DOSEA", "USUBJID", "ATPTREF", "PARAM"),
      box = TRUE,
      plotly = FALSE
    )

    expect_equal(simple_plot$labels$x, "DOSEA")
    expect_equal(simple_plot$labels$colour, "ATPTREF")
    expect_true(grepl("CMAX", simple_plot$labels$y))
    expect_true(any("ggplot" %in% class(simple_plot)))
    expect_equal(c(1, 2, 6), unique(simple_plot$data$USUBJID))
  })

  it("creates a plot with additional xvars", {
    xvars_plot <- flexible_violinboxplot(
      res_nca = boxplotdata,
      parameter = "CMAX",
      xvars = c("DOSEA", "PARAM"),
      colorvars = "ATPTREF",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      tooltip_vars = c("DOSEA", "USUBJID", "ATPTREF", "PARAM"),
      box = TRUE,
      plotly = FALSE
    )

    expect_equal(xvars_plot$labels$x, "DOSEA, PARAM")
    expect_equal(xvars_plot$labels$colour, "ATPTREF")
    expect_true(grepl("CMAX", xvars_plot$labels$y))
    expect_equal(c(1, 2, 6), unique(xvars_plot$data$USUBJID))
    expect_true(any("ggplot" %in% class(xvars_plot)))
  })

  it("creates a plot with additional colorvars", {
    colorvars_plot <- flexible_violinboxplot(
      res_nca = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = c("ATPTREF", "PARAM"),
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      tooltip_vars = c("DOSEA", "USUBJID", "ATPTREF", "PARAM"),
      box = TRUE,
      plotly = FALSE
    )

    expect_equal(colorvars_plot$labels$x, "DOSEA")
    expect_equal(colorvars_plot$labels$colour, "ATPTREF, PARAM")
    expect_true(grepl("CMAX", colorvars_plot$labels$y))
    expect_equal(c(1, 2, 6), unique(colorvars_plot$data$USUBJID))
    expect_true(any("ggplot" %in% class(colorvars_plot)))
  })

  it("creates a plot with additional varvalstofilter if specified", {
    varvalstofilter_plot <- flexible_violinboxplot(
      res_nca = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6", "ATPTREF: 1"),
      tooltip_vars = c("DOSEA", "USUBJID", "ATPTREF", "PARAM"),
      box = TRUE,
      plotly = FALSE
    )

    expect_equal(varvalstofilter_plot$labels$x, "DOSEA")
    expect_equal(varvalstofilter_plot$labels$colour, "ATPTREF")
    expect_true(grepl("CMAX", varvalstofilter_plot$labels$y))
    expect_equal(c(1, 2, 6), unique(varvalstofilter_plot$data$USUBJID))
    expect_equal(1, unique(varvalstofilter_plot$data$ATPTREF))
    expect_true(any("ggplot" %in% class(varvalstofilter_plot)))
  })

  it("creates a plot with all variables if varvalstofilter is unspecified", {
    all_data_plot <- flexible_violinboxplot(
      res_nca = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      tooltip_vars = c("DOSEA", "USUBJID", "ATPTREF", "PARAM"),
      box = TRUE,
      plotly = FALSE
    )
    expect_equal(all_data_plot$labels$x, "DOSEA")
    expect_equal(all_data_plot$labels$colour, "ATPTREF")
    expect_true(grepl("CMAX", all_data_plot$labels$y))
    expect_equal(c(1, 2, 3, 4, 5, 6, 7), unique(all_data_plot$data$USUBJID))
    expect_true(any("ggplot" %in% class(all_data_plot)))
  })

  it("creates a violin plot when box = FALSE", {
    violin_plot <- flexible_violinboxplot(
      res_nca = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 3"),
      tooltip_vars = c("DOSEA", "USUBJID", "ATPTREF", "PARAM"),
      box = FALSE,
      plotly = FALSE
    )

    expect_equal(violin_plot$labels$x, "DOSEA")
    expect_equal(violin_plot$labels$colour, "ATPTREF")
    expect_true(grepl("CMAX", violin_plot$labels$y))
    expect_equal(c(1, 2, 3), unique(violin_plot$data$USUBJID))
    expect_true(any("ggplot" %in% class(violin_plot)))
  })

  it("handles missing data gracefully", {
    boxplotdata_missing <- boxplotdata %>%
      mutate(PPSTRES = NA)
    missing_plot <- flexible_violinboxplot(
      res_nca = boxplotdata_missing,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      tooltip_vars = c("DOSEA", "USUBJID", "ATPTREF", "PARAM"),
      box = TRUE,
      plotly = FALSE
    )

    expect_s3_class(missing_plot, "ggplot")
    expect_true(length(missing_plot$layers) == 0)
  })

  it("handles axis labels correctly when parameter has no unit", {
    plot_with_param_unit <- flexible_violinboxplot(
      res_nca = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      tooltip_vars = c("DOSEA", "USUBJID", "ATPTREF", "PARAM"),
      box = TRUE,
      plotly = TRUE
    )
    expect_true(grepl("\\[ ng/mL", plot_with_param_unit$x$layout$yaxis$title$text))

    plot_wo_param_unit <- flexible_violinboxplot(
      res_nca = boxplotdata %>% mutate(PPSTRESU = ""),
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      tooltip_vars = c("DOSEA", "USUBJID", "ATPTREF", "PARAM"),
      box = TRUE,
      plotly = TRUE
    )
    expect_false(grepl("\\[", plot_wo_param_unit$x$layout$yaxis$title$text))
  })

  it("creates a plotly object correctly", {
    simple_plotly <- flexible_violinboxplot(
      res_nca = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      varvalstofilter = c("USUBJID: 1", "USUBJID: 2", "USUBJID: 6"),
      tooltip_vars = c("DOSEA", "USUBJID", "ATPTREF", "PARAM"),
      box = TRUE,
      plotly = TRUE
    )
    expect_s3_class(simple_plotly, "plotly")
  })
  
  it("creates a violin plotly object correctly", {
    simple_plotly <- flexible_violinboxplot(
      res_nca = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      varvalstofilter = NULL,
      tooltip_vars = c("DOSEA", "USUBJID", "ATPTREF", "PARAM"),
      box = FALSE,
      plotly = TRUE
    )
    expect_s3_class(simple_plotly, "plotly")
  })
})

describe("flexible_violinboxplot: Tooltips & Aesthetics", {
  
  it("generates correct HTML tooltips using labels_df", {
    p <- flexible_violinboxplot(
      res_nca = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      varvalstofilter = c("USUBJID: 1"),
      tooltip_vars = c("USUBJID", "DOSEA"),
      box = TRUE,
      plotly = FALSE,
      labels_df = metadata_nca_variables
    )
    
    # Check tooltip_text column exists
    expect_true("tooltip_text" %in% names(p$data))
    
    # Check bold formatting and label lookup
    # USUBJID -> "Unique Subject Identifier"
    expect_true(any(grepl("<b>Unique Subject Identifier</b>", p$data$tooltip_text)))
  })
  
  it("rounds numeric variables in tooltips", {
    # Modify data to have a long decimal
    res_decimal <- boxplotdata
    res_decimal$result$DOSEA <- 100.123456
    
    p <- flexible_violinboxplot(
      res_nca = res_decimal,
      parameter = "CMAX",
      xvars = "ATPTREF",
      colorvars = "ATPTREF",
      varvalstofilter = c("USUBJID: 1"),
      tooltip_vars = c("DOSEA"),
      box = TRUE,
      plotly = FALSE,
      labels_df = metadata_nca_variables
    )
    print(p$data$tooltip_text)
    # Check that tooltip text contains rounded value (100.12)
    expect_true(any(grepl("100.12", p$data$tooltip_text)))
    # Ensure raw long decimal isn't there
    expect_false(any(grepl("100.123456", p$data$tooltip_text)))
  })
  
  it("handles missing labels_df by falling back to simple text", {
    p <- flexible_violinboxplot(
      res_nca = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      varvalstofilter = c("USUBJID: 1"),
      tooltip_vars = c("USUBJID", "DOSEA"),
      box = TRUE,
      plotly = FALSE,
      labels_df = NULL # NO LABELS
    )
    
    # Expect simple "Var: Value" without bold tags
    expect_true(any(grepl("USUBJID: 1", p$data$tooltip_text)))
    expect_false(any(grepl("<b>", p$data$tooltip_text)))
  })
  
  it("prevents text aesthetic inheritance in geom_boxplot", {
    p <- flexible_violinboxplot(
      res_nca = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      tooltip_vars = c("USUBJID"),
      box = TRUE,
      plotly = FALSE
    )
    
    # Find Boxplot Layer
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    idx <- which(layer_classes == "GeomBoxplot")
    expect_true(length(idx) > 0)
    
    boxplot_layer <- p$layers[[idx]]
    
    # =Verify inherit.aes is FALSE
    expect_false(boxplot_layer$inherit.aes)
    
    # Verify mapping contains x, y, color but NOT text
    expect_true(!is.null(boxplot_layer$mapping$x))
    expect_true(!is.null(boxplot_layer$mapping$y))
    expect_true(!is.null(boxplot_layer$mapping$colour))
    expect_null(boxplot_layer$mapping$text)
  })
  
  it("prevents text aesthetic inheritance in geom_violin", {
    p <- flexible_violinboxplot(
      res_nca = boxplotdata,
      parameter = "CMAX",
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      tooltip_vars = c("USUBJID"),
      box = FALSE, # Violin
      plotly = FALSE
    )
    
    # Find Violin Layer
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    idx <- which(layer_classes == "GeomViolin")
    expect_true(length(idx) > 0)
    
    violin_layer <- p$layers[[idx]]
    
    # Verify inherit.aes is FALSE and no text mapping
    expect_false(violin_layer$inherit.aes)
    expect_null(violin_layer$mapping$text)
  })
  
  it("handles aucint parameter mutation logic", {
    # Test specific PPTESTCD mutation logic: startsWith(PPTESTCD, "aucint")
    res_aucint <- boxplotdata
    res_aucint$result$PPTESTCD <- "aucint.all"
    res_aucint$result$start <- 0
    res_aucint$result$end <- 24
    
    # Filter must match the mutated name: "aucint.all_0-24"
    p <- flexible_violinboxplot(
      res_nca = res_aucint,
      parameter = "aucint.all_0-24", 
      xvars = "DOSEA",
      colorvars = "ATPTREF",
      tooltip_vars = c("USUBJID"),
      box = TRUE,
      plotly = FALSE
    )
    
    # If the plot has data, it means mutation and filtering worked
    expect_true(nrow(p$data) > 0)
    expect_equal(unique(p$data$PPTESTCD), "aucint.all_0-24")
  })
})