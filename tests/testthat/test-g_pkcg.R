adpc <- FIXTURE_CONC_DATA %>%
  filter(USUBJID %in% unique(USUBJID)[1:3])
attr(adpc$USUBJID, "label") <- "Subject ID"
attr(adpc$AVAL, "label") <- "Analysis value"
attr(adpc$DOSEU, "label") <- "Dose unit"

describe("pkcg02", {
  it("generates valid ggplots with LIN scale", {
    combined_plots_lin <- pkcg02(adpc, scale = "LIN") # plotly = FALSE
    expect_equal(length(combined_plots_lin), 2) #add plotly parameter 
    
    vdiffr::expect_doppelganger("combined_lin_plot1", combined_lin_plot1[[1]])
    vdiffr::expect_doppelganger("combined_lin_plot2", combined_lin_plot2[[2]])
  })
  #Confirms that the first element in the list is a valid Plotly object 
  # ensuring that the plot was generated using the Plotly library.
  it("generates plotly plots with LIN scale", {
    combined_plotlys_lin <- pkcg02(adpc, scale = "LIN")
    expect_equal(length(combined_plotlys_lin), 2)
    expect_true(inherits(combined_plotlys_lin[[1]], "plotly")) # plotly = TRUE
  })
  
  it("generates valid ggplots with LOG scale", {
    combined_plots_log <- pkcg02(adpc, scale = "LOG") #, plotly = FALSE
    expect_equal(length(combined_plots_log), 2)
    vdiffr::expect_doppelganger("combined_log_plot1", combined_plots_log[[1]])
    vdiffr::expect_doppelganger("combined_log_plot2", combined_plots_log[[2]])
  })
  
  it("generates plotly plots with LOG scale", {
    combined_plotlys_log <- pkcg02(adpc, scale = "LOG") #, plotly = TRUE
    expect_equal(length(combined_plotlys_log), 2)
    expect_true(inherits(combined_plotlys_log[[1]], "plotly"))
  })
  
  it("generates valid ggplots with SBS scale", {
    combined_plots_sbs <- pkcg02(adpc, scale = "SBS") # , plotly = FALSE
    expect_equal(length(combined_plots_sbs), 2)
    vdiffr::expect_doppelganger("sbs_plot1", combined_plots_sbs[[1]])
    vdiffr::expect_doppelganger("sbs_plot2", combined_plots_sbs[[2]])
  })
  
  it("generates plotly plots with SBS scale", {
    combined_plotlys_sbs <- pkcg02(adpc, scale = "SBS") # , plotly = TRUE
    expect_equal(length(combined_plotlys_sbs), 2)
    expect_true(inherits(combined_plotlys_sbs[[1]], "plotly"))
  })
  
  it("generates plots with custom labels for LIN scale", {
    combined_plots_lin <- pkcg02(
      adpc,
      scale = "LIN",
      xlab = "Custom X Label",
      ylab = "Custom Y Label",
      title = "Custom Title",
      subtitle = "Custom Subtitle",
      footnote = "Custom Footnote",
      plotly = FALSE
    ) 
    plot <- combined_plots_lin[[1]]
    
  
    expect_equal(plot$labels$x, "Custom X Label")
    expect_equal(plot$labels$y, "Custom Y Label")
    expect_equal(plot$labels$title, "Custom Title")
    expect_equal(plot$labels$subtitle, "Custom Subtitle")
    expect_equal(plot$labels$caption, "Custom Footnote")
  })
  
  it("generates plots with custom groups and legend for LIN scale", {
    adpc =  adpc%>%mutate(DOSNO=as.character(DOSNO))
    combined_plots_lin_colors <- pkcg02(
      adpc,
      scale = "LIN",
      xlab = "Custom X Label",
      ylab = "Custom Y Label",
      title = "Custom Title",
      subtitle = "Custom Subtitle",
      footnote = "Custom Footnote",
      #plotly = FALSE,
      color_var = "DOSNO",
      color = c("red","blue","green"))
      #"Nominal Dose")   #/!\ color is legend name not color choices
    
    
    combined_plots_lin_colors <- combined_plots_lin_colors[[2]]
    vdiffr::expect_equal(combined_plots_lin_colors$labels$color, "Nominal Dose")
    vdiffr::expect_doppelganger("combined_plots_lin_colors", combined_plots_lin_colors)
     
  })
  
  
  it("returns error if missing ggh4x package for SBS scale", {
    # Temporarily mock requireNamespace to simulate ggh4x not being available
    testthat::with_mocked_bindings(
      code = {
        expect_error(
          pkcg01(adpc, scale = "SBS", plotly = FALSE),
          "Side-by-side view requires `ggh4x` package, please install it with"
        )
      },
      `requireNamespace` = function(pkg, quietly = FALSE) {
        if (pkg == "ggh4x") return(FALSE) else TRUE
      },
      .package = "base"
    )
  })
  it("returns error if missing scales package for SBS scale", {
    # Temporarily mock requireNamespace to simulate ggh4x not being available
    testthat::with_mocked_bindings(
      code = {
        expect_error(
          pkcg02(adpc, scale = "SBS"), #plotly = FALSE
          "Side-by-side view requires `scales` package, please install it with"
        )
      },
      `requireNamespace` = function(pkg, quietly = FALSE) {
        if (pkg == "scales") return(FALSE) else TRUE
      },
      .package = "base"
    )
  })
})
  
  describe("g_pkcg02_lin", {
    it("generates plot with linear scale", {
      g_pkcg02_lin <- g_pkcg02_lin(adpc)[[1]] #, plotly = FALSE
      expect_equal(g_pkcg02_lin$labels$y, "Analysis value [mg/L]")
      vdiffr::expect_doppelganger("g_pkcg02_lin", g_pkcg02_lin)
    })
  })
  
  describe("g_pkcg02_log", {
    it("generates plot with log scale", {
      
      g_pkcg02_log <- g_pkcg02_log(adpc)[[1]] #, plotly = FALSE
      expect_equal(g_pkcg02_log$labels$y, "Analysis value [mg/L]")
      vdiffr::expect_doppelganger("g_pkcg02_log", g_pkcg02_log)
    })
  })
  
