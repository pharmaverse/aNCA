# The Exploration Navbar tab loads the data from the Data tab, and results from NCA tab
# The user can then explore the data using various visualisation tools

# EXPLORATION ----
tab_explore_ui <- function(id) {
  ns <- NS(id)

  navset_card_pill(
    header = "Exploratory Analysis",
    id = "visuals",
    nav_panel("Individual Plots",
              layout_sidebar(
                sidebar = plot_sidebar_ui(ns("individual_sidebar"), is_mean_plot = FALSE),
                plotlyOutput(ns("individualplot"), height = "100%")
              )
    ),
    nav_panel("Mean Plots",
              layout_sidebar(
                sidebar = plot_sidebar_ui(ns("mean_sidebar"), is_mean_plot = TRUE),
                plotlyOutput(ns("mean_plot"), height = "100%")
              )
    ),
    nav_panel(
      "PK/Dose QC Plot",
      pk_dose_qc_plot_ui(ns("pk_dose_qc_plot"))
    )
  )
}

# SERVER LOGIC OF NAVBAR OUTPUT TAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# In the output tab of the navbar, dynamic graphics of the input
# as well as the results of the NCA analysis are displayed. The user can dynamically
# display graphics and summaries of these data.

tab_explore_server <- function(id, data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initiate the sidebar server modules
    individual_sidebar_inputs <- plot_sidebar_server(
      "individual_sidebar",
      data = data,
      grouping_vars = grouping_vars
      )
    mean_sidebar_inputs <- plot_sidebar_server(
      "mean_sidebar",
      data = data,
      grouping_vars = grouping_vars)

    # TAB: General Lineplot --------------------------------------------------------

    master_palettes_list <- reactive({
      req(individual_sidebar_inputs$palette_theme())
      req(individual_sidebar_inputs$colorby())

      get_persistent_palette(
        data(),
        individual_sidebar_inputs$colorby(),
        palette_name = individual_sidebar_inputs$palette_theme()
      )
    })

    # Compute the individual plot object
    individualplot <- reactive({
      inputs <- individual_sidebar_inputs()
      req(data(), inputs$param, inputs$pcspec, inputs$usubjid, inputs$colorby, inputs$timescale, inputs$log)
      if (inputs$timescale == "By Dose Profile") req(inputs$cycles)
      log_info("Rendering individual plots")
      
      processed_data <- process_data_individual(
        data(),
        selected_usubjids = inputs$usubjid,
        selected_analytes = inputs$param,
        selected_pcspec = inputs$pcspec,
        colorby_var = inputs$colorby,
        time_scale = inputs$timescale,
        yaxis_scale = inputs$log,
        cycle = inputs$cycles
      )
      
      validate(need(nrow(processed_data) > 0, "No data available for the selected filters."))
      
      p <- g_lineplot(
        data = processed_data,
        x_var = "time_var",
        y_var = "AVAL",
        group_var = "USUBJID",
        colorby_var = inputs$colorby,
        facet_by = inputs$facetby,
        yaxis_scale = inputs$log,
        show_threshold = inputs$show_threshold,
        threshold_value = inputs$threshold_value,
        show_dose = inputs$show_dose,
        dose_data = data() %>% mutate(TIME_DOSE = round(AFRLT - ARRLT, 6)),
        palette = master_palettes_list()
      ) %>%
        ggplotly(height = 1000)
      
      if (is.null(inputs$facetby) || length(inputs$facetby) == 0) {
        p <- p %>% layout(xaxis = list(rangeslider = list(type = "time")))
      }
      p
    })

    # Save the object for the zip folder whenever it changes
    observe({
      req(individualplot())
      session$userData$results$exploration$individualplot <- individualplot()
    })

    # Render the inidividual plot in plotly
    output$individualplot <- renderPlotly(individualplot())


    # TAB: Mean Plot -----------------------------------------------------------

    # Compute the meanplot object
    meanplot <- reactive({
      inputs <- mean_sidebar_inputs()
      req(data(), inputs$param, inputs$pcspec, inputs$timescale, inputs$colorby)
      if (inputs$timescale == "By Dose Profile") req(inputs$cycles)
      log_info("Computing meanplot ggplot object")
      
      processed_data <- process_data_mean(
        data(),
        selected_analytes = inputs$param,
        selected_pcspec = inputs$pcspec,
        cycle = inputs$cycles,
        colorby_var = inputs$colorby,
        yaxis_scale = inputs$log,
        time_scale = inputs$timescale
      )
      
      validate(need(nrow(processed_data) > 0, "No data with >= 3 points to calculate mean."))
      
      p <- g_lineplot(
        data = processed_data,
        x_var = "time_var",
        y_var = "Mean",
        group_var = "color_var",
        colorby_var = inputs$colorby,
        yaxis_scale = inputs$log,
        show_sd_min = inputs$sd_min,
        show_sd_max = inputs$sd_max,
        show_ci = inputs$ci,
        facet_by = inputs$facetby,
        show_threshold = inputs$show_threshold,
        threshold_value = inputs$threshold_value,
        show_dose = inputs$show_dose
      ) %>%
        ggplotly(height = 1000) %>%
        layout(xaxis = list(rangeslider = list(type = "time")))
      p
    })

    # Save the object for the zip folder whenever it changes
    observe({
      req(meanplot())
      session$userData$results$exploration$meanplot <- meanplot()
    })

    # Render the mean plot output in plotly
    output$mean_plot <- renderPlotly({
      req(meanplot())
      meanplot()
    })

    pk_dose_qc_plot_server("pk_dose_qc_plot", data = data, grouping_vars = grouping_vars)

  })
}
