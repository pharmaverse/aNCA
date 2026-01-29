# The Exploration Navbar tab loads the data from the Data tab, and results from NCA tab
# The user can then explore the data using various visualisation tools

# EXPLORATION ----
tab_explore_ui <- function(id) {
  ns <- NS(id)

  navset_card_pill(
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

tab_explore_server <- function(id, pknca_data, extra_group_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initiate the sidebar server modules
    individual_inputs <- plot_sidebar_server(
      "individual_sidebar",
      pknca_data = pknca_data,
      grouping_vars = extra_group_vars
    )

    mean_inputs <- plot_sidebar_server(
      "mean_sidebar",
      pknca_data = pknca_data,
      grouping_vars = extra_group_vars
    )

    # TAB: General Lineplot --------------------------------------------------------
    master_palettes_list <- reactive({
      req(individual_inputs()$palette_theme)
      req(individual_inputs()$color_by)

      get_persistent_palette(
        pknca_data()$conc$data,
        individual_inputs()$color_by,
        palette_name = individual_inputs()$palette_theme
      )
    })

    # Compute the individual plot object
    individualplot <- reactive({
      req(pknca_data(), individual_inputs()$color_by)
      log_info("Rendering individual plots")
browser()
      individualplot <- exploration_individualplot(
        pknca_data = pknca_data(),
        color_by = individual_inputs()$color_by,
        facet_by = individual_inputs()$facet_by,
        filtering_list = individual_inputs()$filtering_list,
        show_dose = individual_inputs()$show_dose,
        ylog_scale = individual_inputs()$ylog_scale,
        threshold_value = individual_inputs()$threshold_value,
        labels_df = metadata_nca_variables,
        use_time_since_last_dose = individual_inputs()$use_time_since_last_dose,
        palette = master_palettes_list()
      )

      #lineplot
      individualplot
    })

    # Render the individual plot in plotly
    output$individualplot <- renderPlotly({
      req(individualplot())
      ggplotly(individualplot(), tooltip = "text")
    })

    # TAB: Mean Plot -----------------------------------------------------------

    mean_palettes_list <- reactive({
      req(mean_inputs()$palette_theme)
      req(mean_inputs()$color_by)

      get_persistent_palette(
        pknca_data()$conc$data,
        mean_inputs()$color_by,
        palette_name = mean_inputs()$palette_theme
      )
    })

    # Compute the meanplot object
    meanplot <- reactive({
      req(pknca_data(), mean_inputs()$color_by)
      log_info("Computing meanplot ggplot object")

      meanplot <- exploration_meanplot(
        pknca_data = pknca_data(),
        color_by = mean_inputs()$color_by,
        facet_by = mean_inputs()$facet_by,
        filtering_list = mean_inputs()$filtering_list,
        show_dose = mean_inputs()$show_dose,
        palette = mean_palettes_list(),
        sd_min = mean_inputs()$sd_min,
        sd_max = mean_inputs()$sd_max,
        ci = mean_inputs()$ci,
        ylog_scale = mean_inputs()$ylog_scale,
        threshold_value = mean_inputs()$threshold_value,
        labels_df = metadata_nca_variables,
        use_time_since_last_dose = mean_inputs()$use_time_since_last_dose
      )
      meanplot
    })

    # Save the objects for the ZIP folder whenever they change
    observe({
      req(individualplot(), meanplot())
      session$userData$results$exploration$individualplot <- individualplot()
      session$userData$results$exploration$meanplot <- meanplot()
    })

    # Render the mean plot output in plotly
    output$mean_plot <- renderPlotly({
      req(meanplot())
      ggplotly(meanplot(), tooltip = "text")
    })

    pk_dose_qc_plot_server("pk_dose_qc_plot", pknca_data, extra_group_vars)
  })
}
