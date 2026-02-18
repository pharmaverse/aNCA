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
    # Store the count to avoid redundant updates
    last_n_col <- reactiveVal(-1)
    last_n_mean <- reactiveVal(-1)
    
    observe({
      pknca_obj <- req(pknca_data())
      df_raw <- pknca_obj$conc$data 
      
      # 1. INDIVIDUAL PLOT LOGIC
      ind_in <- individual_inputs()
      req(ind_in$color_by)
      
      # Filter data exactly as the plot function does
      df_ind <- filter_by_list(df_raw, ind_in$filtering_list)
      
      # Calculate count based on the interaction of all color_by variables
      n_col <- nrow(unique(df_ind[ind_in$color_by]))
      
      if (isolate(last_n_ind()) != n_col) {
        updateCheckboxInput(session, "individual_sidebar-show_legend", value = (n_col <= 30))
        last_n_ind(n_ind)
      }
      
      # 2. MEAN PLOT LOGIC
      mean_in <- mean_inputs()
      req(mean_in$color_by)
      
      # Mean data uses the summarized processing logic
      df_mean <- filter_by_list(df_raw, mean_in$filtering_list)
      n_mean <- nrow(unique(df_mean[mean_in$color_by]))
      
      if (isolate(last_n_mean()) != n_mean) {
        updateCheckboxInput(session, "mean_sidebar-show_legend", value = (n_mean <= 30))
        last_n_mean(n_mean)
      }
    })

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

    individualplot <- reactive({
      req(pknca_data(), individual_inputs()$color_by)
      log_info("Rendering individual plots")

      exploration_individualplot(
        pknca_data = isolate(pknca_data()),
        color_by = individual_inputs()$color_by,
        facet_by = individual_inputs()$facet_by,
        filtering_list = individual_inputs()$filtering_list,
        show_dose = individual_inputs()$show_dose,
        ylog_scale = individual_inputs()$ylog_scale,
        show_legend = individual_inputs()$show_legend,
        threshold_value = individual_inputs()$threshold_value,
        labels_df = metadata_nca_variables,
        use_time_since_last_dose = individual_inputs()$use_time_since_last_dose,
        palette = individual_inputs()$palette
      )
    })

    # Render the individual plot in plotly
    output$individualplot <- renderPlotly({
      req(individualplot())
      ggplotly(individualplot(), tooltip = "tooltip_text")
    })

    meanplot <- reactive({
      req(pknca_data(), mean_inputs()$color_by)
      log_info("Computing meanplot ggplot object")

      exploration_meanplot(
        pknca_data = isolate(pknca_data()),
        color_by = mean_inputs()$color_by,
        facet_by = mean_inputs()$facet_by,
        filtering_list = mean_inputs()$filtering_list,
        show_dose = mean_inputs()$show_dose,
        palette = mean_inputs()$palette,
        sd_min = mean_inputs()$sd_min,
        sd_max = mean_inputs()$sd_max,
        ci = mean_inputs()$ci,
        ylog_scale = mean_inputs()$ylog_scale,
        show_legend = mean_inputs()$show_legend,
        threshold_value = mean_inputs()$threshold_value,
        labels_df = metadata_nca_variables,
        use_time_since_last_dose = mean_inputs()$use_time_since_last_dose
      )
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
      ggplotly(meanplot(), tooltip = "tooltip_text")
    })

    pk_dose_qc_plot_server("pk_dose_qc_plot", pknca_data, extra_group_vars)
  })
}
