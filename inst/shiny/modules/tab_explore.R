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

    data <- reactive({
      req(pknca_data())
      pknca_data()$conc$data
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


    # TAB: General Lineplot --------------------------------------------------------

    master_palettes_list <- reactive({
      req(individual_inputs()$palette_theme)
      req(individual_inputs()$color_by)

      get_persistent_palette(
        data(),
        individual_inputs()$color_by,
        palette_name = individual_inputs()$palette_theme
      )
    })

    # Compute the individual plot object
    individualplot <- reactive({
      req(data(), individual_inputs()$param,
          individual_inputs()$pcspec,
          individual_inputs()$usubjid,
          individual_inputs()$color_by)
      log_info("Rendering individual plots")

      individual_output <- process_data_individual(
        data = data(),
        selected_usubjids = individual_inputs()$usubjid,
        selected_analytes = individual_inputs()$param,
        selected_pcspec = individual_inputs()$pcspec,
        profiles_selected = individual_inputs()$profiles,
        ylog_scale = individual_inputs()$ylog_scale
      )

      time_col <- if (!is.null(individual_inputs()$profiles)) "ARRLT" else "AFRLT"

      tt_vars <- unique(c("AVAL", time_col,
                          "USUBJID", individual_inputs()$color_by))

      dose_data = if (individual_inputs()$show_dose) {
        data() %>%
          mutate(TIME_DOSE = round(AFRLT - ARRLT, 6))
      } else {
        NULL
      }

      lineplot <- g_lineplot(
        data = individual_output,
        x_var = time_col,
        y_var = "AVAL",
        color_by = individual_inputs()$color_by,
        facet_by = individual_inputs()$facet_by,
        ylog_scale = individual_inputs()$ylog_scale,
        threshold_value = individual_inputs()$threshold_value,
        dose_data = dose_data,
        palette = master_palettes_list(),
        tooltip_vars = tt_vars,
        labels_df = metadata_nca_variables
      )
      lineplot
    })

    # Save the object for the zip folder whenever it changes
    observe({
      req(individualplot())
      session$userData$results$exploration$individualplot <- individualplot()
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
        data(),
        mean_inputs()$color_by,
        palette_name = mean_inputs()$palette_theme
      )
    })

    # Compute the meanplot object
    meanplot <- reactive({
      req(data(), mean_inputs()$param, mean_inputs()$pcspec,
          mean_inputs()$color_by)
      log_info("Computing meanplot ggplot object")

      mean_output <- process_data_mean(
        data = data(),
        selected_analytes = mean_inputs()$param,
        selected_pcspec = mean_inputs()$pcspec,
        profiles_selected = mean_inputs()$profiles,
        ylog_scale = mean_inputs()$ylog_scale,
        color_by = mean_inputs()$color_by,
        facet_by = mean_inputs()$facet_by
      )

      time_col <- if (!is.null(mean_inputs()$profiles)) "NRRLT" else "NFRLT"

      tt_vars <- unique(c("Mean", time_col, mean_inputs()$colorby))

      dose_data = if (mean_inputs()$show_dose) {
        data() %>%
          mutate(TIME_DOSE = round(NFRLT - NRRLT, 6))
      } else {
        NULL
      }

      meanplot <- g_lineplot(
        data = mean_output,
        x_var = time_col,
        y_var = "Mean",
        color_by = mean_inputs()$color_by,
        facet_by = mean_inputs()$facet_by,
        ylog_scale = mean_inputs()$ylog_scale,
        sd_min = mean_inputs()$sd_min,
        sd_max = mean_inputs()$sd_max,
        ci = mean_inputs()$ci,
        threshold_value = mean_inputs()$threshold_value,
        dose_data = dose_data,
        palette = mean_palettes_list(),
        tooltip_vars = tt_vars,
        labels_df = metadata_nca_variables
      )

      session$userData$results$exploration$meanplot <- meanplot
      meanplot
    })

    # Save the objects for the ZIP folder whenever they change
    observe({
      req(individualplot(), meanplot())

      # Individual plots
      session$userData$results$exploration$individualplot <- individualplot()
      session$userData$results$exploration$individualplot_plotly <- ggplotly(individualplot(),
                                                                             tooltip = "text")

      # Mean plots
      session$userData$results$exploration$meanplot <- meanplot()
      session$userData$results$exploration$meanplot_plotly <- ggplotly(meanplot(), tooltip = "text")
    })

    # Render the mean plot output in plotly
    output$mean_plot <- renderPlotly({
      req(meanplot())
      ggplotly(meanplot(), tooltip = "text")
    })

    pk_dose_qc_plot_server("pk_dose_qc_plot", pknca_data, extra_group_vars)
  })
}
