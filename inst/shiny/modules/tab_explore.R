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

    # Counters for incremented plot names in the ZIP export
    indiv_counter <- reactiveVal(0L)
    mean_counter <- reactiveVal(0L)
    qc_counter <- reactiveVal(0L)

    # Initiate the sidebar server modules
    individual_sidebar <- plot_sidebar_server(
      "individual_sidebar",
      pknca_data = pknca_data,
      grouping_vars = extra_group_vars
    )

    mean_sidebar <- plot_sidebar_server(
      "mean_sidebar",
      pknca_data = pknca_data,
      grouping_vars = extra_group_vars
    )

    individual_inputs <- individual_sidebar$inputs
    mean_inputs <- mean_sidebar$inputs

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
        x_limits = individual_inputs()$x_limits,
        y_limits = individual_inputs()$y_limits,
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
        x_limits = mean_inputs()$x_limits,
        y_limits = mean_inputs()$y_limits,
        threshold_value = mean_inputs()$threshold_value,
        labels_df = metadata_nca_variables,
        use_time_since_last_dose = mean_inputs()$use_time_since_last_dose
      )
    })

    # Save the default objects for the ZIP folder whenever they change
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

    qc_plot_outputs <- pk_dose_qc_plot_server(
      "pk_dose_qc_plot", pknca_data, extra_group_vars
    )

    # Save the default QC plot for the ZIP folder
    observe({
      req(qc_plot_outputs$current_plot())
      session$userData$results$exploration$qcplot <- qc_plot_outputs$current_plot()
    })

    # --- Add to Report handlers ---

    # Track which plot type triggered the modal
    pending_plot_type <- reactiveVal(NULL)

    # Show modal with filename input
    .show_report_modal <- function(default_name) {
      showModal(modalDialog(
        title = "Add to Report",
        textInput(ns("report_plot_name"), "Plot name:", value = default_name),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_add_to_report"), "Save",
                       class = "btn btn-primary")
        ),
        size = "s",
        easyClose = TRUE
      ))
    }

    # Individual plot button
    observeEvent(individual_sidebar$add_to_report(), {
      req(individualplot())
      n <- indiv_counter() + 1L
      indiv_counter(n)
      pending_plot_type("individual")
      .show_report_modal(paste0("individualplot", n))
    })

    # Mean plot button
    observeEvent(mean_sidebar$add_to_report(), {
      req(meanplot())
      n <- mean_counter() + 1L
      mean_counter(n)
      pending_plot_type("mean")
      .show_report_modal(paste0("meanplot", n))
    })

    # QC plot button
    observeEvent(qc_plot_outputs$add_to_report(), {
      req(qc_plot_outputs$current_plot())
      n <- qc_counter() + 1L
      qc_counter(n)
      pending_plot_type("qc")
      .show_report_modal(paste0("qcplot", n))
    })

    # Confirm save from modal
    observeEvent(input$confirm_add_to_report, {
      plot_name <- gsub("[^A-Za-z0-9_-]", "_", input$report_plot_name)
      req(nzchar(plot_name))

      plot_obj <- switch(pending_plot_type(),
        individual = individualplot(),
        mean = meanplot(),
        qc = qc_plot_outputs$current_plot()
      )
      req(plot_obj)

      session$userData$results$exploration[[plot_name]] <- plot_obj
      removeModal()
      showNotification(
        paste0("Plot saved as '", plot_name, "'"),
        type = "message", duration = 3
      )
      log_info("Saved exploration plot: {plot_name}")
    })
  })
}
