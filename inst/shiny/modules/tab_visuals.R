# The Visualisation Navbar tab loads the data from the Data tab, and results from NCA tab
# The user can then explore the data using various visualisation tools

# VISUALISATION ----
tab_visuals_ui <- function(id) {
  ns <- NS(id)

  navset_card_pill(
    header = "Exploratory Analysis",
    id = "visuals",
    nav_panel("Individual Plots",
      layout_sidebar(
        sidebar = sidebar(
          position = "right", open = TRUE,
          selectInput(
            ns("palette_theme"),
            "Select Color Theme:",
            choices = c(
              "Default (ggplot2)" = "default",
              "Viridis" = "viridis",
              "Spectral" = "spectral"
            ),
            selected = "default"
          ),
          pickerInput(
            inputId = ns("generalplot_analyte"),
            label = "Select Analyte:",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            inputId = ns("generalplot_pcspec"),
            label = "Select Matrix:",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            inputId = ns("generalplot_usubjid"),
            label = "Select Subjects:",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            inputId = ns("generalplot_colorby"),
            label = "Choose the variables to color by",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            inputId = ns("generalplot_facetby"),
            label = "Choose the variables to facet by:",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          radioButtons(
            ns("log"),
            "Select the Plot type:",
            choices = c("Lin", "Log")
          ),
          radioButtons(
            ns("timescale"),
            "Choose the Timescale",
            choices = c("All Time", "By Dose Profile")
          ),
          conditionalPanel(
            condition = "input.timescale == 'By Dose Profile'",
            uiOutput(ns("cycle_select")),
            ns = NS(id)
          ),
          checkboxInput(ns("show_threshold"), label = "Show Threshold"),
          conditionalPanel(
            condition = "input.show_threshold == true",
            numericInput(
              ns("threshold_value"),
              label = "Threshold Value",
              value = 0
            ),
            ns = NS(id)
          ),
          checkboxInput(ns("show_dose"), label = "Show Dose Times"),
        ),
        plotlyOutput(ns("individualplot"))
      )
    ),
    nav_panel("Mean Plots",
      layout_sidebar(
        sidebar = sidebar(
          selectInput(
            inputId = ns("analyte_mean"),
            label = "Choose the Analyte:",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            inputId = ns("pcspec_mean"),
            label = "Choose the Matrix:",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            inputId = ns("studyid_mean"),
            label = "Choose the Study ID:",
            choices = NULL
          ),
          selectInput(
            inputId = ns("select_id_var"),
            label = "Choose the variable(s) to group by:",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            inputId = ns("cycles_mean"),
            label = "Choose the cycle:",
            choices = NULL
          ),
          checkboxInput(ns("log_mean_plot"), label = "Scale y Log"),
          checkboxInput(ns("sd_mean_plot_max"), label = "+SD"),
          checkboxInput(ns("sd_mean_plot_min"), label = "-SD"),
          checkboxInput(ns("mean_plot_ci"), label = "Show CI 95%"),
          position = "right",
          open = TRUE
        ),
        plotlyOutput(ns("mean_plot")),
        br(),
        helpText("If n<3 at the specified time point then the mean value is not displayed.")
      )
    ),
    nav_panel("Boxplot",
      layout_sidebar(
        sidebar = sidebar(
          position = "right", open = TRUE,
          pickerInput(
            inputId = ns("selected_param_boxplot"),
            label = "Choose the parameter to display:",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            inputId = ns("selected_xvars_boxplot"),
            label = "Select X grouping variables",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            inputId = ns("selected_colorvars_boxplot"),
            label = "Select coloring variables to differentiate boxplots",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            inputId = ns("selected_filters_boxplot"),
            label = "Select values to display for grouping",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          switchInput(
            inputId = ns("violinplot_toggle_switch"),
            label = "",
            value = TRUE,
            onLabel = "Boxplot",
            offLabel = "Violinplot"
          )
        ),
        plotlyOutput(ns("boxplot"))
      )
    )
  )
}

# SERVER LOGIC OF NAVBAR OUTPUT TAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# In the output tab of the navbar, dynamic graphics of the input
# as well as the results of the NCA analysis are displayed. The user can dynamically
# display graphics and summaries of these data.

tab_visuals_server <- function(id, data, grouping_vars, res_nca) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Plotting Input widgets --------------------------------------------------------

    observeEvent(data(), {
      req(data())
      # Update the analyte picker input
      param_choices_analyte <- data() %>%
        pull(PARAM) %>%
        unique()

      updatePickerInput(
        session,
        "generalplot_analyte",
        choices = param_choices_analyte,
        selected = param_choices_analyte[1]
      )

      # Update pcspec picker input
      param_choices_pcspec <- data() %>%
        pull(PCSPEC) %>%
        unique()

      updatePickerInput(
        session,
        "generalplot_pcspec",
        choices = param_choices_pcspec,
        selected = param_choices_pcspec[1]
      )

      # Update the usubjid picker input
      param_choices_usubjid <- data() %>%
        pull(USUBJID) %>%
        unique()

      updatePickerInput(
        session,
        "generalplot_usubjid",
        choices = param_choices_usubjid,
        selected = param_choices_usubjid[1]
      )

      # Update the colorby and facet by picker inputs
      all_cols <- names(data())
      cols_to_exclude <- c("AVAL", "ARRLT", "AFRLT", "NRRLT", "NFRLT")
      unit_cols <- all_cols[endsWith(all_cols, "U")]
      cols_to_exclude <- c(cols_to_exclude, unit_cols)
      param_choices <- sort(setdiff(all_cols, cols_to_exclude))

      updatePickerInput(
        session,
        "generalplot_colorby",
        choices = param_choices,
        selected = "USUBJID"
      )

      updatePickerInput(
        session,
        "generalplot_facetby",
        choices = param_choices,
        selected = NULL
      )

      # Update the analyte mean select input
      analyte_choices <- data() %>%
        pull(PARAM) %>%
        unique()

      updateSelectInput(
        session,
        "analyte_mean",
        choices = sort(analyte_choices),
        selected = sort(analyte_choices)[1]
      )

      # Update the studyidmean select input
      studyid_choices <- data() %>%
        pull(STUDYID) %>%
        unique()

      updateSelectInput(
        session,
        "studyid_mean",
        choices = sort(studyid_choices)
      )

      # Update pcspec mean choices
      pcspec_choices <- data() %>%
        pull(PCSPEC) %>%
        unique()

      updateSelectInput(
        session,
        "pcspec_mean",
        choices = sort(pcspec_choices),
        selected = sort(pcspec_choices)[1]
      )

      # Update the selectidvar select input
      idvar_choices <- c("PARAM", "PCSPEC", "DOSEA", grouping_vars())

      updateSelectInput(
        session,
        "select_id_var",
        choices = idvar_choices,
        selected = "DOSEA"
      )
    })

    # select the dose number/cycle for the general lineplot, if plotting by cycle is chosen
    output$cycle_select <- renderUI({
      req(input$generalplot_analyte)
      y <- data() %>%
        filter(PARAM %in% input$generalplot_analyte) %>%
        pull(NCA_PROFILE) %>%
        unique()
      selectInput(ns("cycles"), "Choose the cycle :", choices = sort(y),
                  multiple = TRUE, selected = y[1])
    })

    # TAB: General Lineplot --------------------------------------------------------

    master_palettes_list <- reactive({
      req(input$palette_theme)
      req(input$generalplot_colorby)

      get_persistent_palette(
        data(),
        input$generalplot_colorby,
        palette_name = input$palette_theme # Use the user's choice
      )
    })

    # render the general lineplot output in plotly
    output$individualplot <- renderPlotly({
      req(data())
      req(master_palettes_list())
      req(input$generalplot_analyte)
      req(input$generalplot_pcspec)
      req(input$generalplot_usubjid)
      req(input$generalplot_colorby)
      req(input$timescale)
      req(input$log)
      log_info("Rendering individual plots")


      palettes <- master_palettes_list()

      plot_data <- data() %>%
        mutate( #round to prevent floating point precision issues
          TIME_DOSE = round(AFRLT - ARRLT, 6)
        )

      p <- general_lineplot(
        plot_data,
        input$generalplot_analyte,
        input$generalplot_pcspec,
        input$generalplot_usubjid,
        input$generalplot_colorby,
        input$generalplot_facetby,
        input$timescale,
        input$log,
        input$show_threshold,
        input$threshold_value,
        input$show_dose,
        cycle = input$cycles,
        palette = palettes
      ) %>%
        ggplotly()

      # Conditionally add rangeslider only if the plot is not faceted
      if (is.null(input$generalplot_facetby) || length(input$generalplot_facetby) == 0) {
        p <- p %>%
          layout(
            xaxis = list(
              rangeslider = list(type = "time")
            )
          )
      }
      p
    })

    # TAB: Mean Plot -----------------------------------------------------------

    # This tabs plots the mean concentration of the input data in a dynamic plot

    # Update the cyclesmean select input based on selected analyte
    observeEvent(input$analyte_mean, {
      req(data())
      cycle_choices <- data() %>%
        filter(PARAM %in% input$analyte_mean) %>%
        pull(NCA_PROFILE) %>%
        unique()

      updateSelectInput(
        session,
        "cycles_mean",
        choices = sort(cycle_choices)
      )
    })

    # render the mean plot output in plotly
    output$mean_plot <- renderPlotly({
      req(input$studyid_mean)
      req(input$analyte_mean)
      req(input$pcspec_mean)
      req(input$cycles_mean)
      log_info("Rendering mean plot")

      validate(
        need(
          data() %>%
            filter(
              STUDYID %in% input$studyid_mean,
              PARAM %in% input$analyte_mean,
              PCSPEC %in% input$pcspec_mean,
              NCA_PROFILE %in% input$cycles_mean,
              if ("EVID" %in% names(data)) EVID == 0 else TRUE,
              NRRLT > 0
            ) %>%
            group_by(!!!syms(input$select_id_var), NRRLT) %>%
            summarise(N = n()) %>%
            filter(N >= 3) %>%
            nrow(.) > 0,
          message = paste0(
            "Data issue: No data with more than 3 points to calculate average based on",
            "nominal time (NRRLT) and selected variable: ",
            input$select_id_var
          )
        )
      )

      general_meanplot(
        data = data(),
        selected_studyids = input$studyid_mean,
        selected_analytes = input$analyte_mean,
        selected_pcspecs = input$pcspec_mean,
        selected_cycles = input$cycles_mean,
        id_variable = input$select_id_var,
        plot_ylog = input$log_mean_plot,
        plot_sd_min = input$sd_mean_plot_min,
        plot_sd_max = input$sd_mean_plot_max,
        plot_ci = input$mean_plot_ci
      ) %>%
        ggplotly() %>%
        layout(
          xaxis = list(
            rangeslider = list(type = "time")
          )
        )

    })

    # TAB: Parameter Box plots ----------------------------------------------------

    # Create formatted Box plot data: PKNCA + PP results, linking DOSEA + PPTESTCD
    boxplotdata <- reactive({
      group_columns <- unname(unlist(res_nca()$data$conc$columns$groups))
      req(res_nca())
      left_join(
        res_nca()$result,
        res_nca()$data$conc$data %>%
          distinct(across(all_of(group_columns)), .keep_all = TRUE),
        by = group_columns,
        keep = FALSE
      ) %>%
        # Intervals should also be considered as differentiated options each
        mutate(
          PPTESTCD = ifelse(
            startsWith(PPTESTCD, "aucint"),
            paste0(PPTESTCD, "_", start, "-", end),
            PPTESTCD
          )
        )
    })

    # Update picker inputs when boxplotdata is available
    observeEvent(boxplotdata(), {
      # Update the selected_param_boxplot picker input
      param_choices <- boxplotdata() %>%
        pull(PPTESTCD) %>%
        unique()

      updatePickerInput(
        session,
        "selected_param_boxplot",
        choices = param_choices
      )

      # Update the selected_xvars_boxplot picker input
      xvar_choices <- intersect(names(boxplotdata()), names(data()))

      updatePickerInput(
        session,
        "selected_xvars_boxplot",
        choices = xvar_choices,
        selected = "DOSEA"
      )

      # Update the selected_colorvars_boxplot picker input
      colorvar_choices <- intersect(names(boxplotdata()), names(data()))

      updatePickerInput(
        session,
        "selected_colorvars_boxplot",
        choices = colorvar_choices,
        selected = "PARAM"
      )
    })

    observeEvent(list(input$selected_xvars_boxplot, input$selected_colorvars_boxplot), {
      xvar_options_list <- lapply(
        c(input$selected_xvars_boxplot, input$selected_colorvars_boxplot),
        \(id_var) paste(id_var, unique(boxplotdata()[[id_var]]), sep = ": ")
      ) |>
        setNames(c(input$selected_xvars_boxplot, input$selected_colorvars_boxplot))

      updatePickerInput(
        session = session,
        inputId = "selected_filters_boxplot",
        label = "Select values to display for grouping",
        choices = xvar_options_list,
        selected = unlist(xvar_options_list)
      )
    })

    # compute the box plot
    output$boxplot <- renderPlotly({
      req(boxplotdata())
      req(input$selected_param_boxplot)
      req(input$selected_xvars_boxplot)
      req(input$selected_colorvars_boxplot)
      req(input$selected_filters_boxplot)
      log_info("Rendering boxplot")

      flexible_violinboxplot(
        boxplotdata = boxplotdata(),
        parameter = input$selected_param_boxplot,
        xvars = input$selected_xvars_boxplot,
        colorvars = input$selected_colorvars_boxplot,
        varvalstofilter = input$selected_filters_boxplot,
        columns_to_hover = unname(unlist(res_nca()$data$conc$columns$groups)),
        box = input$violinplot_toggle_switch
      )
    })

  })
}
