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
          radioButtons(
            ns("log"),
            "Select the Plot type:",
            choices = c("Lin", "Log")
          ),
          radioButtons(
            ns("timescale"),
            "Choose the Timescale",
            choices = c("All Time", "By Cycle")
          ),
          conditionalPanel(
            condition = "input.timescale == 'By Cycle'",
            uiOutput(ns("cycle_select")),
            ns = NS(id)
          )
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
            label = "Choose the variable to group by:",
            choices = NULL
          ),
          selectInput(
            inputId = ns("cycles_mean"),
            label = "Choose the cycle:",
            choices = NULL
          ),
          checkboxInput(ns("log_mean_plot"), label = "Scale y Log"),
          checkboxInput(ns("sd_mean_plot"), label = "Show SD"),
          checkboxInput(ns("mean_plot_ci"), label = "Show CI 95%"),
          position = "right",
          open = TRUE
        ),
        plotlyOutput(ns("mean_plot")),
        br(),
        helpText("If n<3 at the specified time point then the mean value is not displayed.")
      )
    ),
    nav_panel("Descriptive Statistics",
      layout_sidebar(
        sidebar = sidebar(
          position = "right", open = TRUE,
          pickerInput(
            inputId = ns("select_display_parameters"),
            label = "Filter parameters to display:",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          )
        ),
        card(
          orderInput(
            ns("summary_groupby_source"),
            "Drag and drop these variables...",
            items = c("STUDYID", "USUBJID", "DOSEA"),
            width = shiny::validateCssUnit("100%"),
            connect = ns("summary_groupby")
          ),
          orderInput(
            ns("summary_groupby"),
            "..to hierarchically group by (order matters!):",
            items = c( "ANALYTE", "PCSPEC", "DOSNO"),
            width = shiny::validateCssUnit("100%"),
            connect = ns("summary_groupby_source"),
            placeholder = "Drag items here to group hierarchically..."
          )
        ),
        card(
          reactableOutput(ns("descriptive_stats"))
        ),
        card(
          actionButton(ns("download_improve"), "Download the NCA Summary Data to Improve"),
          downloadButton(ns("download_browser"), "Download the NCA Summary Data")
        )
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
        pull(ANALYTE) %>%
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

      # Update the colorby picker input
      param_choices_colorby <- sort(
        c("STUDYID", "PCSPEC", "ANALYTE", "DOSEA", "DOSNO", "USUBJID", grouping_vars()) 
      )

      updatePickerInput(
        session,
        "generalplot_colorby",
        choices = param_choices_colorby,
        selected = param_choices_colorby[length(param_choices_colorby)]
      )

      # Update the analytemean select input
      analyte_choices <- data() %>%
        pull(ANALYTE) %>%
        unique()

      updateSelectInput(
        session,
        "analyte_mean",
        choices = sort(analyte_choices)
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
        choices = sort(pcspec_choices)
      )

      # Update the selectidvar select input
      idvar_choices <- c("ANALYTE","PCSPEC", "DOSEA", grouping_vars())

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
        filter(ANALYTE %in% input$generalplot_analyte) %>%
        pull(DOSNO) %>%
        unique()
      selectInput(ns("cycles"), "Choose the cycle :", choices = sort(y),
                  multiple = TRUE, selected = y[1])
    })

    # TAB: General Lineplot --------------------------------------------------------

    # render the general lineplot output in plotly
    output$individualplot <- renderPlotly({
      req(data())
      req(input$generalplot_analyte)
      req(input$generalplot_pcspec)
      req(input$generalplot_usubjid)
      req(input$generalplot_colorby)
      req(input$timescale)
      req(input$log)
      log_info("Rendering individual plots")

      general_lineplot(
        data(),
        input$generalplot_analyte,
        input$generalplot_pcspec,
        input$generalplot_usubjid,
        input$generalplot_colorby,
        input$timescale,
        input$log,
        cycle = input$cycles
      )

    })

    # TAB: Mean Plot -----------------------------------------------------------

    # This tabs plots the mean concentration of the input data in a dynamic plot

    # Update the cyclesmean select input based on selected analyte
    observeEvent(input$analyte_mean, {
      req(data())
      cycle_choices <- data() %>%
        filter(ANALYTE %in% input$analyte_mean) %>%
        pull(DOSNO) %>%
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
              ANALYTE %in% input$analyte_mean,
              PCSPEC %in% input$pcspec_mean,
              DOSNO %in% input$cycles_mean,
              if ("EVID" %in% names(data)) EVID == 0 else TRUE,
              NRRLT > 0
            ) %>%
            group_by(!!sym(input$select_id_var), NRRLT) %>%
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
        plot_sd = input$sd_mean_plot,
        plot_ci = input$mean_plot_ci
      ) %>%
        ggplotly() %>%
        plotly_build()

    })


    # TAB: Descriptive Statistics ---------------------------------------------
    # This tab computes and visualizes output data from the NCA analysis

    # Update inputs based on what is available in the data
    observeEvent(res_nca(), {

      # Update the paramselect picker input
      paramselection <- unique(res_nca()$result$PPTESTCD)

      updatePickerInput(
        session,
        "select_display_parameters",
        choices = paramselection,
        selected = paramselection
      )

      # Define the relevant columns for the group by picker
      group_cols <- unname(unlist(res_nca()$data$conc$columns$groups))
      classification_cols <- sort(grouping_vars())
      classification_cols <- classification_cols[
        classification_cols %in% names(res_nca()$data$conc$data)
      ]

      # update the input for the group by picker
      updateOrderInput(session, "summary_groupby_source",
                       items = c(group_cols, classification_cols))
    })

    # Reactive expression for summary table based on selected group and parameters
    summary_stats <- reactive({
      req(input$summary_groupby, input$select_display_parameters)

      # Calculate summary stats and filter by selected parameters
      calculate_summary_stats(res_nca(), input$summary_groupby) %>%
        filter(PPTESTCD %in% input$select_display_parameters)  %>%
        rename(PARAM = PPTESTCD)
    })

    # render the reactive summary table in a data table
    output$descriptive_stats <- renderReactable({
      req(summary_stats())
      log_info("Rendering descriptive statistics table")

      reactable(
        summary_stats(),
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = TRUE,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        bordered = TRUE,
        height = "60vh"
      )
    })

    # Save summary stats to improve
    observeEvent(input$download_improve, {
      showModal(modalDialog(
        title = "Please enter the path to the folder on improve for your results:",
        textInput(ns("pathresults"), "Path:"),
        textInput(ns("filename"), "Enter filename of choice:", ""),
        actionButton(ns("go2"), "GO"),
        footer = modalButton("Close")
      ))
    })

    # alternatively: save locally
    output$download_browser <- downloadHandler(
      filename = function() {
        paste("NCA_summary.csv")
      },
      content = function(file) {
        log_info("Downloading summary statistics as CSV")
        write.csv(summary_stats(), file)
      }
    )

    # TAB: Parameter Box plots ----------------------------------------------------

    # Create formatted Box plot data: PKNCA + PP results, linking DOSEA + PPTESTCD
    boxplotdata <- reactive({
      group_columns <- unname(unlist(res_nca()$data$conc$columns$groups))

      left_join(
        res_nca()$result %>%
          filter(
            end == Inf | startsWith(PPTESTCD, "aucint")
          ),
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
        selected = "DOSNO"
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
