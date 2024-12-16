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
          pickerInput(
            inputId = ns("generalplot_analyte"),
            label = "Select Analyte:",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
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
            ns("log"), "Select the Plot type:", choices = c("Lin", "Log")),
          radioButtons(
            ns("timescale"), "Choose the Timescale",
            choices = c("All Time", "By Cycle"), selected = "All Time"
          ),
          conditionalPanel(
           condition = "input.timescale == 'By Cycle'",
           uiOutput(ns("cycleselect")),
           ns = NS(id)
          ),
          position = "right",
          open = TRUE),
        plotlyOutput(ns("individualplot"))
        )
    ),
    nav_panel("Mean Plots",
      layout_sidebar(
        sidebar = sidebar(
          selectInput(
            inputId = ns("analytemean"),
            label = "Choose the Analyte:",
            choices = NULL
          ),
          selectInput(
            inputId = ns("studyidmean"),
            label = "Choose the Study ID:",
            choices = NULL
          ),
          selectInput(
            inputId = ns("selectidvar"),
            label = "Choose the variable to group by:",
            choices = c("PCSPEC", "DOSEA", "TRT01A", "TRT01P"),
            selected = "DOSEA"
          ),
          selectInput(
            inputId = ns("cyclesmean"),
            label = "Choose the cycle:",
            choices = NULL
          ),
          checkboxInput(ns("logmeanplot"), label = "Scale y Log"),
          checkboxInput(ns("sdmeanplot"), label = "Show SD"),
          checkboxInput(ns("mean_plot_ci"), label = "Show CI 95%"),
          position = "right",
          open = TRUE),
        plotlyOutput(ns("meanplot")),
        br(),
        helpText("If n<3 at the specified time point 
                 then the mean value is not displayed.")
        )
      ),
    nav_panel("Descriptive Statistics",
      layout_sidebar(
        sidebar = sidebar(
          pickerInput(
            inputId = ns("paramselect"),
            label = "Filter parameters to display:",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          position = "right",
          open = TRUE),
        card(
          orderInput(
            ns("summarygroupbysource"),
            "Drag and drop these variables...",
            items = c("STUDYID", "USUBJID", "DOSEA", "PCSPEC", "ANALYTE"),
            width = shiny::validateCssUnit("100%"),
            connect = ns("summarygroupby")
          ),
          orderInput(
            ns("summarygroupby"),
            "..to hierarchically group by (order matters!):",
            items = c("DOSNO"),
            width = shiny::validateCssUnit("100%"),
            connect = ns("summarygroupbysource"),
            placeholder = "Drag items here to group hierarchically..."
          )
        ),
        card(
             reactableOutput(ns("descriptivestats"))
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
            inputId = ns("selected_varvalstofilter_boxplot"),
            label = "Select values to display",
            multiple = TRUE,
            choices = NULL,
            selected = NULL,
            options = list(`actions-box` = TRUE)
          ),
          switchInput(
            inputId = ns("violinplot_toggle_switch"),
            label = "",
            value = TRUE,
            onLabel = "Boxplot",
            offLabel = "Violinplot"
          ),
          position = "right",
          open = TRUE),
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
        choices = param_choices_analyte
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
      param_choices_colorby <- sort(c("STUDYID", "PCSPEC", "ANALYTE", "DOSEA", "DOSNO", "USUBJID", grouping_vars()))
      
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
        "analytemean",
        choices = sort(analyte_choices)
      )
      
      # Update the studyidmean select input
      studyid_choices <- data() %>%
        pull(STUDYID) %>%
        unique()
      
      updateSelectInput(
        session,
        "studyidmean",
        choices = sort(studyid_choices)
      )
      
      # Update the selectidvar select input
      idvar_choices <- c("PCSPEC", "DOSEA", grouping_vars())
      
      updateSelectInput(
        session,
        "selectidvar",
        choices = idvar_choices,
        selected = "DOSEA"
      )
    })

    # select the dose number/cycle for the general lineplot, if plotting by cycle is chosen
    output$cycleselect <- renderUI({
      req(input$generalplot_analyte)
      y <- data() %>%
        filter(ANALYTE == input$generalplot_analyte) %>%
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
      req(input$generalplot_usubjid)
      req(input$generalplot_colorby)
      req(input$timescale)
      req(input$log)

      general_lineplot(
        data(),
        input$generalplot_analyte,
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
    observeEvent(input$analytemean, {
      req(data())
      cycle_choices <- data() %>%
        filter(ANALYTE %in% input$analytemean) %>%
        pull(DOSNO) %>%
        unique()
      
      updateSelectInput(
        session,
        "cyclesmean",
        choices = sort(cycle_choices)
      )
    })

    # render the mean plot output in plotly
    output$meanplot <- renderPlotly({
      req(input$studyidmean)
      req(input$analytemean)
      req(input$cyclesmean)

      validate(
        need(
          data() %>%
            filter(
              STUDYID %in% input$studyidmean,
              ANALYTE %in% input$analytemean,
              DOSNO %in% input$cyclesmean,
              if ("EVID" %in% names(data)) EVID == 0 else TRUE,
              NRRLT > 0
            ) %>%
            group_by(!!sym(input$selectidvar), NRRLT) %>%
            summarise(N = n()) %>%
            filter(N >= 3) %>%
            nrow(.) > 0,
          message = paste0(
          "Data issue: No data with more than 3 points to calculate average based on
          nominal time (NRRLT) and selected variable: ",
          input$selectidvar)
        )
      )

      general_meanplot(data = data(),
                       selected_studyids = input$studyidmean,
                       selected_analytes = input$analytemean,
                       selected_cycles = input$cyclesmean,
                       id_variable = input$selectidvar,
                       plot_ylog = input$logmeanplot,
                       plot_sd = input$sdmeanplot,
                       plot_ci = input$mean_plot_ci) %>%
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
        "paramselect",
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
      updateOrderInput(session, "summarygroupbysource",
                       items = c(group_cols, classification_cols))
    })

    # Reactive expression for summary table based on selected group and parameters
    summary_stats <- reactive({
      req(input$summarygroupby, input$paramselect)

      # Calculate summary stats and filter by selected parameters
      calculate_summary_stats(res_nca(), input$summarygroupby) %>%
        filter(PPTESTCD %in% input$paramselect)  %>%
        rename(PARAM = PPTESTCD)
    })

    # render the reactive summary table in a data table
    output$descriptivestats <- renderReactable({
      req(summary_stats())
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
        paste("NCA_summary.csv", sep = "_")
      },
      content = function(file) {
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
        inputId = "selected_varvalstofilter_boxplot",
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
      req(input$selected_varvalstofilter_boxplot)

      flexible_violinboxplot(
        boxplotdata = boxplotdata(),
        parameter = input$selected_param_boxplot,
        xvars = input$selected_xvars_boxplot,
        colorvars = input$selected_colorvars_boxplot,
        varvalstofilter = input$selected_varvalstofilter_boxplot,
        columns_to_hover = unname(unlist(res_nca()$data$conc$columns$groups)),
        box = input$violinplot_toggle_switch
      )
    })

  })
}
