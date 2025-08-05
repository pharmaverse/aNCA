parameter_plots_ui <- function(id) {
  ns <- NS(id)
  
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
}


parameter_plots_server <- function(id, res_nca) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
      
      updatePickerInput(
        session,
        "selected_xvars_boxplot",
        choices = names(boxplotdata()),
        selected = "DOSEA"
      )
      
      updatePickerInput(
        session,
        "selected_colorvars_boxplot",
        choices = names(boxplotdata()),
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