parameter_plots_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      position = "right", open = TRUE,
      uiOutput(ns("params_to_display_ui_wrapper")
      ),
      uiOutput(ns("group_xvars_ui_wrapper")
      ),
      uiOutput(ns("select_colorvars_ui_wrapper")
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

    # Update picker inputs when boxplotdata is available
    observeEvent(res_nca(), {
      # Update the selected_param_boxplot picker input
      param_choices <- unique(res_nca()$result$PPTESTCD)

      default_selection <- if ("CMAX" %in% param_choices) {
        "CMAX"
      } else {
        param_choices[1]
      }

      conc_dose_cols <- unique(c(
        names(res_nca()$data$conc$data),
        names(res_nca()$data$dose$data)
      ))
      
      # Generate dataset for parameters and labels in the dropdowns
      parameters_choices <- reactive({
        req(metadata_nca_parameters)
        
        # Taking the parameters and labels from the metadata
        choices_df <- metadata_nca_parameters %>%
          select(PPTESTCD, PPTEST) %>%
          distinct(PPTESTCD, .keep_all = TRUE) %>%
          filter(!is.na(PPTESTCD), PPTESTCD != "") %>%
          filter(PPTESTCD %in% param_choices)
        
        unname(purrr::pmap(list(choices_df$PPTESTCD, choices_df$PPTEST), function(var, lab) {
          list(
            label = as.character(var),
            value = as.character(var),
            description = as.character(lab)
          )
        }))
      })
      
      # Rendering the parameters to display selector
      output$params_to_display_ui_wrapper <- renderUI({
        req(parameters_choices, res_nca())
        parameters_boxplot <- parameters_choices()
        
        shinyWidgets::virtualSelectInput(
          inputId = ns("selected_param_boxplot"),
          label = "Choose the parameter to display:",
          choices = parameters_boxplot,
          multiple = TRUE,
          selected = default_selection,
          search = TRUE,
          hasOptionDescription = TRUE,
          dropboxDirection = "bottom"
        )
      })
      
      # Generating dataset for the x grouping variables and their labels
      variables_choices <- reactive({
        req(metadata_nca_variables)
        
        # Taking the variables and labels from the metadata
        choices_df <- metadata_nca_variables %>%
          select(Variable, Label) %>%
          distinct(Variable, .keep_all = TRUE) %>%
          filter(!is.na(Variable), Variable != "") %>%
          filter(Variable %in% conc_dose_cols)
        
        unname(purrr::pmap(list(choices_df$Variable, choices_df$Label), function(var, lab) {
          list(
            label = as.character(var),
            value = as.character(var),
            description = as.character(lab)
          )
        }))
      })
      
      
      # Rendering the grouping x vars selector
      output$group_xvars_ui_wrapper <- renderUI({
        req(variables_choices())
        x_grouping_vars <- variables_choices()
        
        shinyWidgets::virtualSelectInput(
          inputId = ns("selected_xvars_boxplot"),
          label = "Select X grouping variables",
          choices = x_grouping_vars,
          multiple = TRUE,
          selected = c(res_nca()$data$dose$columns$dose,
                       res_nca()$data$conc$columns$groups$group_analyte),
          search = TRUE,
          hasOptionDescription = TRUE,
          dropboxDirection = "bottom"
        )
      })
      
      # Rendering select color vars dropdown
      output$select_colorvars_ui_wrapper <- renderUI({
        req(variables_choices())
        x_colouring_vars <- variables_choices()
        
        shinyWidgets::virtualSelectInput(
          inputId = ns("selected_colorvars_boxplot"),
          label = "Select X grouping variables",
          choices = x_colouring_vars,
          multiple = TRUE,
          selected = c(res_nca()$data$dose$columns$dose,
                       res_nca()$data$conc$columns$groups$group_analyte),
          search = TRUE,
          hasOptionDescription = TRUE,
          dropboxDirection = "bottom"
        )
      })
      
      updatePickerInput(
        session,
        "selected_colorvars_boxplot",
        choices = conc_dose_cols,
        selected = c(res_nca()$data$dose$columns$dose,
                     res_nca()$data$conc$columns$groups$group_analyte)
      )
    })

    observeEvent(list(input$selected_xvars_boxplot, input$selected_colorvars_boxplot), {
      .get_conc_dose_vals <- function(res_nca, id_var) {
        unique(c(
          res_nca$data$conc$data[[id_var]],
          res_nca$data$dose$data[[id_var]]
        ))
      }
      
      xvar_options_list <- lapply(
        c(input$selected_xvars_boxplot, input$selected_colorvars_boxplot),
        function(id_var) paste(id_var, .get_conc_dose_vals(res_nca(), id_var), sep = ": ")
      ) %>%
        setNames(c(input$selected_xvars_boxplot, input$selected_colorvars_boxplot))
      
      updatePickerInput(
        session,
        inputId = "selected_filters_boxplot",
        choices = xvar_options_list,
        selected = unlist(xvar_options_list)
      )
    })

    # compute the box plot
    boxplot <- reactive({
      req(input$selected_param_boxplot)
      req(input$selected_xvars_boxplot)
      req(input$selected_colorvars_boxplot)
      req(input$selected_filters_boxplot)
      log_info("Rendering boxplot")

      boxplot <- flexible_violinboxplot(
        res_nca = res_nca(),
        parameter = input$selected_param_boxplot,
        xvars = input$selected_xvars_boxplot,
        colorvars = input$selected_colorvars_boxplot,
        varvalstofilter = input$selected_filters_boxplot,
        tooltip_vars = unname(unlist(res_nca()$data$conc$columns$groups)),
        box = input$violinplot_toggle_switch,
      )
    })

    # Save the boxplot for the zip folder
    observe({
      req(boxplot())
      session$userData$results$nca_results$boxplot <- boxplot()
    })

    # Render the boxplot
    output$boxplot <- renderPlotly({
      boxplot()
    })
  })
}
