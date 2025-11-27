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

    # Update picker inputs when boxplotdata is available
    observeEvent(res_nca(), {
      # Update the selected_param_boxplot picker input
      param_choices <- unique(res_nca()$result$PPTESTCD)
      conc_dose_cols <- unique(c(
        names(res_nca()$data$conc$data),
        names(res_nca()$data$dose$data)
      ))

      updatePickerInput(
        session,
        "selected_param_boxplot",
        choices = param_choices
      )

      updatePickerInput(
        session,
        "selected_xvars_boxplot",
        choices = conc_dose_cols,
        selected = res_nca()$data$dose$columns$dose
      )

      updatePickerInput(
        session,
        "selected_colorvars_boxplot",
        choices = conc_dose_cols,
        selected = res_nca()$data$conc$columns$groups$group_analyte
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
        session = session,
        inputId = "selected_filters_boxplot",
        label = "Select values to display for grouping",
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
