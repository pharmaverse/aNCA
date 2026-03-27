#' Module for interactive violin/box plots of NCA parameters.
#'
#' Provides controls to select a parameter, grouping variables, coloring
#' variables, and filter values, then renders the plot with export support.
#'
#' @param id Module namespace ID.

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

      # Rendering parameters to display selector with labels
      selector_label(input = input,
                     output = output,
                     session = session,
                     choices = param_choices,
                     initial_selection = default_selection,
                     selector_ui_wrapper = "params_to_display_ui_wrapper",
                     id = "selected_param_boxplot",
                     label = "Choose the parameter to display:",
                     metadata_type = "parameter",
                     multiple = FALSE)

      # Rendering parameters to display selector with labels
      selector_label(input = input,
                     output = output,
                     session = session,
                     choices = conc_dose_cols,
                     initial_selection = c(res_nca()$data$dose$columns$dose,
                                           res_nca()$data$conc$columns$groups$group_analyte),
                     selector_ui_wrapper = "group_xvars_ui_wrapper",
                     id = "selected_xvars_boxplot",
                     label = "Select X grouping variables:",
                     pknca_data = NULL,
                     metadata_type = "variable")

      # Rendering select color vars selector with labels
      selector_label(input = input,
                     output = output,
                     session = session,
                     choices = conc_dose_cols,
                     initial_selection = c(res_nca()$data$dose$columns$dose,
                                           res_nca()$data$conc$columns$groups$group_analyte),
                     selector_ui_wrapper = "select_colorvars_ui_wrapper",
                     id = "selected_colorvars_boxplot",
                     label = "Select coloring variables to differentiate boxplots:",
                     metadata_type = "variable")
    })

    observeEvent(list(input$selected_xvars_boxplot, input$selected_colorvars_boxplot), {
      .get_conc_dose_vals <- function(res_nca, id_var) {
        unique(c(
          res_nca$data$conc$data[[id_var]],
          res_nca$data$dose$data[[id_var]]
        ))
      }

      all_vars <- unique(c(input$selected_xvars_boxplot, input$selected_colorvars_boxplot))

      xvar_options_list <- lapply(
        all_vars,
        function(id_var) paste(id_var, .get_conc_dose_vals(res_nca(), id_var), sep = ": ")
      ) %>%
        setNames(all_vars)

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
      req(res_nca())
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
