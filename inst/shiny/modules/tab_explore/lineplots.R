#' Reusable Plot Sidebar UI
#'
#' @param id A character string specifying the module ID.
#' @param is_mean_plot A logical value indicating whether to show options
#'                     specific to the mean plot. Default is FALSE.
#'
#' @return A UI definition for the plot sidebar.
plot_sidebar_ui <- function(id, is_mean_plot = FALSE) {
  ns <- NS(id)

  sidebar(
    position = "right",
    open = TRUE,
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
      inputId = ns("param"),
      label = "Select Parameters:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    pickerInput(
      inputId = ns("pcspec"),
      label = "Select Matrix:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    if (!is_mean_plot) {
      pickerInput(
        inputId = ns("usubjid"),
        label = "Select Subjects:",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    },
    pickerInput(
      inputId = ns("colorby"),
      label = "Choose the variables to color by:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    pickerInput(
      inputId = ns("facetby"),
      label = "Choose the variables to facet by:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    checkboxInput(
      ns("log"),
      "Semi-log scale"
    ),
    radioButtons(
      ns("timescale"),
      "Choose the Timescale",
      choices = c("All Time", "By Dose Profile")
    ),
    conditionalPanel(
      condition = "input.timescale == 'By Dose Profile'",
      uiOutput(ns("profile_selection")),
      ns = ns
    ),
    checkboxInput(ns("show_threshold"), label = "Show Threshold"),
    conditionalPanel(
      condition = "input.show_threshold == true",
      numericInput(ns("threshold_value"), label = "Threshold Value", value = NULL),
      ns = ns
    ),
    checkboxInput(ns("show_dose"), label = "Show Dose Times"),
    # --- Conditional UI Elements ---
    if (is_mean_plot) {
      tagList(
        checkboxInput(ns("sd_max"), label = "+SD", value = TRUE),
        checkboxInput(ns("sd_min"), label = "-SD", value = FALSE),
        checkboxInput(ns("ci"), label = "Show 95% CI", value = FALSE),
        helpText("Mean values are not displayed if n < 3 for a time point.")
      )
    }
  )
}

#' Reusable Plot Sidebar Server
#'
#' @param id A character string specifying the module ID.
#' @param pknca_data A PKNCA data object.
#' @param grouping_vars A reactive vector of grouping variables.
#'
#' @return A list of reactive inputs from the sidebar.
plot_sidebar_server <- function(id, pknca_data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(pknca_data(), {
      req(pknca_data())

      # Get the concentration data and grouping variables
      data <- pknca_data()$conc$data
      conc_groups <- group_vars(pknca_data()$conc)
      dose_groups <- group_vars(pknca_data()$dose)
      dose_col <- pknca_data()$dose$columns$dose
      subject_col <- pknca_data()$conc$columns$subject

      # Update the param picker input
      param_choices <- data %>%
        pull(PARAM) %>%
        unique()

      updatePickerInput(
        session,
        "param",
        choices = param_choices,
        selected = param_choices[1]
      )

      # Update pcspec picker input
      pcspec_choices <- data %>%
        pull(PCSPEC) %>%
        unique()

      updatePickerInput(
        session,
        "pcspec",
        choices = pcspec_choices,
        selected = pcspec_choices[1]
      )

      # Update the usubjid picker input (if it exists)
      if ("usubjid" %in% names(input)) {
        usubjid_choices <- data %>%
          pull(USUBJID) %>%
          unique()

        updatePickerInput(
          session,
          "usubjid",
          choices = usubjid_choices,
          selected = usubjid_choices
        )
      }

      full_grouping_vars <- unique(c(conc_groups, dose_groups,
                                     dose_col, grouping_vars(), "ATPTREF"))

      updatePickerInput(
        session,
        "colorby",
        choices = full_grouping_vars,
        # Always select USUBJID if individual, if mean plot, select nothing
        selected = if ("usubjid" %in% names(input)) subject_col else dose_col
      )

      updatePickerInput(
        session,
        "facetby",
        choices = full_grouping_vars,
        selected = NULL
      )
    })

    # Render the cycle selection UI
    output$profile_selection <- renderUI({
      req(input$param)
      y <- pknca_data()$conc$data %>%
        filter(PARAM %in% input$param) %>%
        pull(ATPTREF) %>%
        unique()
      pickerInput(
        ns("profiles"),
        "Choose the profile(s):",
        choices = sort(y),
        multiple = TRUE, selected = y[1], options = list(`actions-box` = TRUE)
      )
    })

    filtering_list <- reactive({
      lst <- list(
        PARAM = input$param,
        PCSPEC = input$pcspec
      )
      if ("usubjid" %in% names(input)) {
        lst$USUBJID <- input$usubjid
      }
      if (!is.null(input$profiles)) {
        lst$ATPTREF <- input$profiles
      }
      lst
    })

    # Return all inputs as a list of reactives
    reactive({
      list(
        palette_theme = input$palette_theme,
        param = input$param,
        pcspec = input$pcspec,
        usubjid = input$usubjid,
        color_by = input$colorby,
        facet_by = input$facetby,
        ylog_scale = input$log,
        profiles_selected = input$profiles,
        threshold_value = input$threshold_value,
        show_dose = input$show_dose,
        sd_max = input$sd_max,
        sd_min = input$sd_min,
        ci = input$ci,
        filtering_list = filtering_list(),
        use_time_since_last_dose = input$timescale == "By Dose Profile",
        palette = input$palette_theme
      )
    })
  })
}
