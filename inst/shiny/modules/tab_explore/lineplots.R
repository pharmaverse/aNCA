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
      "Use Logarithmic Y-Axis"
    ),
    radioButtons(
      ns("timescale"),
      "Choose the Timescale",
      choices = c("All Time", "By Dose Profile")
    ),
    conditionalPanel(
      condition = "input.timescale == 'By Dose Profile'",
      uiOutput(ns("cycle_select_ui")),
      ns = ns
    ),
    checkboxInput(ns("show_threshold"), label = "Show Threshold"),
    conditionalPanel(
      condition = "input.show_threshold == true",
      numericInput(ns("threshold_value"), label = "Threshold Value", value = 0),
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
#' @param data A reactive data frame.
#' @param grouping_vars A reactive vector of grouping variables.
#'
#' @return A list of reactive inputs from the sidebar.
plot_sidebar_server <- function(id, data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(data(), {
      req(data())

      # Update the param picker input
      param_choices <- data() %>%
        pull(PARAM) %>%
        unique()

      updatePickerInput(
        session,
        "param",
        choices = param_choices,
        selected = param_choices[1]
      )

      # Update pcspec picker input
      pcspec_choices <- data() %>%
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
        usubjid_choices <- data() %>%
          pull(USUBJID) %>%
          unique()

        updatePickerInput(
          session,
          "usubjid",
          choices = usubjid_choices,
          selected = usubjid_choices
        )
      }

      # Update the colorby and facet by picker inputs
      all_cols <- names(data())
      cols_to_exclude <- c("AVAL", "ARRLT", "AFRLT", "NRRLT", "NFRLT")
      unit_cols <- all_cols[endsWith(all_cols, "U")]
      cols_to_exclude <- c(cols_to_exclude, unit_cols)
      param_choices_cf <- sort(setdiff(all_cols, cols_to_exclude))

      updatePickerInput(
        session,
        "colorby",
        choices = param_choices_cf,
        # Always select USUBJID if individual, if mean plot, select nothing
        selected = if ("usubjid" %in% names(input)) "USUBJID" else "DOSEA"
      )

      updatePickerInput(
        session,
        "facetby",
        choices = param_choices_cf,
        selected = NULL
      )
    })

    # Render the cycle selection UI
    output$cycle_select_ui <- renderUI({
      req(input$param)
      y <- data() %>%
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

    # Return all inputs as a list of reactives
    inputs <- reactive({
      list(
        palette_theme = input$palette_theme,
        param = input$param,
        pcspec = input$pcspec,
        usubjid = input$usubjid,
        colorby = input$colorby,
        facetby = input$facetby,
        log = input$log,
        timescale = input$timescale,
        profiles = input$profiles,
        show_threshold = input$show_threshold,
        threshold_value = input$threshold_value,
        show_dose = input$show_dose,
        sd_max = input$sd_max,
        sd_min = input$sd_min,
        ci = input$ci
      )
    })
    inputs
  })
}
