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
    width = "375px",
    selectInput(
      ns("palette"),
      "Select Color Theme:",
      choices = c(
        "Default (ggplot2)" = "default",
        "Plasma" = "plasma",
        "Cividis" = "cividis",
        "Inferno" = "inferno"
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
      inputId = ns("profiles"),
      label = "Choose the profile(s):",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
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
    checkboxInput(ns("show_threshold"), label = "Show Threshold"),
    conditionalPanel(
      condition = "input.show_threshold == true",
      numericInput(ns("threshold_value"), label = "Threshold Value", value = NULL),
      ns = ns
    ),
    checkboxInput(ns("show_dose"), label = "Show Dose Times"),
    div(
      style = "display:flex; gap:8px;",
      div(style = "flex:1;", numericInput(ns("x_min"), label = "X Min", value = NULL)),
      div(style = "flex:1;", numericInput(ns("x_max"), label = "X Max", value = NULL))
    ),
    div(
      style = "display:flex; gap:8px;",
      div(style = "flex:1;", numericInput(ns("y_min"), label = "Y Min", value = NULL)),
      div(style = "flex:1;", numericInput(ns("y_max"), label = "Y Max", value = NULL))
    ),
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

      profile_choices <- data %>%
        mutate(ATPTREF = as.character(ATPTREF)) %>%
        pull(ATPTREF) %>%
        unique()

      updatePickerInput(
        session,
        "profiles",
        choices = profile_choices,
        selected = profile_choices
      )

      full_grouping_vars <- unique(c(conc_groups, dose_groups,
                                     dose_col, grouping_vars(), "ATPTREF"))

      is_individual <- "usubjid" %in% names(input)

      # Default color_by: USUBJID for individual plots, first available
      # variable from the priority list for mean plots
      default_color <- if (is_individual) {
        subject_col
      } else {
        color_priority <- c("TRT01A", "GROUP", "ACTARM", "COHORT", "DOSEA")
        available_color <- intersect(color_priority, full_grouping_vars)
        if (length(available_color) > 0) available_color[1] else dose_col
      }

      updatePickerInput(
        session,
        "colorby",
        choices = full_grouping_vars,
        selected = default_color
      )

      # Default facet_by: for mean plots, first available from priority list;
      # for individual plots, no default facet
      default_facet <- if (!is_individual) {
        facet_priority <- c("TRT01A", "DOSEA", "GROUP", "ACTARM", "COHORT")
        match <- intersect(facet_priority, full_grouping_vars)
        if (length(match) > 0) match[1] else NULL
      }

      updatePickerInput(
        session,
        "facetby",
        choices = full_grouping_vars,
        selected = default_facet
      )
    })

    # Create a reactive value to store the filtering list
    # based on PARAM, PCSPEC, USUBJID, and ATPTREF selections
    filtering_list <- reactiveVal(list())
    observeEvent(
      list(input$param, input$pcspec, input$usubjid, input$profiles),
      {
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
        filtering_list(lst)
      },
      ignoreNULL = FALSE
    )

    # Return all inputs as a list of reactives
    reactive({
      list(
        palette = input$palette,
        color_by = input$colorby,
        facet_by = input$facetby,
        ylog_scale = input$log,
        threshold_value = input$threshold_value,
        show_dose = input$show_dose,
        x_limits = c(input$x_min, input$x_max),
        y_limits = c(input$y_min, input$y_max),
        sd_max = input$sd_max,
        sd_min = input$sd_min,
        ci = input$ci,
        filtering_list = filtering_list(),
        use_time_since_last_dose = input$timescale == "By Dose Profile"
      )
    })
  })
}
