#' NCA Setup Server Module
#'
#' This module handles the server-side logic for the NCA settings, including file uploads,
#' analyte/dose/specimen selection, AUC intervals, NCA parameters, as well as slope selection and
#' their unit specification. Then it integrates them to adnca_data object and returns data
#' that is processed accordingly.
#'
#' @param id ID of the module.
#' @param data Reactive with data table containing raw data uploaded to the app.
#' @param adnca_data Reactive with `PKNCAdata` object including the `adnca_data`.
#'
#' @returns List with three reactive expressions:
#'   * processed_pknca_data - PKNCAdata object with applied settings and slope selections.
#'   * settings - List with raw settings as gathered by the module.
#'   * slope_rules - Data frame with slope inclusions / exclusions provided by slope selector.

setup_ui <- function(id) {
  ns <- NS(id)

  navset_pill_list(
    nav_panel(
      "Settings",
      fluidRow(
        column(6,
          fileInput(
            ns("settings_upload"),
            width = "100%",
            label = "Upload settings",
            buttonLabel = list(icon("folder"), "Browse"),
            accept = ".rds"
          )
        ),
        column(6,
          downloadButton(
            ns("settings_download"),
            label = "Download settings"
          )
        )
      ),
      fluidRow(units_table_ui(ns("units_table"))),
      settings_ui(ns("nca_settings")),
      accordion(
        accordion_panel(
          title = "Ratio Calculations",
          ratios_table_ui(ns("ratio_calculations_table"))
        ),
        open = c("General Settings", "Parameter Selection")
      )
    ),
    nav_panel("Parameter Selection", parameter_selection_ui(ns("nca_setup_parameter"))),
    nav_panel("Slope Selector", slope_selector_ui(ns("slope_selector")))
  )
}

setup_server <- function(id, data, adnca_data) {
  moduleServer(id, function(input, output, session) {

    imported_settings <- reactive({
      req(input$settings_upload)
      readRDS(input$settings_upload$datapath)
    })

    settings_override <- reactive(imported_settings()$settings)
    manual_slopes_override <- reactive(imported_settings()$slope_rules)
    parameters_override <- reactive(imported_settings()$settings$parameter_selections)

    # Gather all settings from the appropriate module
    settings <- settings_server(
      "nca_settings",
      data,
      adnca_data,
      settings_override
    )

    # Create processed data object with applied settings.
    base_pknca_data <- reactive({
      req(adnca_data(), settings())
      log_trace("Updating PKNCA::data object.")

      base_pknca_data <- PKNCA_update_data_object(
        adnca_data = adnca_data(),
        method = settings()$method,
        selected_analytes = settings()$analyte,
        selected_profile = settings()$profile,
        selected_pcspec = settings()$pcspec,
        should_impute_c0 = settings()$data_imputation$impute_c0
      )

      # Show bioavailability widget if it is possible to calculate
      if (base_pknca_data$dose$data$std_route %>% unique() %>% length() == 2) {
        shinyjs::show(selector = ".bioavailability-picker")
      } else {
        shinyjs::hide(selector = ".bioavailability-picker")
      }

      base_pknca_data
    })

    parameters_output <- parameter_selection_server(
      "nca_setup_parameter",
      base_pknca_data,
      parameters_override
    )

    final_settings <- reactive({
      req(settings(), parameters_output$selections())

      current_settings <- settings()
      current_settings$parameter_selections <- parameters_output$selections()

      current_settings
    })

    # Update intervals using summary output
    processed_pknca_data <- reactive({
      req(base_pknca_data(), parameters_output$selections(), parameters_output$types_df())

      final_data <- base_pknca_data()

      # Call the updated function with the direct inputs
      final_data <- update_main_intervals(
        data = base_pknca_data(),
        parameter_selections = parameters_output$selections(),
        study_types_df = parameters_output$types_df(),
        auc_data = settings()$partial_aucs,
        impute = settings()$data_imputation$impute_c0
      )

      if (nrow(final_data$intervals) == 0) {
        showNotification(
          "All intervals were filtered. Please revise your settings",
          type = "warning",
          duration = 10
        )
      }

      final_data
    })

    # Keep the post processing ratio calculations requested by the user
    ratio_table <- ratios_table_server(
      id = "ratio_calculations_table",
      adnca_data = processed_pknca_data
    )
    session$userData$ratio_table <- reactive(ratio_table())

    # Automatically update the units table when settings are uploaded.
    observeEvent(settings_override(), {
      req(settings_override()$units)
      session$userData$units_table(settings_override()$units)
    })

    # Parameter unit changes option: Opens a modal message with a units table to edit
    units_table_server("units_table", processed_pknca_data)

    # Create version for slope plots
    # Only parameters required for the slope plots are set in intervals
    # NCA dynamic changes/filters based on user selections
    slopes_pknca_data <- reactive({
      req(adnca_data(), settings(), settings()$profile,
          settings()$analyte, settings()$pcspec)
      log_trace("Updating PKNCA::data object for slopes.")

      df <- PKNCA_update_data_object(
        adnca_data = adnca_data(),
        method = settings()$method,
        selected_analytes = settings()$analyte,
        selected_profile = settings()$profile,
        selected_pcspec = settings()$pcspec,
        should_impute_c0 = settings()$data_imputation$impute_c0
      )

      params <- c("lambda.z.n.points", "lambda.z.time.first",
                  "r.squared", "adj.r.squared", "tmax")

      df$intervals <- df$intervals %>%
        mutate(across(any_of(params), ~ TRUE, .names = "{.col}"),
               impute = NA)

      df
    })

    slope_rules <- slope_selector_server(
      "slope_selector",
      slopes_pknca_data,
      manual_slopes_override
    )

    # Handle downloading and uploading settings
    output$settings_download <- downloadHandler(
      filename = function() {
        paste0(session$userData$project_name(), "_settings_", Sys.Date(), ".rds")
      },
      content = function(con) {
        saveRDS(list(settings = final_settings(), slope_rules = slope_rules$manual_slopes()), con)
      }
    )

    list(
      processed_pknca_data = processed_pknca_data,
      settings = final_settings,
      ratio_table = ratio_table,
      slope_rules = slope_rules
    )
  })
}
