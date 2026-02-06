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
        downloadButton(
          ns("settings_download"),
          label = "Download settings"
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
    nav_panel("Slope Selector", slope_selector_ui(ns("slope_selector"))),
    nav_panel("General Exclusions", general_exclusions_ui(ns("general_exclusions")))
  )
}

setup_server <- function(id, data, adnca_data, extra_group_vars, settings_override) {
  moduleServer(id, function(input, output, session) {

    imported_settings <- reactive(settings_override()$settings)
    imported_slopes <- reactive(settings_override()$slope_rules)
    imported_params <- reactive(imported_settings()$parameter_selections)
    general_excl_override <- reactive(imported_settings()$general_exclusions)

    # Gather all settings from the appropriate module
    settings <- settings_server(
      "nca_settings",
      data,
      adnca_data,
      imported_settings
    )

    general_exclusions <- general_exclusions_server(
      "general_exclusions",
      processed_pknca_data,
      general_excl_override
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
        should_impute_c0 = settings()$data_imputation$impute_c0,
        hl_adj_rules = slope_rules$manual_slopes(),
        exclusion_list = general_exclusions(),
        keep_interval_cols = extra_group_vars()
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
      imported_params
    )

    final_settings <- reactive({

      req(settings(), parameters_output$selections(), general_exclusions())

      current_settings <- settings()
      current_settings$general_exclusions <- general_exclusions()
      current_settings$parameters <- list(
        selections = parameters_output$selections(),
        types_df = parameters_output$types_df()
      )
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
        impute = settings()$data_imputation$impute_c0,
        blq_imputation_rule = settings()$data_imputation$blq_imputation_rule
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
      adnca_data = processed_pknca_data,
      extra_group_vars = extra_group_vars
    )

    # Automatically update the units table when settings are uploaded.
    observeEvent(imported_settings(), {
      req(imported_settings()$units)
      session$userData$units_table(imported_settings()$units)
    })

    # Parameter unit changes option: Opens a modal message with a units table to edit
    units_table_server("units_table", processed_pknca_data)

    # Collect all half life manual adjustments done in the `Slope Selector` section
    # and controls the half life plots that are displayed
    slope_rules <- slope_selector_server(
      "slope_selector",
      processed_pknca_data,
      imported_slopes
    )

    # Handle downloading and uploading settings
    output$settings_download <- downloadHandler(
      filename = function() {
        paste0(session$userData$project_name(), "_settings_", Sys.Date(), ".yaml")
      },
      content = function(con) {
        # Prepare the list
        export_list <- list(
          settings = final_settings(),
          slope_rules = slope_rules()
        )
        # write yaml file
        yaml::write_yaml(export_list, file = con)
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
