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
        id = "acc",
        open = c("General Settings", "Parameter Selection")
      )
    ),
    nav_panel("Slope Selector", slope_selector_ui(ns("slope_selector"))),
    nav_panel("Summary", summary_ui(ns("nca_setup_summary")))
  )
}

setup_server <- function(id, data, adnca_data) {
  moduleServer(id, function(input, output, session) {
    # Gather all settings from the appropriate module
    settings <- settings_server("nca_settings", data, adnca_data, settings_override)

    # Create processed data object with applied settings.
    processed_pknca_data <- reactive({
      req(adnca_data(), settings())
      log_trace("Updating PKNCA::data object.")

      processed_pknca_data <- PKNCA_update_data_object(
        adnca_data = adnca_data(),
        auc_data = settings()$partial_aucs,
        method = settings()$method,
        selected_analytes = settings()$analyte,
        selected_profile = settings()$profile,
        selected_pcspec = settings()$pcspec,
        params = settings()$parameter_selection,
        should_impute_c0 = settings()$data_imputation$impute_c0
      )

      # Show bioavailability widget if it is possible to calculate
      if (processed_pknca_data$dose$data$std_route %>% unique() %>% length() == 2) {
        shinyjs::show(selector = ".bioavailability-picker")
      } else {
        shinyjs::hide(selector = ".bioavailability-picker")
      }

      if (nrow(processed_pknca_data$intervals) == 0) {
        showNotification(
          "All intervals were filtered. Please revise your settings",
          type = "warning",
          duration = 10
        )
      }

      processed_pknca_data
    })

    # Keep the post processing ratio calculations requested by the user
    ratio_table <- ratios_table_server(
      id = "ratio_calculations_table",
      adnca_data = processed_pknca_data
    )

    # Parameter unit changes option: Opens a modal message with a units table to edit
    units_table_server("units_table", processed_pknca_data)

    # Create version for slope plots
    # Only parameters required for the slope plots are set in intervals
    # NCA dynamic changes/filters based on user selections
    slopes_pknca_data <- reactive({
      req(adnca_data(), settings(), settings()$profile,
          settings()$analyte, settings()$pcspec)
      log_trace("Updating PKNCA::data object for slopes.")

      PKNCA_update_data_object(
        adnca_data = adnca_data(),
        auc_data = settings()$partial_aucs,
        method = settings()$method,
        selected_analytes = settings()$analyte,
        selected_profile = settings()$profile,
        selected_pcspec = settings()$pcspec,
        params = c("lambda.z.n.points", "lambda.z.time.first",
                   "r.squared", "adj.r.squared", "tmax"),
        should_impute_c0 = settings()$data_imputation$impute_c0
      )
    })

    slope_rules <- slope_selector_server(
      "slope_selector",
      slopes_pknca_data,
      manual_slopes_override
    )

    summary_server("nca_setup_summary", processed_pknca_data)

    # Handle downloading and uploading settings
    output$settings_download <- downloadHandler(
      filename = paste0("aNCA_settings_", Sys.Date(), ".rds"),
      content = function(con) {
        saveRDS(list(settings = settings(), slope_rules = slope_rules$manual_slopes()), con)
      }
    )

    imported_settings <- reactive({
      req(input$settings_upload)
      readRDS(input$settings_upload$datapath)
    })

    settings_override <- reactive(imported_settings()$settings)
    manual_slopes_override <- reactive(imported_settings()$slope_rules)

    list(
      processed_pknca_data = processed_pknca_data,
      settings = settings,
      ratio_table = ratio_table,
      slope_rules = slope_rules
    )
  })
}
