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
            accept = c(".csv", ".xpt")
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
      settings_ui(ns("nca_settings"))
    ),
    nav_panel("Slope Selector", slope_selector_ui(ns("slope_selector"))),
    nav_panel("Summary", summary_ui(ns("nca_setup_summary")))
  )
}

#' NCA Settings Server Module
#'
#' This module handles the server-side logic for the NCA settings, including file uploads,
#' analyte/dose/specimen selection, AUC intervals, NCA parameters and their unit specifications.
#' Then it integrates them to adnca_data and updates the object.
#'
#' - id The module's ID.
#' - data A reactive expression containing the read and mapped data from the app.
#'        It is only used for the file uploads and the analyte/dose/specimen selection.
#' - adnca_data A reactive expression of the PKNCAdata object,
#'  which contains data and NCA specifications.
setup_server <- function(id, data, adnca_data, res_nca) {
  moduleServer(id, function(input, output, session) {
    settings <- settings_server("nca_settings", data, adnca_data)

    setup <- settings$all_settings
    processed_pknca_data <- settings$processed_pknca_data
    settings_rules <- settings$rules
    f_auc_options <- settings$bioavailability

    # Parameter unit changes option: Opens a modal message with a units table to edit
    units_table_server("units_table", processed_pknca_data)

    # Create version for slope plots
    # Only parameters required for the slope plots are set in intervals
    # NCA dynamic changes/filters based on user selections
    slopes_pknca_data <- reactive({
      req(adnca_data(), setup())
      log_trace("Updating PKNCA::data object for slopes.")

      PKNCA_update_data_object(
        adnca_data = adnca_data(),
        auc_data = setup()$partial_aucs,
        method = setup()$method,
        selected_analytes = setup()$analyte,
        selected_dosno = setup()$doseno,
        selected_pcspec = setup()$pcspec,
        params = c("lambda.z.n.points", "lambda.z.time.first",
                   "r.squared", "adj.r.squared", "tmax"),
        should_impute_c0 = setup()$data_imputation$impute_c0
      )
    })

    slope_rules <- slope_selector_server(
      "slope_selector",
      slopes_pknca_data,
      res_nca,
      reactive(input$settings_upload)
    )

    summary_server("nca_setup_summary", processed_pknca_data)

    list(
      processed_pknca_data = processed_pknca_data,
      slopes_pknca_data = slopes_pknca_data,
      rules = settings_rules,
      bioavailability = f_auc_options,
      slope_rules = slope_rules
    )
  })
}
