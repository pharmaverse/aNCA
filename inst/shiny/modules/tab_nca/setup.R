setup_ui <- function(id) {
  ns <- NS(id)

  navset_pill_list(
    nav_panel("Summary", summary_ui(ns("nca_setup_summary"))),
    nav_panel("Settings", settings_ui(ns("nca_settings"))),
    nav_panel("Slope Selector", slope_selector_ui(ns("slope_selector")))
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

    processed_pknca_data <- settings$processed_pknca_data
    slopes_pknca_data <- settings$slopes_pknca_data
    settings_rules <- settings$rules
    f_auc_options <- settings$bioavailability

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
