# This script sources the server logic from the tabs folder
# Define server logic
function(input, output, session) {
  log_info("Startup")

  # Initially disable all tabs except the 'Data' tab
  shinyjs::disable(selector = "#page li a[data-value=nca]")
  shinyjs::disable(selector = "#page li a[data-value=visualisation]")
  shinyjs::disable(selector = "#page li a[data-value=tlg]")

  # DATA ----
  data_module <- tab_data_server("data")
  # Data set for analysis
  data <- data_module$data
  #' Create global data object. This is accessible by all modules, without the need to pass
  #' data reactive directly.
  session$userData$data <- reactive(data())
  # Grouping Variables
  grouping_vars <- data_module$grouping_variables

  # NCA ----
  res_nca <- tab_nca_server("nca", data, grouping_vars)
  # VISUALISATION ----
  tab_visuals_server("visuals", data, grouping_vars, res_nca)

  # TLG
  tab_tlg_server("tlg")
}
