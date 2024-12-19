# This script sources the server logic from the tabs folder
# Define server logic
function(input, output, session) {
  log_info("Startup")

  # DATA ----
  data_module <- tab_data_server("data")
  # Data set for analysis
  data <- data_module$data
  # Grouping Variables
  grouping_vars <- data_module$grouping_variables
  # NCA ----
  source(system.file("shiny/tabs/nca.R", package = "aNCA"), local = TRUE)
  # OUTPUT ----
  tab_visuals_server("visuals", data, grouping_vars, res_nca)

  # TLG
  tab_tlg_server("tlg", data)
}
