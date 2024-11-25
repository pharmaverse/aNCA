# This script sources the server logic from the tabs folder
# Define server logic
function(input, output, session) {
  log_info("Startup")

  # DATA ----
  data <- tab_data_server("data")
  # NCA ----
  source(system.file("shiny/tabs/nca.R", package = "aNCA"), local = TRUE)
  # OUTPUT ----
  tab_visuals_server("visuals", data, res_nca)

  # TLG
  tab_tlg_server("tlg", data)
}
