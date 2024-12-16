# This script sources the server logic from the tabs folder
# Define server logic
function(input, output, session) {
  log_info("Startup")

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
  source(system.file("shiny/tabs/nca.R", package = "aNCA"), local = TRUE)
  # OUTPUT ----
  source(system.file("shiny/tabs/outputs.R", package = "aNCA"), local = TRUE)

  # TLG
  tab_tlg_server("tlg")
}
