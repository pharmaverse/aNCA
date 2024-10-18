# This script sources the server logic from the tabs folder
# Define server logic
function(input, output, session) {
  # DATA ----
  data <- tab_data_server("data")
  # NCA ----
  source(system.file("shiny/tabs/nca.R", package = "aNCA"), local = TRUE)
  # OUTPUT ----
  source(system.file("shiny/tabs/outputs.R", package = "aNCA"), local = TRUE)
  # shiny functions #
  system.file("shiny/functions", package = "aNCA") |>
    list.files(full.names = TRUE) |>
    sapply(source)
}
