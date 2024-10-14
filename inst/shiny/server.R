# This script sources the server logic from the tabs folder
# Define server logic
function(input, output, session) {
  # DATA ----
  source(system.file("shiny/tabs/data.R", package = "aNCA"), local = TRUE)
  # NCA ----
  source(system.file("shiny/tabs/nca.R", package = "aNCA"), local = TRUE)
  # OUTPUT ----
  source(system.file("shiny/tabs/outputs.R", package = "aNCA"), local = TRUE)
  # TLG ----  
  source(system.file("shiny/tabs/tlg.R", package = "aNCA"), local = TRUE)

}
