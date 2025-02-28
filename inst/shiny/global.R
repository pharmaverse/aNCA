lapply(list.files("modules", pattern = "\\.R$", full.names = TRUE, recursive = TRUE), source)
lapply(list.files("functions", pattern = "\\.R$", full.names = TRUE, recursive = TRUE), source)

LABELS <- read.csv(system.file("shiny/data/adnca_labels.csv", package = "aNCA"))
