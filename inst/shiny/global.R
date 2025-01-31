lapply(list.files("modules", pattern = "\\.R$", full.names = TRUE), source)
lapply(list.files("functions", pattern = "\\.R$", full.names = TRUE), source)

LABELS <- read.csv(system.file("shiny/data/adnca_labels.csv", package = "aNCA"))