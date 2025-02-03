source("modules/tab_data.R")
#Load labels
LABELS <- read.csv(system.file("shiny/data/adnca_labels.csv", package = "aNCA"))

source("modules/nca_settings.R")

source("modules/slope_selector.R")

source("modules/units_table.R")

source("functions/partial_auc_input.R")

source("modules/tlg_plot.R")

source("modules/tab_visuals.R")

source("functions/mapping_selectize_inputs.R")
source("functions/generate_col_defs.R")
