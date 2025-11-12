# Load NCA parameters dataset
metadata_nca_parameters <- read.csv("data-raw/metadata_nca_variables.csv")
usethis::use_data(metadata_nca_variables, overwrite = TRUE)