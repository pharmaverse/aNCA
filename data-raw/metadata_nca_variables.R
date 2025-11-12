# Load NCA variables dataset
metadata_nca_variables <- read.csv("data-raw/metadata_nca_variables.csv")
usethis::use_data(metadata_nca_variables, overwrite = TRUE)