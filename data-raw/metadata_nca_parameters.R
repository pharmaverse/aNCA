# Load NCA parameters dataset
metadata_nca_parameters <- read.csv("data-raw/metadata_nca_parameters.csv")
usethis::use_data(metadata_nca_parameters, overwrite = TRUE)