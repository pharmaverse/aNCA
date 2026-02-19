# Load NCA parameters dataset
adnca_example <- read.csv("data-raw/metadata_nca_parameters.csv")
usethis::use_data(adnca_example, overwrite = TRUE)
