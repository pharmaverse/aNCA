# Load NCA parameters dataset
adnca_example <- read.csv("data-raw/adnca_example.csv", na.strings = c("", "NA"))
usethis::use_data(adnca_example, overwrite = TRUE)
