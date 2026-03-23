# Build adnca_example dataset from CSV
adnca_example <- read.csv("data-raw/adnca_example.csv", na.strings = c("", "NA")) %>%
  mutate(FORMULATION = paste0(ROUTE, " OF ", DOSETRT))
usethis::use_data(adnca_example, overwrite = TRUE)
