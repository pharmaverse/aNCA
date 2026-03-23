# Build adnca_example dataset from CSV
adnca_example <- read.csv("data-raw/adnca_example.csv", na.strings = c("", "NA")) %>%
  mutate(DOSEFRM = ifelse(ROUTE == "ORAL", "TABLET", "INJECTION, SOLUTION"))
usethis::use_data(adnca_example, overwrite = TRUE)
