library(dplyr)

# Build adnca_example dataset from CSV
multian_test <- read.csv("data-raw/adnca_example.csv", na.strings = c("", "NA")) %>%
  mutate(DOSFRM = ifelse(ROUTE == "ORAL", "TABLET", "INJECTION, SOLUTION")) %>%
  # Provide different units for the metabolite
  mutate(
    AVAL = ifelse(PARAM == "Metab-DrugA" & AVALU == "ug/mL", AVAL / 1000, AVAL),
    AVALU = ifelse(PARAM == "Metab-DrugA" & AVALU == "ug/mL", "mg/mL", AVALU)
  )
write.csv(multian_test, "tests/testthat/data/test-multianalyte-ADNCA.csv", row.names = FALSE)
