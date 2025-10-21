#' !!! WARNING !!!
#' This script is not meant for stand-alone running.
#' It requires parsing against a Shiny session with userData containing reactive expressions.
#' It is used for generating the data pre-processing code for download in the app.

# Proper script description goes here.
library(aNCA)

# Load raw data #
data_path <- NULL

adnca_data <- read_pk(data_path)
if (is.null(data_path)) stop("Please provide a data path within the script")

# Apply filters #
applied_filters <- deparse1(session$userData$applied_filters())
filtered_data <- apply_filters(adnca_data, applied_filters)

# Map data to required columns #
mapping <- deparse1(session$userData$mapping())

MANUAL_UNITS <- list(
  concentration = c(
    "mg/L", "µg/mL", "ng/mL", "pg/mL", "mol/L", "mmol/L", "µmol/L", "nmol/L", "pmol/L", "mg/dL",
    "µg/dL", "ng/dL"
  ),
  dose = c(
    "mg", "g", "µg", "ng", "pg", "mol", "mmol", "µmol", "nmol", "pmol", "mg/kg", "g/kg", "µg/kg",
    "ng/kg", "pg/kg", "mol/kg", "mmol/kg", "µmol/kg", "nmol/kg", "pmol/kg"
  ),
  time = c("sec", "min", "hr", "day", "week", "month", "year")
)

# Define the required columns and group them into categories
MAPPING_COLUMN_GROUPS <- list(
  "Group Identifiers" = c("STUDYID", "USUBJID", "ATPTREF"),
  "Sample Variables" = c("PARAM", "PCSPEC", "ROUTE", "AVAL"),
  "Dose Variables" = c("DRUG", "DOSEA"),
  "Time Variables" = c("AFRLT", "ARRLT", "NFRLT", "NRRLT"),
  "Unit Variables" = c("AVALU", "DOSEU", "RRLTU"),
  "Supplemental Variables" = c("Grouping_Variables", "TAU", "ADOSEDUR", "VOLUME", "VOLUMEU")
)

# Define the desired column order
MAPPING_DESIRED_ORDER <- c(
  "STUDYID", "USUBJID", "PARAM", "PCSPEC", "ATPTREF",
  "AVAL", "AVALU", "AFRLT", "ARRLT", "NRRLT", "NFRLT",
  "RRLTU", "ROUTE", "DRUG", "DOSEA", "DOSEU", "ADOSEDUR",
  "VOLUME", "VOLUMEU", "TAU"
)

mapped_data <- apply_column_mapping(
  filtered_data,
  mapping,
  MANUAL_UNITS,
  MAPPING_COLUMN_GROUPS,
  MAPPING_DESIRED_ORDER
)

# Create PKNCA data object #
pknca_object <- PKNCA_create_data_object(mapped_data)

saveRDS(pknca_object, file = "pknca_data.rds")
write.csv(mapped_data, file = "processed_data.csv", row.names = FALSE)
