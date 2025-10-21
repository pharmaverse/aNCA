#' !!! WARNING !!!
#' This script is not meant for stand-alone running.
#' It requires parsing against a Shiny session with userData containing reactive expressions.
#' It is used for generating the data pre-processing code for download in the app.
session <- readRDS("LASESSION.rds")

# Proper script description goes here.
library(aNCA)

# Load raw data #
data_path <- "inst/shiny/data/DummyRO_ADNCA.csv"
adnca_data <- read_pk(data_path)

## Preprocess data ########################################
mapping <- session$userData$mapping
names(mapping) <- gsub("select_", "", names(mapping))
applied_filters <- session$userData$applied_filters

preprocessed_adnca <- adnca_data %>%

  # Apply filters
  apply_filters(applied_filters) %>%

  # Map columns to their standards
  apply_mapping(
    mapping = mapping,
    desired_order =  c(
      "STUDYID", "USUBJID", "PARAM", "PCSPEC", "ATPTREF",
      "AVAL", "AVALU", "AFRLT", "ARRLT", "NRRLT", "NFRLT",
      "RRLTU", "ROUTE", "DOSETRT", "DOSEA", "DOSEU", "ADOSEDUR",
      "VOLUME", "VOLUMEU", "TRTRINT", "METABFL"
    ),
    silent = FALSE
  ) %>%

  # Derive METABFL column using PARAM metabolites
  create_metabfl(mapping$select_Metabolites)


## Setup NCA settings in the PKNCA object ########################
auc_data <- session$userData$settings$partial_aucs
units_table <- session$userData$units_table

pknca_obj <- preprocessed_adnca %>%
  
  # Create from ADNCA the PKNCA object
  PKNCA_create_data_object() %>%
  
  # Setup basic settings
  PKNCA_update_data_object(
    auc_data = auc_data,
    method = session$userData$settings$method,
    selected_analytes = session$userData$settings$analyte,
    selected_profile = session$userData$settings$profile,
    selected_pcspec = session$userData$settings$pcspec,
    params = session$userData$settings$parameter_selection,
    # hl_adj_rules = RULES
    should_impute_c0 = session$userData$settings$data_imputation$impute_c0
  ) %>% 

  # Define the desired units for the parameters (PPSTRESU)
  {
    pknca_obj <- .
    pknca_obj[["units"]] <- units_table
    pknca_obj
  }

## Run NCA calculations ########################################
#ratios_table <- session$userData$ratios_table
flag_rules <- session$userData$settings$flags

pknca_res <- pknca_obj %>%
  # Run pk.nca and join subject and dose information to the results
  PKNCA_calculate_nca()  %>%

  # Add bioavailability results if requested
  add_f_to_pknca_results(session$userData$settings$bioavailability) %>%

  # Apply standard CDISC names
  mutate(PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD")) %>%

  # Derive secondary parameters (ratio parameters)
  #calculate_table_ratios_app() %>%

  # Flag relevant parameters based on AUCPEO, AUCPEP & lambda span
  PKNCA_hl_rules_exclusion(
    rules = isolate(flag_rules) |>
      purrr::keep(\(x) x$is.checked) |>
      purrr::map(\(x) x$threshold)
  )

## Obtain PP, ADPP, ADNCA & Pivoted results #########################
cdisc_datasets <- export_cdisc(pknca_res)
pivoted_results <- pivot_wider_pknca_results(pknca_res)
