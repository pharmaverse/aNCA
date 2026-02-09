# Load the package (https://github.com/pharmaverse/aNCA)  #
###########################################################
## Please, set your working directory to this file:
# setwd("path/to/this/file/script_template.R")

if (!require("aNCA")) install.packages("aNCA")
library(aNCA)
library(dplyr)

# Load raw data #
data_path <- "../data/data.rds"
adnca_data <- read_pk(data_path)

## Preprocess data ########################################
mapping <- yaml_settings$mapping
names(mapping) <- gsub("select_", "", names(mapping))
applied_filters <- yaml_settings$applied_filters

preprocessed_adnca <- adnca_data %>%

  # Filter the data
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
  create_metabfl(mapping$Metabolites) %>%
  
  # Make sure all variables are in its correct class
  adjust_class_and_length(metadata_nca_variables)

## Setup NCA settings in the PKNCA object ########################
auc_data <- yaml_settings$settings$partial_aucs
units_table <- yaml_settings$final_units
parameters_selected_per_study <- yaml_settings$settings$parameters$selections
study_types_df <- yaml_settings$settings$parameters$types_df
extra_vars_to_keep <-  yaml_settings$extra_vars_to_keep
slope_rules <- yaml_settings$slope_rules

pknca_obj <- preprocessed_adnca %>%

  # Create from ADNCA the PKNCA object
  PKNCA_create_data_object(
    nca_exclude_reason_columns = c("DTYPE", mapping$NCAwXRS)
  ) %>%

  # Setup basic settings
  PKNCA_update_data_object(
    method = yaml_settings$settings$method,
    selected_analytes = yaml_settings$settings$analyte,
    selected_profile = yaml_settings$settings$profile,
    selected_pcspec = yaml_settings$settings$pcspec,
    should_impute_c0 = yaml_settings$settings$data_imputation$impute_c0,
    exclusion_list = yaml_settings$settings$general_exclusions,
    hl_adj_rules = slope_rules,
    keep_interval_cols = setdiff(extra_vars_to_keep, c("DOSEA", "ATPTREF", "ROUTE"))
  ) %>%

  update_main_intervals(
    auc_data = auc_data,
    parameter_selections = parameters_selected_per_study,
    study_types_df =  study_types_df,
    impute = yaml_settings$settings$data_imputation$impute_c0
  ) %>%

  # Define the desired units for the parameters (PPSTRESU)
  {
    pknca_obj <- .
    if (!is.null(units_table)) {
      pknca_obj[["units"]] <- units_table
    }
    pknca_obj
  }

## Run NCA calculations ########################################
flag_rules <- yaml_settings$settings$flags
ratio_table <- yaml_settings$ratio_table
blq_rule <- yaml_settings$settings$data_imputation$blq_imputation_rule

pknca_res <- pknca_obj %>%

  # Run pk.nca and join subject and dose information to the results
  # Consider the BLQ imputation rule before calculations (if any)
  PKNCA_calculate_nca(
    blq_rule = blq_rule
  ) %>%

  # Add bioavailability results if requested
  add_f_to_pknca_results(yaml_settings$settings$bioavailability) %>%

  # Apply standard CDISC names
  mutate(PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD")) %>%

  # Flag relevant parameters based on AUCPEO, AUCPEP & lambda span
  PKNCA_hl_rules_exclusion(
    rules = flag_rules %>%
      purrr::keep(\(x) x$is.checked) %>%
      purrr::map(\(x) x$threshold)
  ) %>%

  # Derive secondary parameters (ratio parameters)
  calculate_table_ratios(ratio_table) %>%
  
  # Filter only parameters that have been requested
  remove_pp_not_requested()

## Obtain PP, ADPP, ADNCA & Pivoted results #########################
cdisc_datasets <- pknca_res %>%
  export_cdisc()

pivoted_results <- pivot_wider_pknca_results(
  myres = pknca_res,
  flag_rules = flag_rules,
  extra_vars_to_keep = extra_vars_to_keep
)
