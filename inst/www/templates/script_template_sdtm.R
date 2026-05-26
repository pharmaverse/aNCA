# Load the package (https://github.com/pharmaverse/aNCA)  #
###########################################################
## Please, set your working directory to this file:
# setwd("path/to/this/file/script_template_sdtm.R")

if (!require("aNCA")) install.packages("aNCA")
library(aNCA)
library(dplyr)

# Load SDTM domain data #
pc_data <- read_pk("../input_pc.rds")
ex_data <- read_pk("../input_ex.rds")
dm_data <- read_pk("../input_dm.rds")

## Apply column mapping from session settings ##################
# If the user mapped non-standard column names in the app,
# rename them back to expected SDTM names before conversion.
sdtm_mapping <- settings_list$mapping
sdtm_mapping <- sdtm_mapping[
  !names(sdtm_mapping) %in% c("Metabolites", "Grouping_Variables")
]

rename_cols <- function(df, mapping) {
  for (var in names(mapping)) {
    selected <- mapping[[var]]
    if (is.null(selected) || length(selected) != 1 || selected == "") next
    if (selected != var && selected %in% names(df)) {
      names(df)[names(df) == selected] <- var
    }
  }
  df
}

pc_data <- rename_cols(pc_data, sdtm_mapping)
ex_data <- rename_cols(ex_data, sdtm_mapping)
dm_data <- rename_cols(dm_data, sdtm_mapping)

## Create PKNCA object from SDTM domains ######################
metabolites <- settings_list$sdtm_metabolites

pknca_obj <- sdtm_to_PKNCAdata(
  pc = pc_data,
  ex = ex_data,
  dm = dm_data,
  metabolites = metabolites
)

## Setup NCA settings, intervals, parameter selections, and units
int_parameters <- settings_list$settings$int_parameters
units_table <- settings_list$units_table
parameters_selected_per_study <- settings_list$settings$parameters$selections
extra_vars_to_keep <- settings_list$extra_vars_to_keep
slope_rules <- settings_list$slope_rules

pknca_obj <- pknca_obj %>%
  PKNCA_update_data_object(
    method = settings_list$settings$method,
    selected_analytes = settings_list$settings$analyte,
    selected_profile = settings_list$settings$profile,
    selected_pcspec = settings_list$settings$pcspec,
    start_impute = settings_list$settings$data_imputation$impute_c0,
    exclusion_list = settings_list$settings$general_exclusions,
    hl_adj_rules = slope_rules,
    keep_interval_cols = setdiff(extra_vars_to_keep, c("DOSEA", "ATPTREF", "ROUTE")),
    min_hl_points = settings_list$settings$min_hl_points %||% 3,
    parameter_selections = parameters_selected_per_study,
    int_parameters = int_parameters,
    custom_units_table = units_table
  )

## Run NCA calculations ########################################
flag_rules <- settings_list$settings$flags
ratio_table <- settings_list$ratio_table
blq_rule <- settings_list$settings$data_imputation$blq_imputation_rule

pknca_res <- pknca_obj %>%

  # Run pk.nca and join subject and dose information to the results
  # Consider the BLQ imputation rule before calculations (if any)
  PKNCA_calculate_nca(
    blq_rule = blq_rule
  ) %>%

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

# Build flag rule exclusion messages for ADPP CRITy/CRITyFL/PPSUMFL columns
flag_operators <- c(R2ADJ = " < ", R2 = " < ", AUCPEO = " > ", AUCPEP = " > ", LAMZSPN = " < ")
checked_flags <- purrr::keep(flag_rules, function(x) x$is.checked)
flag_rule_msgs <- if (length(checked_flags) > 0) {
  vapply(names(checked_flags), function(nm) {
    paste0(nm, flag_operators[nm], checked_flags[[nm]]$threshold)
  }, character(1), USE.NAMES = FALSE)
} else {
  NULL
}

cdisc_datasets <- pknca_res %>%
  export_cdisc(grouping_vars = extra_vars_to_keep, flag_rules = flag_rule_msgs)

pivoted_results <- pivot_wider_pknca_results(
  myres = pknca_res,
  flag_rules = flag_rules,
  extra_vars_to_keep = extra_vars_to_keep
)
