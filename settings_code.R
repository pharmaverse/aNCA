# Load the package (https://github.com/pharmaverse/aNCA)  #
###########################################################
## Please, set your working directory to this file:
# setwd("path/to/this/file/script_template.R")

if (!require("aNCA")) install.packages("aNCA")
library(aNCA)
library(dplyr)

# Load raw data #
data_path <- "data_raw/adnca_example.csv"
adnca_data <- read_pk(data_path)

## Preprocess data & create PKNCA object ######################
mapping <- list(
  select_STUDYID = "STUDYID",
  select_USUBJID = "USUBJID",
  select_DOSEA = "DOSEA",
  select_DOSEU = "DOSEU",
  select_DOSETRT = "DOSETRT",
  select_PARAM = "PARAM",
  select_Metabolites = "Metab-DrugA",
  select_ARRLT = "ARRLT",
  select_NRRLT = "NRRLT",
  select_AFRLT = "AFRLT",
  select_NCAwXRS = c("NCA1XRS", "NCA2XRS"),
  select_NFRLT = "NFRLT",
  select_PCSPEC = "PCSPEC",
  select_ROUTE = "ROUTE",
  select_TRTRINT = "TRTRINT",
  select_ADOSEDUR = "ADOSEDUR",
  select_Grouping_Variables = c("TRT01A", "RACE", "SEX"),
  select_RRLTU = "RRLTU",
  select_VOLUME = "VOLUME",
  select_VOLUMEU = "VOLUMEU",
  select_AVAL = "AVAL",
  select_AVALU = "AVALU",
  select_ATPTREF = "ATPTREF"
)
names(mapping) <- gsub("select_", "", names(mapping))
applied_filters <- NULL
time_duplicate_rows <- NULL

int_parameters <- data.frame(
  parameter = c("AUCINT", "AUCINT"),
  start_auc = c(NA, NA),
  end_auc = c(NA, NA)
)
units_table <- NULL
parameters_selected_per_study <- list(
  "Multiple IV Bolus" = c(
    "adj.r.squared", "auclast", "auclast.dn", "clast.obs", "clast.obs.dn", "cmax", "cmax.dn", "half.life", "lambda.z", "lambda.z.n.points", "lambda.z.time.first", "r.squared", "span.ratio", "tlast", "tmax",
    "vss.obs"
  ),
  "Multiple IV Bolus (Metabolite)" = c("adj.r.squared", "auclast", "clast.obs", "cmax", "half.life", "lambda.z", "lambda.z.n.points", "lambda.z.time.first", "r.squared", "span.ratio", "tlast", "tmax"),
  "Multiple IV Infusion" = c(
    "adj.r.squared", "auclast", "auclast.dn", "clast.obs", "clast.obs.dn", "cmax", "cmax.dn", "half.life", "lambda.z", "lambda.z.n.points", "lambda.z.time.first", "r.squared", "span.ratio", "tlast", "tmax",
    "vss.obs"
  ),
  "Multiple IV Infusion (Metabolite)" = c("adj.r.squared", "auclast", "clast.obs", "cmax", "half.life", "lambda.z", "lambda.z.n.points", "lambda.z.time.first", "r.squared", "span.ratio", "tlast", "tmax")
)
study_types_df <- list(
  USUBJID = c(
    "S1-01", "S1-01", "S1-02", "S1-02", "S1-03", "S1-03", "S1-04", "S1-04", "S1-05", "S1-05", "S1-06", "S1-06", "S1-07", "S1-07", "S1-08",
    "S1-08"
  ),
  PARAM = c(
    "DrugA", "Metab-DrugA", "DrugA", "Metab-DrugA", "DrugA", "Metab-DrugA", "DrugA", "Metab-DrugA", "DrugA", "Metab-DrugA", "DrugA", "Metab-DrugA", "DrugA", "Metab-DrugA", "DrugA",
    "Metab-DrugA"
  ),
  std_route = c(rep("intravascular", 16)),
  type = c(
    "Multiple IV Infusion", "Multiple IV Infusion (Metabolite)", "Multiple IV Infusion", "Multiple IV Infusion (Metabolite)", "Multiple IV Infusion", "Multiple IV Infusion (Metabolite)", "Multiple IV Infusion", "Multiple IV Infusion (Metabolite)", "Multiple IV Infusion", "Multiple IV Infusion (Metabolite)", "Multiple IV Bolus", "Multiple IV Bolus (Metabolite)", "Multiple IV Bolus", "Multiple IV Bolus (Metabolite)", "Multiple IV Bolus",
    "Multiple IV Bolus (Metabolite)"
  )
)
extra_vars_to_keep <-  NULL
slope_rules <- data.frame()

pknca_obj <- adnca_data %>%

  # Preprocess raw data and create the PKNCA object
  PKNCA_create_data_object(
    mapping = mapping,
    applied_filters = applied_filters,
    time_duplicate_rows = time_duplicate_rows
  ) %>%

  # Setup basic settings
  PKNCA_update_data_object(
    method = "lin up/log down",
    selected_analytes = c("DrugA", "Metab-DrugA"),
    selected_profile = "DOSE 1",
    selected_pcspec = "SERUM",
    should_impute_c0 = TRUE,
    exclusion_list = list(),
    hl_adj_rules = slope_rules,
    keep_interval_cols = setdiff(extra_vars_to_keep, c("DOSEA", "ATPTREF", "ROUTE"))
  ) %>%

  update_main_intervals(
    int_parameters = int_parameters,
    parameter_selections = parameters_selected_per_study,
    study_types_df =  study_types_df,
    impute = TRUE
  ) %>%

  # Define the desired units for the parameters (PPSTRESU)
  {
    pknca_obj <- .
    if (!is.null(units_table)) {
      pknca_obj[["units"]] <- dplyr::rows_update(
        pknca_obj[["units"]],
        units_table,
        by = c("PPTESTCD", "PPORRESU"),
        unmatched = "ignore"
      )
    }
    pknca_obj
  }

## Run NCA calculations ########################################
flag_rules <- list(
  R2ADJ = list(
    is.checked = TRUE,
    threshold = 0.7
  ),
  R2 = list(
    is.checked = FALSE,
    threshold = 0.7
  ),
  AUCPEO = list(
    is.checked = TRUE,
    threshold = 20
  ),
  AUCPEP = list(
    is.checked = TRUE,
    threshold = 20
  ),
  LAMZSPN = list(
    is.checked = TRUE,
    threshold = 2
  )
)
ratio_table <- NULL
blq_rule <- list(
  first = "keep",
  middle = "keep",
  last = "keep"
)

pknca_res <- pknca_obj %>%

  # Run pk.nca and join subject and dose information to the results
  # Consider the BLQ imputation rule before calculations (if any)
  PKNCA_calculate_nca(
    blq_rule = blq_rule
  ) %>%

  # Add bioavailability results if requested
  add_f_to_pknca_results(NULL) %>%

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
  export_cdisc(grouping_vars = extra_vars_to_keep)

pivoted_results <- pivot_wider_pknca_results(
  myres = pknca_res,
  flag_rules = flag_rules,
  extra_vars_to_keep = extra_vars_to_keep
)
