# Load the package (https://github.com/pharmaverse/aNCA)  #
###########################################################
## Please, set your working directory to this file:
# setwd("path/to/this/file/script_template.R")

if (!require("aNCA")) install.packages("aNCA")
library(aNCA)
library(dplyr)

# Load raw data #
data_path <- "adnca_example.csv"
adnca_data <- read_pk(data_path)

## Preprocess data & create PKNCA object ######################
mapping <- list(
  STUDYID = "STUDYID",
  USUBJID = "USUBJID",
  ATPTREF = "ATPTREF",
  PARAM = "PARAM",
  Metabolites = "Metab-DrugA",
  PCSPEC = "PCSPEC",
  AVAL = "AVAL",
  ROUTE = "ROUTE",
  DOSETRT = "DOSETRT",
  DOSEA = "DOSEA",
  AFRLT = "AFRLT",
  ARRLT = "ARRLT",
  NFRLT = "NFRLT",
  NRRLT = "NRRLT",
  AVALU = "AVALU",
  RRLTU = "RRLTU",
  DOSEU = "DOSEU",
  Grouping_Variables = c("TRT01A", "RACE", "SEX"),
  AEFRLT = "AFRLT",
  NCAwXRS = c("NCA1XRS", "NCA2XRS"),
  ADOSEDUR = "ADOSEDUR",
  VOLUME = "VOLUME",
  VOLUMEU = "VOLUMEU",
  TRTRINT = "TRTRINT"
)
applied_filters <- list(
  filter_1 = list(
    column = "ATPTREF",
    condition = "==",
    value = c("DOSE 1", "DOSE 2", "DOSE 3")
  )
)
time_duplicate_rows <- NULL

int_parameters <- data.frame(
  parameter = c("AUCINT", "AUCINT"),
  start_auc = c(NA, NA),
  end_auc = c(NA, NA)
)
units_table <- NULL
parameters_selected_per_study <- list(
  "Multiple IV Infusion" = c(
    "adj.r.squared", "auclast", "auclast.dn", "clast.obs", "clast.obs.dn", "cmax", "cmax.dn", "half.life", "lambda.z", "lambda.z.n.points", "lambda.z.time.first", "r.squared", "span.ratio", "tlast", "tmax",
    "vss.obs"
  )
)
extra_vars_to_keep <-  c("TRT01A", "RACE", "SEX", "DOSEA", "ATPTREF", "ROUTE")
slope_rules <- data.frame()

pknca_obj <- adnca_data %>%

  # Preprocess raw data and create the PKNCA object
  PKNCA_create_data_object(
    mapping = mapping,
    applied_filters = applied_filters,
    time_duplicate_rows = time_duplicate_rows
  ) %>%

  # Setup NCA settings, intervals, parameter selections, and units
  PKNCA_update_data_object(
    method = "lin up/log down",
    selected_analytes = c("DrugA", "Metab-DrugA"),
    selected_profile = "DOSE 1",
    selected_pcspec = "SERUM",
    start_impute = TRUE,
    exclusion_list = list(),
    hl_adj_rules = slope_rules,
    keep_interval_cols = setdiff(extra_vars_to_keep, c("DOSEA", "ATPTREF", "ROUTE")),
    min_hl_points = 3 %||% 3,
    parameter_selections = parameters_selected_per_study,
    int_parameters = int_parameters,
    custom_units_table = units_table
  )

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
ratio_table <- data.frame()
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
