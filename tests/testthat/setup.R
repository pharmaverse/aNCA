# Testing plots
TEST_PLOTS <- readRDS(testthat::test_path("data", "test_plots.rds"))

# Import dataset from testthat/data folder

DUMMY_DATA_FIXTURE <- read.csv(testthat::test_path("data", "adnca_dummy_sm_dataset.csv"))

# Create PKNCAdata object
PKNCA_DATA_FIXTURE <- PKNCA_create_data_object(DUMMY_DATA_FIXTURE %>% filter(PCSPEC == "Plasma"))
# Set intervals
PKNCA_DATA_FIXTURE$intervals <- format_pkncadata_intervals(
  PKNCA_DATA_FIXTURE$conc, PKNCA_DATA_FIXTURE$dose,
  params = c("aucinf.obs", "aucint.last", "auclast",
             "cmax", "half.life", "tmax",
             "lambda.z", "lambda.z.n.points",
             "r.squared", "adj.r.squared", "lambda.z.time.first")
)
PKNCA_DATA_FIXTURE <- create_start_impute(PKNCA_DATA_FIXTURE)

# Create NCA results
PKNCA_RESULTS_FIXTURE <- PKNCA_calculate_nca(PKNCA_DATA_FIXTURE)
