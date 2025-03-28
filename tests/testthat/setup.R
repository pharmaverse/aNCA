# Import dataset from testthat/data folder

data <- read.csv(testthat::test_path("data", "adnca_dummy_sm_dataset.csv")) 

# Create PKNCAdata object
pknca_data <- PKNCA_create_data_object(data %>% filter(PCSPEC == "Plasma"))
# Set intervals
pknca_data$intervals <- format_pkncadata_intervals(pknca_data$conc, pknca_data$dose,
                                                   params = c("aucinf.obs", "aucint.last", "auclast",
                                                              "cmax", "half.life", "tmax",
                                                              "lambda.z", "lambda.z.n.points",
                                                              "r.squared", "adj.r.squared", "lambda.z.time.first"))
pknca_data <- create_start_impute(pknca_data)

# Create NCA results
res_nca <- PKNCA_calculate_nca(pknca_data)
