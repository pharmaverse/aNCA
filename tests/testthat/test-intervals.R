# Shared data setup for all tests
ADNCA <- data.frame(
  STUDYID = rep(1, 20),
  USUBJID = rep(1:2, each = 10),
  PCSPEC = rep("Plasma", 20),
  DRUG = rep("DrugA", 20),
  PARAM = rep("Analyte1", 20),
  AFRLT = rep(seq(0, 9), 2),
  ARRLT = rep(seq(0, 4), 4),
  NFRLT = rep(seq(0, 9), 2),
  NCA_PROFILE = rep(1, 20),
  DOSEA = rep(c(5, 10), each = 10),
  ROUTE = rep(c("intravascular", "extravascular"), each = 10),
  ADOSEDUR = rep(c(0, 0), each = 10),
  AVAL = runif(20)
)

describe("format_pkncadata_intervals", {
  multi_analyte_adnca <- ADNCA %>% mutate(PARAM = rep(c("Analyte1", "Metabolite1"), each = 10))
  df_conc <- format_pkncaconc_data(multi_analyte_adnca,
                                   group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                     "DRUG", "PARAM"),
                                   time_column = "AFRLT")
  
  df_dose <- format_pkncadose_data(df_conc,
                                   group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                                     "DRUG"))
  
  pknca_conc <- PKNCA::PKNCAconc(
    df_conc,
    formula = AVAL ~ AFRLT | STUDYID + PCSPEC + DRUG + USUBJID / PARAM,
    exclude_half.life = "exclude_half.life",
    time.nominal = "NFRLT"
  )
  
  pknca_dose <- PKNCA::PKNCAdose(
    data = df_dose,
    formula = DOSEA ~ AFRLT | STUDYID + DRUG + USUBJID
  )

  it("handles multiple analytes with metabolites", {
    result <- format_pkncadata_intervals(pknca_conc, pknca_dose)
    
    expect_equal(result$start[1], 0)
    expect_equal(result$end[1], 5)
    expect_equal(unique(result$PARAM), c("Analyte1", "Metabolite1"))
    expect_equal(result$cmax[1], FALSE)
    expect_equal(result$tmax[1], FALSE)
    expect_equal(result$half.life[1], FALSE)
    expect_equal(result$cl.obs[1], FALSE)

  })
  
  it("handles incorrect input type", {
    expect_error(format_pkncadata_intervals(pknca_conc = df_conc, data.frame()),
                 regexp = "Input pknca_conc must be a PKNCAconc object from the PKNCA package.")
    
    expect_error(format_pkncadata_intervals(pknca_conc = pknca_conc, data.frame()),
                 regexp = "Input pknca_dose must be a PKNCAdose object from the PKNCA package.")
  })
  
  it("handles missing columns", {
    missing_col_pknca_dose <- pknca_dose
    missing_col_pknca_dose$data <- select(missing_col_pknca_dose$data, -DRUG)
    expect_error(
      format_pkncadata_intervals(
        pknca_conc,
        missing_col_pknca_dose
      ),
      regexp = "Missing required columns: DRUG"
    )
  })
  
  it("correctly uses tau if column is available", {
    df_conc_tau <- df_conc %>%
      mutate(TAU = 5)  # Add a tau column for testing
    
    pknca_conc_tau <- PKNCA::PKNCAconc(
      df_conc_tau,
      formula = AVAL ~ AFRLT | STUDYID + PCSPEC + DRUG + USUBJID / PARAM,
      exclude_half.life = "exclude_half.life",
      time.nominal = "NFRLT"
    )
    
    result_tau <- format_pkncadata_intervals(pknca_conc_tau, pknca_dose)
    
    expect_equal(result_tau$end[4], 10)
  })
  
  it("sets last time to end AFRLT if no TAU available", {
    result <- format_pkncadata_intervals(pknca_conc, pknca_dose)
    expect_equal(result$end[4], 9)
  })
  
  it("sets end to Inf if no TAU and single dose", {
    single_dose_pknca_conc <- pknca_conc
    single_dose_pknca_conc$data <- single_dose_pknca_conc$data %>%
      filter(DOSNOA == 1)  # Filter to a single dose
    
    single_dose_pknca_dose <- pknca_dose
    single_dose_pknca_dose$data <- single_dose_pknca_dose$data %>%
      filter(DOSNOA == 1)  # Filter to a single dose
    
    result_single_dose <- format_pkncadata_intervals(single_dose_pknca_conc,
                                                     single_dose_pknca_dose)
    expect_true(all(is.infinite(result_single_dose$end)))
  })
  
  it("sets end to Inf if TAU= NA and single dose", {
    single_dose_pknca_conc <- pknca_conc
    single_dose_pknca_conc$data <- single_dose_pknca_conc$data %>%
      mutate(TAU = NA) %>%  # Set TAU to NA
      filter(DOSNOA == 1)  # Filter to a single dose
    
    single_dose_pknca_dose <- pknca_dose
    single_dose_pknca_dose$data <- single_dose_pknca_dose$data %>%
      mutate(TAU = NA) %>%  # Set TAU to NA
      filter(DOSNOA == 1)  # Filter to a single dose
    
    result_single_dose <- format_pkncadata_intervals(single_dose_pknca_conc,
                                                     single_dose_pknca_dose)
    expect_true(all(is.infinite(result_single_dose$end)))
  })
  
  it("sets end to last time point if TAU= NA and multiple dose", {
    pknca_conc_na_tau <- pknca_conc
    pknca_conc_na_tau$data <- pknca_conc$data %>%
      mutate(TAU = NA) # Set TAU to NA
    
    pknca_dose_na_tau <- pknca_dose
    pknca_dose_na_tau$data <- pknca_dose$data %>%
      mutate(TAU = NA)  # Set TAU to NA
    
    result_single_dose <- format_pkncadata_intervals(pknca_conc_na_tau,
                                                     pknca_dose_na_tau)
    expect_equal(result_single_dose$end[4], max(pknca_conc_na_tau$data$AFRLT, na.rm = TRUE))
  })
  
  
  it("uses ARRLT for start when start_from_last_dose is FALSE", {
    result <- format_pkncadata_intervals(
      pknca_conc = pknca_conc,
      pknca_dose = pknca_dose,
      start_from_last_dose = FALSE
    )
    expect_true(all(result$start >= 0))
  })
  
})













# 
# expect_no_error(
#   PKNCA::PKNCAdata(
#     data.conc = pknca_conc,
#     data.dose = pknca_dose,
#     intervals = result,
#     options = list(
#       keep_interval_cols = c("NCA_PROFILE", "DOSNOA", "type_interval")
#     ),
#     units = PKNCA::pknca_units_table(
#       concu = "ng/mL",
#       doseu = "mg",
#       amountu = "ng",
#       timeu = "h"
#     )
#   )
# )



# 
# it("does not impute C0 when not requested", {
#   updated_data <- PKNCA_update_data_object(
#     adnca_data = pknca_data,
#     method = method,
#     selected_analytes = analytes,
#     selected_profile = dosnos,
#     selected_pcspec = pcspecs,
#     should_impute_c0 = FALSE
#   )
#   expect_true("impute" %in% names(updated_data))
#   expect_true(all(is.na(updated_data$impute)))
# })
# 
# it("handles partial AUCs (auc_data) creating proper intervals for each", {
#   auc_data <- data.frame(
#     start_auc = c(0, 1, 2),
#     end_auc = c(1, 2, 3)
#   )
#   updated_data <- PKNCA_update_data_object(
#     adnca_data = pknca_data,
#     auc_data = auc_data,
#     method = method,
#     selected_analytes = analytes,
#     selected_profile = dosnos,
#     selected_pcspec = pcspecs,
#     params = params,
#     should_impute_c0 = TRUE
#   )
#   # Check that the interval_type column is present with at least one "manual" value
#   expect_true(any(updated_data$intervals$type_interval == "manual"))
#   
#   # Check AUC interval rows have proper columns and only aucint.last parameter as TRUE
#   auc_intervals <- updated_data$intervals  %>%
#     dplyr::filter(type_interval == "manual") %>%
#     dplyr::select(start, end, STUDYID, DRUG, USUBJID, PARAM,
#                   NCA_PROFILE, auclast, aucint.last, tmax)
#   
#   expected_res <- tidyr::tibble(
#     start = c(0, 1, 2),
#     end = c(1, 2, 3),
#     STUDYID = rep("STUDY001", 3),
#     DRUG = rep("DrugA", 3),
#     USUBJID = rep("SUBJ001", 3),
#     PARAM = rep("AnalyteA", 3),
#     NCA_PROFILE = rep(1, 3),
#     auclast = rep(FALSE, 3),
#     aucint.last = rep(TRUE, 3),
#     tmax = rep(FALSE, 3)
#   )
#   
#   expect_equal(auc_intervals, expected_res)
# })