# Simple example dataset
simple_data <- data.frame(
  STUDYID = rep("STUDY001", 6),
  PCSPEC = rep("Plasma", 6),
  ROUTE = rep("IV", 6),
  DRUG = rep("DrugA", 6),
  USUBJID = rep("SUBJ001", 6),
  NCA_PROFILE = rep(1, 6),
  PARAM = rep("AnalyteA", 6),
  AVAL = c(0, 5, 10, 7, 3, 1),
  AVALU = rep(c("ng/mL"), 6),
  DOSEA = rep(100, 6),
  DOSEU = rep("mg", 6),
  AFRLT = c(0.5, 1, 2, 3, 4, 6),
  ARRLT = c(0.5, 1, 2, 3, 4, 6),
  NFRLT = c(0, 1, 2, 3, 4, 6),
  ADOSEDUR = rep(0.5, 6),
  RRLTU = rep("hour", 6)
)

# Multiple
multiple_data <- data.frame(
  STUDYID = rep("STUDY002", 12),
  PCSPEC = rep("Plasma", 12),
  ROUTE = rep("IV", 12),
  DRUG = rep("DrugB", 12),
  USUBJID = rep(rep(c("SUBJ002", "SUBJ003"), each = 6)),
  NCA_PROFILE = rep(1, 12),
  PARAM = rep(c("AnalyteX", "AnalyteY"), each = 6),
  AVAL = c(0, 2, 8, 6, 4, 1, 0, 10, 20, 18, 8, 3),
  AVALU = rep(c("ng/mL", "mg/mL"), each = 6),
  DOSEA = rep(200, 12),
  DOSEU = rep("mg", 12),
  AFRLT = rep(c(0.5, 1, 2, 3, 4, 6), 2),
  ARRLT = rep(c(0.5, 1, 2, 3, 4, 6), 2),
  NFRLT = rep(c(0, 1, 2, 3, 4, 6), 2),
  ADOSEDUR = rep(1, 12),
  RRLTU = rep("hour", 12)
)

# Simple example dataset
pknca_data <- PKNCA_create_data_object(simple_data)

# ToDo (Gerardo): refine PKNCA_create_data_object
#                 to make less implicit column assumptions

describe("PKNCA_create_data_object", {
  it("creates a PKNCAdata object with concentration, doses, and units based on ADNCA data", {
    expect_s3_class(pknca_data, "PKNCAdata")
  })

  it("handles missing columns required for PKNCA in the input data", {
    # Missing columns in the input data
    missing_columns_data <- simple_data[, -which(names(simple_data) %in% c("AVAL", "AVALU"))]
    # expect error message to user
    expect_error(
      PKNCA_create_data_object(missing_columns_data),
      paste("All of the variables in the formula must be in the data.  Missing: AVAL")
    )
  })

  it("handles missing columns required for the functions in the input data", {
    # Missing columns in the function
    missing_columns_conc <- simple_data[, -which(names(simple_data) %in%  c("ARRLT"))]
    expect_error(
      PKNCA_create_data_object(missing_columns_conc),
      paste("Missing required columns: ARRLT")
    )

    missing_columns_dose <- simple_data[, -which(names(simple_data) %in%  c("AFRLT"))]
    expect_error(
      PKNCA_create_data_object(missing_columns_dose),
      paste("Missing required columns: AFRLT")
    )
  })

  it("handles duplicates in DFLAG", {
    # Duplicate DFLAG values
    duplicate_data <- simple_data %>% mutate(AFRLT = c(0.5, 1, 2, 3, 3, 6))
    duplicate_data$DFLAG <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
    results <- PKNCA_create_data_object(duplicate_data)

    # Check that flag = FALSE values are removed
    expect_equal(nrow(results$conc$data), 5)
  })

  it("produces a message error when missing values are in group columns", {
    # Duplicate DFLAG values
    adnca_with_group_na <- multiple_data
    adnca_with_group_na$PCSPEC[1] <- NA_character_
    expect_error(
      PKNCA_create_data_object(adnca_with_group_na),
      "Missing values detected in grouping columns: PCSPEC"
    )
  })

  it("handles VOLUME column correctly", {
    # Create a data frame with VOLUME column
    volume_data <- simple_data
    volume_data$VOLUME <- c(1, 2, 3, 4, 5, 6)
    volume_data$VOLUMEU <- "mL"

    # Create PKNCAdata object
    pknca_volume_data <- PKNCA_create_data_object(volume_data)

    # Check if VOLUME and VOLUMEU are included in the object
    expect_true("VOLUME" %in% names(pknca_volume_data$conc$data))
    expect_true("VOLUMEU" %in% names(pknca_volume_data$conc$data))
    expect_true("AMOUNTU" %in% names(pknca_volume_data$conc$data))
  })
})



# Test PKNCA_update_data_object
describe("PKNCA_update_data_object", {

  method <- "lin up log down"
  params <- c("cmax", "tmax", "auclast", "aucinf.obs")
  analytes <- unique(simple_data$PARAM)
  dosnos <- unique(simple_data$NCA_PROFILE)
  pcspecs <- unique(simple_data$PCSPEC)
  auc_data <- data.frame(start_auc = numeric(), end_auc = numeric())

  ma_data <- PKNCA_create_data_object(multiple_data)

  it("returns a PKNCAdata object", {
    updated_data <- PKNCA_update_data_object(
      adnca_data = pknca_data,
      auc_data = auc_data,
      method = method,
      selected_analytes = analytes,
      selected_profile = dosnos,
      selected_pcspec = pcspecs,
      params = params,
      should_impute_c0 = TRUE
    )
    expect_s3_class(updated_data, "PKNCAdata")
  })

  it("includes only selected analytes, dosnos, and pcspecs in intervals", {
    updated_data <- PKNCA_update_data_object(
      adnca_data = ma_data,
      auc_data = auc_data,
      method = method,
      selected_analytes = "AnalyteX",
      selected_profile = 1,
      selected_pcspec = "Plasma",
      params = params,
      should_impute_c0 = FALSE
    )
    intervals <- updated_data$intervals
    expect_true(all(intervals$PARAM == "AnalyteX"))
    expect_true(all(intervals$NCA_PROFILE == 1))
    expect_true(all(intervals$PCSPEC == "Plasma"))
  })

  it("sets NCA options correctly", {
    updated_data <- PKNCA_update_data_object(
      adnca_data = pknca_data,
      auc_data = auc_data,
      method = method,
      selected_analytes = analytes,
      selected_profile = dosnos,
      selected_pcspec = pcspecs,
      params = params,
      should_impute_c0 = TRUE
    )
    expect_equal(updated_data$options$auc.method, "lin up log down")
    expect_equal(updated_data$options$min.hl.r.squared, 0.01)
    expect_true("NCA_PROFILE" %in% updated_data$options$keep_interval_cols)
  })

  it("does not impute C0 when not requested", {
    updated_data <- PKNCA_update_data_object(
      adnca_data = pknca_data,
      auc_data = auc_data,
      method = method,
      selected_analytes = analytes,
      selected_profile = dosnos,
      selected_pcspec = pcspecs,
      params = params,
      should_impute_c0 = FALSE
    )
    expect_true("impute" %in% names(updated_data))
    expect_true(all(is.na(updated_data$impute)))
  })

  it("handles partial AUCs (auc_data) creating proper intervals for each", {
    auc_data <- data.frame(
      start_auc = c(0, 1, 2),
      end_auc = c(1, 2, 3)
    )
    updated_data <- PKNCA_update_data_object(
      adnca_data = pknca_data,
      auc_data = auc_data,
      method = method,
      selected_analytes = analytes,
      selected_profile = dosnos,
      selected_pcspec = pcspecs,
      params = params,
      should_impute_c0 = TRUE
    )
    # Check that the interval_type column is present with at least one "manual" value
    expect_true(any(updated_data$intervals$type_interval == "manual"))

    # Check AUC interval rows have proper columns and only aucint.last parameter as TRUE
    auc_intervals <- updated_data$intervals  %>%
      dplyr::filter(type_interval == "manual") %>%
      dplyr::select(start, end, STUDYID, DRUG, USUBJID, PARAM,
                    NCA_PROFILE, auclast, aucint.last, tmax)

    expected_res <- tidyr::tibble(
      start = c(0, 1, 2),
      end = c(1, 2, 3),
      STUDYID = rep("STUDY001", 3),
      DRUG = rep("DrugA", 3),
      USUBJID = rep("SUBJ001", 3),
      PARAM = rep("AnalyteA", 3),
      NCA_PROFILE = rep(1, 3),
      auclast = rep(FALSE, 3),
      aucint.last = rep(TRUE, 3),
      tmax = rep(FALSE, 3)
    )

    expect_equal(auc_intervals, expected_res)
  })
})


#Calculate NCA
nca_results <- PKNCA_calculate_nca(pknca_data)

describe("PKNCA_calculate_nca", {

  it("calculates results for PKNCA analysis", {
    expect_s3_class(nca_results, "PKNCAresults")
  })

  it("adds start and end from most recent dose", {
    # Check that the results have the dosing data
    expect_true("start_dose" %in% colnames(nca_results$result))
    expect_true("end_dose" %in% colnames(nca_results$result))

    # Check that only two items have been added to the list
    expect_equal(length(colnames(nca_results$result)), 15)
  })

  it("handles warning levels correctly", {

    # Modify the data to eliminate points needed for half life
    modified_data <- simple_data[1, ] # Remove one time point
    modified_data$AVAL <- NA
    pknca_data_modified <- suppressWarnings(PKNCA_create_data_object(modified_data))
    pknca_data_modified$intervals$half.life <- TRUE
  })
})

describe("PKNCA_impute_method_start_logslope", {
  it("does not impute when start is in the data", {
    expect_equal(
      PKNCA_impute_method_start_logslope(conc = 3:1, time = 0:2, start = 0, end = 2),
      data.frame(conc = 3:1, time = 0:2)
    )
  })

  it("imputes when start is not in the data", {
    expect_equal(
      PKNCA_impute_method_start_logslope(conc = 3:1, time = 1:3, start = 0, end = 3),
      data.frame(conc = c(4.5, 3:1), time = 0:3),
      ignore_attr = TRUE
    )
  })

  it("ignores data outside the interval (before interval)", {
    expect_equal(
      PKNCA_impute_method_start_logslope(conc = c(0, 2:1), time = c(-1, 1:2), start = 0, end = 2),
      data.frame(conc = c(0, 4, 2:1), time = c(-1, 0, 1:2)),
      ignore_attr = TRUE
    )
  })

  it("does not modify if no C1 -> C2 decline in samples", {
    expect_equal(
      PKNCA_impute_method_start_logslope(conc = c(1, 1, 1), time = 1:3, start = 0, end = 3),
      data.frame(conc = c(1, 1, 1), time = 1:3),
      ignore_attr = TRUE
    )
  })

  it("does not modify if C1 = C2 in samples", {
    expect_equal(
      PKNCA_impute_method_start_logslope(conc = c(3, 3, 1), time = 1:3, start = 0, end = 3),
      data.frame(conc = c(3, 3, 1), time = 1:3),
      ignore_attr = TRUE
    )
  })
})

describe("PKNCA_impute_method_start_c1", {
  it("does not impute when start is in the data", {
    expect_equal(
      PKNCA_impute_method_start_c1(conc = 1:3, time = 0:2, start = 0, end = 2),
      data.frame(conc = 1:3, time = 0:2)
    )
  })

  it("imputes when start is not in the data", {
    expect_equal(
      PKNCA_impute_method_start_c1(conc = 1:3, time = 1:3, start = 0, end = 3),
      data.frame(conc = c(1, 1:3), time = 0:3),
      ignore_attr = TRUE
    )
  })

  it("ignores data outside the interval (before interval)", {
    expect_equal(
      PKNCA_impute_method_start_c1(conc = 1:3, time = c(-1, 1:2), start = 0, end = 2),
      data.frame(conc = c(1, 2, 2:3), time = c(-1, 0, 1:2)),
      ignore_attr = TRUE
    )
  })
})

# Tests for PKNA_build_units_table
describe("PKNCA_build_units_table", {

  # Subset the data to only include USUBJID 8 (2 analytes, A & B)
  d_conc <- FIXTURE_CONC_DATA %>%
    filter(USUBJID == 8)
  d_dose <- FIXTURE_DOSE_DATA %>%
    filter(USUBJID == 8)

  o_conc <- PKNCA::PKNCAconc(d_conc, AVAL ~ AFRLT | USUBJID / PARAM,
                             concu = "AVALU", timeu = "RRLTU")
  o_dose <- PKNCA::PKNCAdose(d_dose, DOSEA ~ AFRLT | USUBJID,
                             doseu = "DOSEU")
  units_table <- expect_no_error(PKNCA_build_units_table(o_conc, o_dose))

  it("creates a seggregated units table when unit columns are defined in the PKNCA objects", {
    # Check units_table is a data frame
    expect_true(is.data.frame(units_table))

    # Contains the seggregating variable PARAM & parameter unit columns
    expect_equal(
      colnames(units_table),
      c("PARAM", "PPTESTCD", "PPORRESU", "PPSTRESU", "conversion_factor")
    )

    # Differentiates concentration units by PARAM
    analyte_a_cmax_unit <- units_table %>%
      dplyr::filter(PARAM == "A", PPTESTCD == "cmax") %>%
      dplyr::pull(PPSTRESU)
    analyte_b_cmax_unit <- units_table %>%
      dplyr::filter(PARAM == "B", PPTESTCD == "cmax") %>%
      dplyr::pull(PPSTRESU)
    expect_equal(analyte_a_cmax_unit, "ng/mL")
    expect_equal(analyte_b_cmax_unit, "ug/mL")
  })

  it("creates an uniform units table when units are not defined as columns in the PKNCA obj", {
    o_conc <- PKNCA::PKNCAconc(d_conc, AVAL ~ AFRLT | USUBJID / PARAM,
                               concu = "ng/mL", timeu = "h")
    o_dose <- PKNCA::PKNCAdose(d_dose, DOSEA ~ AFRLT | USUBJID,
                               doseu = "mg")
    units_table <- expect_no_error(PKNCA_build_units_table(o_conc, o_dose))
    # Check units_table is a data frame
    expect_true(is.data.frame(units_table))
    # Does not consider any seggregating variable
    expect_equal(
      colnames(units_table),
      c("PPTESTCD", "PPORRESU", "PPSTRESU", "conversion_factor")
    )
    # Contains unit values for concentration and dose
    cmaxdn_unit <- units_table %>%
      dplyr::filter(PPTESTCD == "cmax.dn") %>%
      dplyr::pull(PPSTRESU)
    expect_equal(cmaxdn_unit, "(ng/mL)/mg")
  })

  it("creates a NA units tables when units are not defined in the PKNCA objects", {
    o_conc <- PKNCA::PKNCAconc(d_conc, AVAL ~ AFRLT | USUBJID / PARAM)
    o_dose <- PKNCA::PKNCAdose(d_dose, DOSEA ~ AFRLT | USUBJID)
    units_table <- expect_no_error(PKNCA_build_units_table(o_conc, o_dose))
    # Check units_table is a data frame
    expect_true(is.data.frame(units_table))
    # Does not consider any seggregating variable
    expect_equal(
      colnames(units_table),
      c("PPTESTCD", "PPORRESU", "PPSTRESU", "conversion_factor")
    )
    # Does not contain any unit values except for adimensional parameters
    expect_setequal(
      unique(units_table$PPSTRESU),
      c("unitless", "fraction", "%", "count", NA)
    )
  })

  it("reports an error when units are not uniform through all concentration groups", {
    d_conc$AVALU[1] <- "pg/L"
    o_conc <- PKNCA::PKNCAconc(d_conc, AVAL ~ AFRLT | USUBJID / PARAM,
                               concu = "AVALU", timeu = "RRLTU")
    expect_error(
      PKNCA_build_units_table(o_conc, o_dose),
      regexp = "Units should be uniform at least across concentration groups.*"
    )
  })
})

describe("select_level_grouping_cols", {

  # Make a dataset where a variable `d` depends on `a` & `b`
  data <- data.frame(
    a = rep(letters[c(1, 2, 3)], each = 4),
    b = rep(letters[c(1, 2)], each = 3),
    c = letters[1]
  ) %>%
    mutate(
      d = paste0(a, b)
    )

  it("returns the minimal grouping_columns (a, b) for one target column", {
    result <- select_minimal_grouping_cols(data, "d")
    expect_equal(result, data[c("a", "b", "d")])
  })

  # Note: this case will never happen in the App or PKNCA_build_units_table
  it("returns the original data if target_columns is NULL", {
    result <- select_minimal_grouping_cols(data, NULL)
    expect_equal(result, data)
  })

  it("returns just the strata columns if no stratification groups are found", {
    data[, "a"] <- 10
    result <- select_minimal_grouping_cols(data, "d")
    expect_equal(result, data["d"])
  })
})
