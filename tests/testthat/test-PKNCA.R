# Simple example dataset
simple_data <- data.frame(
  STUDYID = rep("STUDY001", 6),
  PCSPEC = rep("Plasma", 6),
  ROUTE = rep("IV", 6),
  DRUG = rep("DrugA", 6),
  USUBJID = rep("SUBJ001", 6),
  DOSNO = rep(1, 6),
  ANALYTE = rep("AnalyteA", 6),
  AVAL = c(0, 5, 10, 7, 3, 1),
  AVALU = rep("ng/mL", 6),
  DOSEA = rep(100, 6),
  DOSEU = rep("mg", 6),
  AFRLT = c(0, 1, 2, 3, 4, 6),
  ARRLT = c(0, 1, 2, 3, 4, 6),
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
  DOSNO = rep(1, 12),
  ANALYTE = rep(c("AnalyteX", "AnalyteY"), each = 6),
  AVAL = c(0, 2, 8, 6, 4, 1, 0, 10, 20, 18, 8, 3),
  AVALU = rep("ng/mL", 12),
  DOSEA = rep(200, 12),
  DOSEU = rep("mg", 12),
  AFRLT = rep(c(0, 1, 2, 3, 4, 6), 2),
  ARRLT = rep(c(0, 1, 2, 3, 4, 6), 2),
  NFRLT = rep(c(0, 1, 2, 3, 4, 6), 2),
  ADOSEDUR = rep(1, 12),
  RRLTU = rep("hour", 12)
)

# Simple example dataset
pknca_data <- PKNCA_create_data_object(simple_data)

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
    missing_columns_conc <- simple_data[, -which(names(simple_data) %in%  c("AFRLT"))]
    expect_error(
      PKNCA_create_data_object(missing_columns_conc),
      paste("Missing required columns: AFRLT")
    )

    missing_columns_dose <- simple_data[, -which(names(simple_data) %in%  c("ARRLT"))]
    expect_error(
      PKNCA_create_data_object(missing_columns_dose),
      paste("Missing required columns: ARRLT")
    )

  })

  it("handles multiple analytes", {
    # Multiple analytes and units
    results <- PKNCA_create_data_object(multiple_data)

    units_table <- results$units
    #contains ANALYTE column with two unique values
    expect_true("ANALYTE" %in% colnames(units_table))

    unique_analytes <- unique(units_table$ANALYTE)
    expect_equal(length(unique_analytes), 2)

  })
  # TODO: Add test for multiple units once implemented
  # TODO: add test for duplicated rows error message
})


# Test PKNCA_update_data_object
describe("PKNCA_update_data_object", {

  method <- "lin up log down"
  params <- c("cmax", "tmax", "auclast", "aucinf.obs")
  analytes <- unique(simple_data$ANALYTE)
  dosnos <- unique(simple_data$DOSNO)
  pcspecs <- unique(simple_data$PCSPEC)
  default_units <- pknca_data$units

  ma_data <- PKNCA_create_data_object(multiple_data)

  it("returns a PKNCAdata object", {
    updated_data <- PKNCA_update_data_object(
      adnca_data = pknca_data,
      method = method,
      selected_analytes = analytes,
      selected_dosno = dosnos,
      selected_pcspec = pcspecs,
      params = params,
      should_impute_c0 = TRUE
    )
    expect_s3_class(updated_data, "PKNCAdata")
  })

  it("includes only selected analytes, dosnos, and pcspecs in intervals", {
    updated_data <- PKNCA_update_data_object(
      adnca_data = ma_data,
      method = method,
      selected_analytes = "AnalyteX",
      selected_dosno = 1,
      selected_pcspec = "Plasma",
      params = params,
      should_impute_c0 = FALSE
    )
    intervals <- updated_data$intervals
    expect_true(all(intervals$ANALYTE == "AnalyteX"))
    expect_true(all(intervals$DOSNO == 1))
    expect_true(all(intervals$PCSPEC == "Plasma"))
  })

  it("updates units for each analyte", {
    updated_data <- PKNCA_update_data_object(
      adnca_data = pknca_data,
      method = method,
      selected_analytes = analytes,
      selected_dosno = dosnos,
      selected_pcspec = pcspecs,
      params = params,
      should_impute_c0 = TRUE
    )
    unit_analytes <- unique(updated_data$units$ANALYTE)
    expect_setequal(unit_analytes, analytes)
  })

  it("sets NCA options correctly", {
    updated_data <- PKNCA_update_data_object(
      adnca_data = pknca_data,
      method = method,
      selected_analytes = analytes,
      selected_dosno = dosnos,
      selected_pcspec = pcspecs,
      params = params,
      should_impute_c0 = TRUE
    )
    expect_equal(updated_data$options$auc.method, "lin up log down")
    expect_equal(updated_data$options$min.hl.r.squared, 0.01)
    expect_true("DOSNO" %in% updated_data$options$keep_interval_cols)
  })

  it("does not impute C0 when not requested", {
    updated_data <- PKNCA_update_data_object(
      adnca_data = pknca_data,
      method = method,
      selected_analytes = analytes,
      selected_dosno = dosnos,
      selected_pcspec = pcspecs,
      params = params,
      should_impute_c0 = FALSE
    )
    expect_true("impute" %in% names(updated_data))
    expect_true(all(is.na(updated_data$impute)))
  })

  it("ensures units table has separate rows per analyte", {
    updated_data <- PKNCA_update_data_object(
      adnca_data = pknca_data,
      method = "lin up log down",
      selected_analytes = unique(simple_data$ANALYTE),
      selected_dosno = unique(simple_data$DOSNO),
      selected_pcspec = unique(simple_data$PCSPEC),
      params = c("cmax", "tmax", "auclast", "aucinf.obs"),
      should_impute_c0 = TRUE
    )

    # Get units table and analyte column
    units_table <- updated_data$units
    analyte_col <- updated_data$conc$columns$groups$group_analyte

    # Extract number of unique analytes and parameters
    analytes <- unique(units_table[[analyte_col]])
    params <- unique(units_table$PPTESTCD)

    # Expect a row for every analyte-param combination
    expected_nrows <- length(analytes) * length(params)
    expect_equal(nrow(units_table), expected_nrows)

    # Sanity: make sure all analyte-param pairs exist
    pair_check <- tidyr::crossing(
      ANALYTE = analytes,
      PPTESTCD = params
    )

    expect_true(all(
      dplyr::semi_join(pair_check, units_table, by = c("ANALYTE", "PPTESTCD")) %>%
        nrow() == nrow(pair_check)
    ))
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

    #check that only two items have been added to list
    expect_equal(length(colnames(nca_results$result)), 15)
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
