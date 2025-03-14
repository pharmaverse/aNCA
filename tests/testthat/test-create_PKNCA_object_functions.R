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
units_data <- data.frame(
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
    expect_error(PKNCA_create_data_object(missing_columns_data),
    paste("All of the variables in the formula must be in the data.  Missing: AVAL"))
  })
  
  it("handles missing columns required for the functions in the input data", {
    # Missing columns in the function
    missing_columns_conc <- simple_data[, -which(names(simple_data) %in%  c("AFRLT"))]
    expect_error(PKNCA_create_data_object(missing_columns_conc),
                 paste("Missing required columns: AFRLT"))
    
    missing_columns_dose <- simple_data[, -which(names(simple_data) %in%  c("ARRLT"))]
    expect_error(PKNCA_create_data_object(missing_columns_dose),
                 paste("Missing required columns: ARRLT"))
    
  })
  
  it("handles multiple analytes", {
    # Multiple analytes and units
    results <- PKNCA_create_data_object(units_data)
    
    units_table <- results$units
    #contains ANALYTE column with two unique values
    expect_true("ANALYTE" %in% colnames(units_table))
    
    unique_analytes <- unique(units_table$ANALYTE)
    expect_equal(length(unique_analytes), 2)
    
  })
  
  #TODO: Add test for multiple units once implemented
  
  #TODO: add test for duplicated rows error message
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