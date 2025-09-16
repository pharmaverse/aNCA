
expected_df <- data.frame(
  USUBJID = 1:3,
  AVAL = 1:3,
  AVALU = "ng/mL",
  DOSE = 1:3,
  DOSEU = "mg",
  AFRLT = 1:3,
  DOSNO = 1
)
desired_order <- names(expected_df)

# Define the desired column order
MAPPING_DESIRED_ORDER <- c(
  "STUDYID", "USUBJID", "PARAM", "PCSPEC", "NCA_PROFILE",
  "AVAL", "AVALU", "AFRLT", "ARRLT", "NRRLT", "NFRLT",
  "RRLTU", "ROUTE", "DRUG", "DOSEA", "DOSEU", "ADOSEDUR",
  "VOLUME", "VOLUMEU", "TAU"
)

describe("apply_mapping", {
  it("renames columns correctly and informs of the changes", {
    # Change variable names to simulate user dataset
    test_df <- expected_df
    names(test_df) <- c("id", "CONC", "dose", "TIME")

    # Define the input specifications
    mapping <- as.list(setNames(names(expected_df), names(test_df)))
    result <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)

    expect

    expect_true(all(c("ID", "AVAL", "DOSEVAR", "RRLT") %in% colnames(result)))
    expect_equal(result$AVAL, 1:3)
    expect_equal(result$DOSEVAR, 4:6)
    expect_equal(result$RRLT, 7:9)
    expect_equal(result$AVALU[1], "ng/mL")
    expect_equal(result$DOSEU[1], "mg")
    expect_equal(result$RRLTU[1], "h")
  })
  
  it("adds ADOSEDUR = 0 if not mapped", {
    dataset <- data.frame(ID = 1:2, CONC = 1:2, DOSE = 3:4, TIME = 5:6)
    mapping <- list(ID = "ID", AVAL = "CONC", DOSEVAR = "DOSE", RRLT = "TIME", NCA_PROFILE = NULL, AVALU = "ng/mL", DOSEU = "mg", RRLTU = "h")
    manual_units <- list(concentration = c("ng/mL"), dose = c("mg"), time = c("h"))
    column_groups <- list(
      "Group Identifiers" = c(ID = "ID"),
      "Sample Variables" = c(AVAL = "CONC", DOSEVAR = "DOSE", RRLT = "TIME"),
      "Supplemental Variables" = character(0)
    )
    desired_order <- c("ID", "AVAL", "DOSEVAR", "RRLT")
    result <- apply_mapping(dataset, mapping, manual_units, column_groups, desired_order, silent = TRUE)
    expect_true("ADOSEDUR" %in% colnames(result))
    expect_true(all(result$ADOSEDUR == 0))
  })
  
  it("removes concentration duplicates", {
    dataset <- data.frame(ID = c(1,1,2), CONC = c(10,10,20), DOSE = c(5,5,10), TIME = c(0,0,1))
    mapping <- list(ID = "ID", AVAL = "CONC", DOSEVAR = "DOSE", RRLT = "TIME", NCA_PROFILE = NULL, AVALU = "ng/mL", DOSEU = "mg", RRLTU = "h")
    manual_units <- list(concentration = c("ng/mL"), dose = c("mg"), time = c("h"))
    column_groups <- list(
      "Group Identifiers" = c(ID = "ID"),
      "Sample Variables" = c(AVAL = "CONC", DOSEVAR = "DOSE", RRLT = "TIME"),
      "Supplemental Variables" = character(0)
    )
    desired_order <- c("ID", "AVAL", "DOSEVAR", "RRLT")
    result <- apply_mapping(dataset, mapping, manual_units, column_groups, desired_order, silent = TRUE)
    expect_equal(nrow(result), 2)
    expect_true(all(result$ID %in% c(1,2)))
  })
  
  it("returns NULL if mapping is invalid (missing)", {
    dataset <- data.frame(ID = 1:2, CONC = 1:2, DOSE = 3:4, TIME = 5:6)
    mapping <- list(ID = "ID", AVAL = "CONC", DOSEVAR = "DOSE", RRLT = "TIME", NCA_PROFILE = NULL, AVALU = "ng/mL", DOSEU = "mg", RRLTU = "h")
    manual_units <- list(concentration = c("ng/mL"), dose = c("mg"), time = c("h"))
    column_groups <- list(
      "Group Identifiers" = c(ID = "ID"),
      "Sample Variables" = c(AVAL = "CONC", DOSEVAR = "DOSE", RRLT = "TIME"),
      "Supplemental Variables" = character(0)
    )
    desired_order <- c("ID", "AVAL", "DOSEVAR", "RRLT")
    mapping$AVAL <- "" # missing
    expect_error(apply_mapping(dataset, mapping, manual_units, column_groups, desired_order, silent = TRUE), "Unmapped columns detected")
  })
  
  it("returns NULL if mapping is invalid (duplicate)", {
    dataset <- data.frame(ID = 1:2, CONC = 1:2, DOSE = 3:4, TIME = 5:6)
    mapping <- list(ID = "ID", AVAL = "CONC", DOSEVAR = "DOSE", RRLT = "TIME", NCA_PROFILE = NULL, AVALU = "ng/mL", DOSEU = "mg", RRLTU = "h")
    manual_units <- list(concentration = c("ng/mL"), dose = c("mg"), time = c("h"))
    column_groups <- list(
      "Group Identifiers" = c(ID = "ID"),
      "Sample Variables" = c(AVAL = "CONC", DOSEVAR = "DOSE", RRLT = "TIME"),
      "Supplemental Variables" = character(0)
    )
    desired_order <- c("ID", "AVAL", "DOSEVAR", "RRLT")
    mapping$AVAL <- "ID" # duplicate
    expect_error(apply_mapping(dataset, mapping, manual_units, column_groups, desired_order, silent = TRUE), "Duplicate column selection detected")
  })
})
