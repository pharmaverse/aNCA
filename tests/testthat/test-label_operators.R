ADNCA_LABELS_FIXTURE <- data.frame(
  Variable = c("USUBJID", "AVAL", "RACE"),
  Label =  c("Unique Subject Identifier", "Analysis Value", "Race"),
  Dataset = c("ADNCA", "ADNCA", "ADNCA")
)

data <- data.frame(
  USUBJID =  c("S1-1", "S1-2", "S1-3"),
  AVAL = c(10, 20, 30),
  RACE = as.factor(c("WHITE", "ASIAN", "ASIAN"))
)

describe("apply_labels", {
  labeled_data  <- expect_no_error(apply_labels(data, ADNCA_LABELS_FIXTURE))
  it("applies labels to the data frame", {
    expect_equal(base::attr(labeled_data$USUBJID, "label"), "Unique Subject Identifier")
    expect_equal(base::attr(labeled_data$AVAL, "label"), "Analysis Value")
    expect_equal(base::attr(labeled_data$RACE, "label"), "Race")
  })

  it("uses column names as labels when the variable is not in the labels list", {
    EMPTY_ADNCA_LABELS_FIXTURE  <- data.frame(
      Variable = character(),
      Label = character(),
      Dataset = character()
    )
    labeled_data <- expect_no_error(apply_labels(data, EMPTY_ADNCA_LABELS_FIXTURE))
    expect_equal(base::attr(labeled_data$USUBJID, "label"), "USUBJID")
    expect_equal(base::attr(labeled_data$AVAL, "label"), "AVAL")
    expect_equal(base::attr(labeled_data$RACE, "label"), "RACE")
  })


  it("does not change labels if already applied", {
    data_with_existing_labels <- data
    attr(data_with_existing_labels$USUBJID, "label") <- "Existing label for USUBJID"

    labeled_data <- apply_labels(data_with_existing_labels, ADNCA_LABELS_FIXTURE)
    expect_equal(base::attr(labeled_data$USUBJID, "label"), "Existing label for USUBJID")
    expect_equal(base::attr(labeled_data$AVAL, "label"), "Analysis Value")
  })
})

describe("get_label", {
  it("returns label of a heading if it exists in the label file", {
    expect_equal(get_label("USUBJID", "ADNCA", ADNCA_LABELS_FIXTURE), "Unique Subject Identifier")
  })

  it("returns the variable name if the label does not exist", {
    expect_equal(get_label("USUBJID", "ADP", ADNCA_LABELS_FIXTURE), "USUBJID")
  })
})

mock_vec <- c("A", "B", "C")
attr(mock_vec, "label") <- "Example Label"

describe("generate_tooltip_text", {
  TEST_DATA <- data.frame(
    USUBJID = c("S1-1", "S1-2"),
    AVAL = c(10.5, NA_real_),
    RACE = as.factor(c("WHITE", "ASIAN"))
  )
  TEST_VARS <- c("USUBJID", "AVAL", "RACE")

  it("generates correct tooltip string for multiple rows and data types", {
    tooltips <- generate_tooltip_text(TEST_DATA, ADNCA_LABELS_FIXTURE, TEST_VARS, "ADNCA")
    expected_output <- c(
      "<b>Unique Subject Identifier</b>: S1-1<br><b>Analysis Value</b>: 10.5<br><b>Race</b>: WHITE",
      "<b>Unique Subject Identifier</b>: S1-2<br><b>Analysis Value</b>: NA<br><b>Race</b>: ASIAN"
    )
    expect_equal(tooltips, expected_output)
  })

  it("returns an empty string for each row if tooltip_vars is empty", {
    tooltips <- generate_tooltip_text(TEST_DATA, ADNCA_LABELS_FIXTURE, character(0), "ADNCA")
    expect_equal(tooltips, c("", ""))
  })

  it("returns an empty vector for data with zero rows", {
    empty_data <- TEST_DATA[0, ]
    tooltips <- generate_tooltip_text(empty_data, ADNCA_LABELS_FIXTURE, TEST_VARS, "ADNCA")
    expect_equal(tooltips, character(0))
  })

  it("uses the variable name as a label if it's not in labels_df", {
    data_with_unlabeled_var <- TEST_DATA %>% mutate(AGE = c(45, 52))
    vars_with_unlabeled <- c("USUBJID", "AGE")
    expected_output <- c(
      "<b>Unique Subject Identifier</b>: S1-1<br><b>AGE</b>: 45",
      "<b>Unique Subject Identifier</b>: S1-2<br><b>AGE</b>: 52"
    )
    tooltips <- generate_tooltip_text(data_with_unlabeled_var, ADNCA_LABELS_FIXTURE,
                                      vars_with_unlabeled, "ADNCA")
    expect_equal(tooltips, expected_output)
  })
})

describe("add_label_attribute", {
  
  # Filter fixture to a manageable subset for testing
  myres_base <- FIXTURE_PKNCA_RES
  
  it("correctly assigns labels for standard (main) and manual intervals with units", {
    # 1. Create data with temp names, then rename to include brackets/units
    df_input <- data.frame(
      cmax = 1,
      auc1 = 1,
      auc2 = 1,
      tmax = 1,
      random = 1
    )
    names(df_input) <- c(
      "CMAX[ng/mL]", 
      "AUCINT_0-2[hr*ng/mL]", 
      "AUCINT_2-4[hr*ng/mL]", 
      "TMAX[hr]", 
      "RandomCol"
    )
    
    # 2. Apply function
    df_result <- add_label_attribute(df_input, myres_base)
    
    # 3. Assertions
    expect_equal(attr(df_result[["CMAX[ng/mL]"]], "label"), "Max Conc")
    expect_equal(attr(df_result[["TMAX[hr]"]], "label"), "Time of CMAX Observation")
    expect_equal(attr(df_result[["AUCINT_0-2[hr*ng/mL]"]], "label"), "AUC from T1 to T2")
    expect_null(attr(df_result[["RandomCol"]], "label"))
  })
  
  it("handles edge cases Manual/Main intervals WITHOUT units", {
    # Modify fixture to remove units
    myres_mod <- myres_base
    myres_mod$result <- myres_mod$result %>%
      mutate(
        PPSTRESU = case_when(
          PPTESTCD == "CMAX" ~ "", 
          PPTESTCD == "AUCINT" & start == 0 ~ "", 
          TRUE ~ PPSTRESU
        )
      )
    
    # Create input matching the modified unit-less names
    # Note: data.frame replaces hyphens with dots, so we must rename explicitly
    df_input_mod <- data.frame(
      CMAX = 1, 
      AUCINT = 1
    )
    names(df_input_mod) <- c("CMAX", "AUCINT_0-2")
    
    # Apply function
    df_result_mod <- add_label_attribute(df_input_mod, myres_mod)
    
    # Assertions
    expect_equal(attr(df_result_mod[["CMAX"]], "label"), "Max Conc")
    expect_equal(attr(df_result_mod[["AUCINT_0-2"]], "label"), "AUC from T1 to T2")
  })
  
  it("validates specific expected labels list", {
    expected_labels_map <- c(
      `CMAX[ng/mL]` = "Max Conc",
      `TMAX[hr]` = "Time of CMAX Observation",
      `TLST[hr]` = "Time of Last Nonzero Conc",
      `CLST[ng/mL]` = "Last Nonzero Conc",
      `LAMZ[1/hr]` = "Lambda z",
      `AUCINT_0-2[hr*ng/mL]` = "AUC from T1 to T2"
    )
    
    # Create DF with these columns
    df_input <- as.data.frame(matrix(NA, ncol = length(expected_labels_map), nrow = 1))
    names(df_input) <- names(expected_labels_map)
    
    df_result <- add_label_attribute(df_input, myres_base)
    
    for (col_name in names(expected_labels_map)) {
      expect_equal(
        attr(df_result[[col_name]], "label"), 
        expected_labels_map[[col_name]],
        info = paste("Checking label for", col_name)
      )
    }
  })
})