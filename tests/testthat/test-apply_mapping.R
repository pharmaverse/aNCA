expected_df <- data.frame(
  USUBJID = 1:3,
  PARAM = "A",
  DRUG = "A",
  AVAL = 1:3,
  AVALU = "ng/mL",
  DOSE = 1:3,
  ADOSEDUR = 0,
  AFRLT = 1:3,
  NCA_PROFILE = factor(1:3)
)
var_labels(expected_df) <- c(
  "Unique Subject Identifier",
  "Parameter",
  "Medication Name",
  "Analysis Value",
  "Analysis Value Unit",
  "DOSE",
  "Actual Duration of Treatment Dose",
  "Act. Rel. Time from Analyte First Dose",
  "NCA_PROFILE"
)
desired_order <- names(expected_df)

# Create the general test scenario
test_df <- expected_df
names(test_df) <- c(
  "id", "analyte", "drug", "CONC", "CONC_unit",
  "dose", "dosedur", "TIME", "DOSNO"
)
var_labels(test_df) <- rep(NA_character_, ncol(test_df))
mapping <- as.list(setNames(names(test_df), names(expected_df)))

describe("apply_mapping", {
  it("renames columns correctly", {
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_equal(result_df, expected_df)
  })

  it("informs of the changes when silent is FALSE", {
    expect_message(
      apply_mapping(
        dataset = test_df, mapping = mapping, desired_order = desired_order, silent = FALSE
      ),
      message(paste0(paste0("* ", names(mapping), " -> ", unname(mapping)), collapse = "\n"))
    )
  })

  it("creates columns when its map reference is not a column name in the dataset", {
    test_df <- test_df[, !names(expected_df) %in% c("AVALU")]
    mapping["AVALU"] <- "ng/mL"
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_equal(result_df, expected_df)
  })

  it("orders the columns correctly", {
    wrong_order_cols <- rev(desired_order)
    test_df <- expected_df[, wrong_order_cols]
    mapping <- as.list(setNames(names(test_df), names(test_df)))
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_equal(names(result_df), desired_order)
  })

  it("returns an error if any required column is not mapped", {
    mapping["AVAL"] <- ""
    expect_error(
      apply_mapping(
        dataset = test_df,
        mapping = mapping,
        desired_order = desired_order,
        req_mappings = names(expected_df)
      ),
      "Unmapped required columns detected: AVAL"
    )
  })

  it("apply labels correctly when missing", {
    var_labels(test_df) <- rep(NA_character_, length(names(test_df)))
    mapping <- as.list(setNames(names(test_df), names(expected_df)))
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_equal(var_labels(result_df), var_labels(expected_df))
  })

  it("adds ADOSEDUR = 0 if not mapped and warns the user", {
    test_df <- test_df[, !names(expected_df) == "ADOSEDUR"]
    mapping["ADOSEDUR"] <- NULL
    expect_warning(
      result_df <- apply_mapping(
        dataset = test_df,
        mapping = mapping,
        desired_order = desired_order
      ),
      "Dose duration is assumed to be 0  for all records \\(ADOSEDUR = 0"
    )
    expect_equal(result_df, expected_df)
  })

  it("adds DRUG = PARAM if not mapped and warns the user", {
    mapping["DRUG"] <- ""
    test_df <- test_df[, !names(expected_df) %in% "DRUG"]
    expect_warning(
      result_df <- apply_mapping(
        dataset = test_df,
        mapping = mapping,
        desired_order = desired_order
      ),
      "Drug is assumed to be the same as the analyte for all records \\(DRUG = PARAM"
    )
    expect_equal(result_df, expected_df)
  })

  it("removes concentration duplicates only if exact", {
    test_df <- rbind(expected_df, expected_df %>% mutate(AVAL = 1))
    filtered_df <- test_df %>%
      filter(!duplicated(paste(USUBJID, AVAL))) %>%
      arrange(USUBJID, AVAL)
    
    # add label attr to NCA profile
    var_labels(filtered_df) <- var_labels(expected_df)
    
    mapping <- as.list(setNames(names(test_df), names(test_df)))
    expect_warning(
      apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order),
      "Duplicate concentration data detected and filtered"
    )
    result_df <- suppressWarnings(
      apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    )
    expect_equal(result_df, filtered_df)
  })

  it("allows duplicated mappings", {
    mapping["PARAM"] <- mapping["DRUG"]
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_equal(result_df[, names(result_df) != "analyte"], expected_df, ignore_attr = TRUE)
    # NOTE: uDplicated mapped columns will conserve duplicated labels if originally present
  })

  it("makes sure NCA_PROFILE is a factor", {
    test_df <- expected_df
    mapping <- as.list(setNames(names(test_df), names(expected_df)))
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_true(is.factor(result_df$NCA_PROFILE))
    expect_equal(result_df, expected_df)
  })

  it("warns the user about columns lost after mapping due to column name conflicts", {
    test_df$DRUG <- "Medication A"
    expect_warning(
      apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order),
      "Conflictive column names between input and mapping names are removed: DRUG"
    )
  })
})
