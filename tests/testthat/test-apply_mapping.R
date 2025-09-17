expected_df <- data.frame(
  USUBJID = 1:3,
  AVAL = 1:3,
  AVALU = "ng/mL",
  DOSE = 1:3,
  ADOSEDUR = 0,
  AFRLT = 1:3,
  NCA_PROFILE = factor(1:3)
)
var_labels(expected_df) <- c(
  "Unique Subject Identifier",
  "Analysis Value",
  "Analysis Value Unit",
  "DOSE",
  "Actual Duration of Treatment Dose",
  "Act. Rel. Time from Analyte First Dose",
  "NCA_PROFILE"
)
desired_order <- names(expected_df)

describe("apply_mapping", {
  it("renames columns correctly", {
    test_df <- expected_df
    names(test_df) <- c("id", "CONC", "CONC_unit", "dose", "dosedur", "TIME", "DOSNO")
    mapping <- as.list(setNames(names(test_df), names(expected_df)))
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_equal(result_df, expected_df)
  })
  
  it ("informs of the changes when silent is FALSE", {
    test_df <- expected_df
    names(test_df) <- c("id", "CONC", "CONC_unit", "dose", "dosedur", "TIME", "DOSNO")
    mapping <- as.list(setNames(names(test_df), names(expected_df)))
    expect_message(
      apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order, silent = FALSE),
      message(paste0(paste0("* ", names(mapping), " -> ", unname(mapping)), collapse = "\n"))
    )
  })

  it("creates columns when its map reference is not a column name in the dataset", {
    test_df <- expected_df[, !names(expected_df) %in% c("AVALU")]
    mapping <- as.list(setNames(names(test_df), names(test_df)))
    mapping["AVALU"] <- "ng/mL"
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_equal(result_df, expected_df)
  })

  it("orders the columns correctly", {
    wrong_order_cols <- c("USUBJID", "AFRLT", "NCA_PROFILE", "DOSE", "ADOSEDUR", "AVALU", "AVAL")
    test_df <- expected_df[, wrong_order_cols]
    mapping <- as.list(setNames(names(test_df), names(test_df)))
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_equal(names(result_df), desired_order)
  })

  it("apply labels correctly", {
    test_df <- expected_df
    names(test_df) <- c("id", "CONC", "CONC_unit", "dose", "dosedur", "TIME", "DOSNO")
    var_labels(test_df) <- rep(NA_character_, length(names(test_df)))
    mapping <- as.list(setNames(names(test_df), names(expected_df)))
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_equal(var_labels(result_df), var_labels(expected_df))
  })

  it("adds ADOSEDUR = 0 if not mapped", {
    test_df <- expected_df[, !names(expected_df) %in% "ADOSEDUR"]
    mapping <- as.list(setNames(names(test_df), names(test_df)))
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_equal(result_df, expected_df)
  })

  it("removes concentration duplicates", {
    test_df <- rbind(expected_df, expected_df)
    mapping <- as.list(setNames(names(test_df), names(test_df)))
    expect_warning(
      apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order),
      "Duplicate concentration data detected and filtered"
    )
    result_df <- suppressWarnings(
      apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    )
    expect_equal(result_df, expected_df)
  })

  it("throws error if mapping includes duplicates or empty characters", {
    test_df <- expected_df
    mapping <- as.list(setNames(names(test_df), names(test_df)))

    mapping_with_dup <- c(mapping, mapping["AVAL"])
    expect_error(
      apply_mapping(dataset = test_df, mapping = mapping_with_dup, desired_order = desired_order),
      "Duplicate column selection detected."
    )

    mapping_with_empty_char <- mapping
    mapping_with_empty_char["AVAL"] <- ""
    expect_error(
      apply_mapping(
        dataset = test_df, mapping = mapping_with_empty_char, desired_order = desired_order
      ),
      "Unmapped columns detected."
    )
  })

  it("makes sure NCA_PROFILE is a factor", {
    test_df <- expected_df
    mapping <- as.list(setNames(names(test_df), names(expected_df)))
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_true(is.factor(result_df$NCA_PROFILE))
    expect_equal(result_df, expected_df)
  })
})
