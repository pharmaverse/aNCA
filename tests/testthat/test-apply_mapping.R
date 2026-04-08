expected_df <- data.frame(
  USUBJID = 1:3,
  PARAM = "A",
  DOSETRT = "A",
  AVAL = 1:3,
  AVALU = "ng/mL",
  DOSE = 1:3,
  ADOSEDUR = 0,
  AFRLT = 1:3,
  ATPTREF = factor(1:3)
)
var_labels(expected_df) <- c(
  "Unique Subject Identifier",
  "Parameter",
  "Name of Treatment",
  "Analysis Value",
  "Analysis Value Unit",
  "DOSE",
  "Actual Duration of Treatment Dose",
  "Act. Rel. Time from Analyte First Dose",
  "Analysis Timepoint Reference"
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

  it("adds DOSETRT = PARAM if not mapped and warns the user", {
    mapping["DOSETRT"] <- ""
    test_df <- test_df[, !names(expected_df) %in% "DOSETRT"]
    expect_warning(
      result_df <- apply_mapping(
        dataset = test_df,
        mapping = mapping,
        desired_order = desired_order
      ),
      "Treatment is assumed to be the same as the analyte for all records \\(DOSETRT = PARAM"
    )
    expect_equal(result_df, expected_df)
  })

  it("allows duplicated mappings", {
    mapping["PARAM"] <- mapping["DOSETRT"]
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_equal(result_df[, names(result_df) != "analyte"], expected_df, ignore_attr = TRUE)
    # NOTE: uDplicated mapped columns will conserve duplicated labels if originally present
  })

  it("makes sure ATPTREF is a factor", {
    test_df <- expected_df
    mapping <- as.list(setNames(names(test_df), names(expected_df)))
    result_df <- apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order)
    expect_true(is.factor(result_df$ATPTREF))
    expect_equal(result_df, expected_df)
  })

  it("warns the user about columns lost after mapping due to column name conflicts", {
    test_df$DOSETRT <- "Medication A"
    expect_warning(
      apply_mapping(dataset = test_df, mapping = mapping, desired_order = desired_order),
      "Conflictive column names between input and mapping names are removed: DOSETRT"
    )
  })
})

describe("create_metabflag", {
  it("creates the METABFL if indicates the metabolites", {
    test_df <- data.frame(
      USUBJID = 1:6,
      PARAM = c("A", "B", "M1", "M2", "C", "M1"),
      AVAL = c(10, 20, 5, 3, 15, 7)
    )
    expected_df <- test_df %>%
      mutate(METABFL = c("", "", "Y", "Y", "", "Y"))
    result_df <- create_metabfl(test_df, c("M1", "M2"))
    expect_equal(result_df, expected_df)
  })
  it("creates an empty METABFL if no metabolites are indicated", {
    test_df <- data.frame(
      USUBJID = 1:3,
      PARAM = c("A", "B", "C"),
      AVAL = c(10, 20, 15)
    )
    expected_df <- test_df %>%
      mutate(METABFL = c("", "", ""))
    result_df <- create_metabfl(test_df, character(0))
    result_df2 <- create_metabfl(test_df, "")
    result_df3 <- create_metabfl(test_df, NULL)
    expect_equal(result_df, expected_df)
    expect_equal(result_df2, expected_df)
    expect_equal(result_df3, expected_df)
  })
})

describe("extract_time_dup_keys", {
  test_data <- data.frame(
    AFRLT = c(1, 1, 2), STUDYID = c("S1", "S1", "S1"),
    PCSPEC = c("PLASMA", "PLASMA", "PLASMA"),
    DOSETRT = c("D1", "D1", "D1"), USUBJID = c("U1", "U1", "U2"),
    PARAM = c("P1", "P1", "P1"), AVAL = c(10, 20, 30),
    EXTRA_COL = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  it("extracts key columns for given row indices", {
    result <- extract_time_dup_keys(test_data, c(1, 2))
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
    expect_equal(names(result), TIME_DUP_KEY_COLS)
    expect_equal(result$AVAL, c(10, 20))
    expect_false("EXTRA_COL" %in% names(result))
  })

  it("returns NULL for NULL indices", {
    expect_null(extract_time_dup_keys(test_data, NULL))
  })

  it("returns NULL for empty indices", {
    expect_null(extract_time_dup_keys(test_data, integer(0)))
  })
})

describe("match_time_dup_keys", {
  test_data <- data.frame(
    AFRLT = c(1, 1, 2, 3), STUDYID = c("S1", "S1", "S1", "S1"),
    PCSPEC = c("PLASMA", "PLASMA", "PLASMA", "PLASMA"),
    DOSETRT = c("D1", "D1", "D1", "D1"),
    USUBJID = c("U1", "U1", "U2", "U3"),
    PARAM = c("P1", "P1", "P1", "P1"),
    AVAL = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )

  it("matches stored keys to row indices in the dataset", {
    keys_df <- data.frame(
      AFRLT = 1, STUDYID = "S1", PCSPEC = "PLASMA",
      DOSETRT = "D1", USUBJID = "U1", PARAM = "P1", AVAL = 20,
      stringsAsFactors = FALSE
    )
    result <- match_time_dup_keys(test_data, keys_df)
    expect_equal(result, 2L)
  })

  it("returns NULL when no keys match", {
    keys_df <- data.frame(
      AFRLT = 99, STUDYID = "S1", PCSPEC = "PLASMA",
      DOSETRT = "D1", USUBJID = "U1", PARAM = "P1", AVAL = 999,
      stringsAsFactors = FALSE
    )
    expect_null(match_time_dup_keys(test_data, keys_df))
  })

  it("returns NULL for NULL keys_df", {
    expect_null(match_time_dup_keys(test_data, NULL))
  })

  it("returns NULL for empty keys_df", {
    expect_null(match_time_dup_keys(test_data, data.frame()))
  })

  it("matches multiple keys", {
    keys_df <- data.frame(
      AFRLT = c(1, 2), STUDYID = c("S1", "S1"),
      PCSPEC = c("PLASMA", "PLASMA"), DOSETRT = c("D1", "D1"),
      USUBJID = c("U1", "U2"), PARAM = c("P1", "P1"),
      AVAL = c(10, 30),
      stringsAsFactors = FALSE
    )
    result <- match_time_dup_keys(test_data, keys_df)
    expect_equal(sort(result), c(1L, 3L))
  })
})

