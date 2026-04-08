describe("read_pk", {
  # load dummy testing data available in csv format, used to simulate other formats #
  data_dummy <- read.csv("data/test-multispec-ADNCA.csv")

  it("reads csv data correctly", {
    df <- read_pk("data/test-multispec-ADNCA.csv")
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 131)
  })

  it("reads rds data correctly", {
    tmp_rds <- withr::local_tempfile(fileext = ".rds")
    saveRDS(data_dummy, tmp_rds)

    df <- read_pk(tmp_rds)

    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 131)
  })

  it("reads excel data correctly", {
    skip_if_not_installed("readxl")
    skip_if_not_installed("writexl") # used to write the test file
    skip_on_cran()

    tmp_xlsx <- withr::local_tempfile(fileext = ".xlsx")
    writexl::write_xlsx(data_dummy, tmp_xlsx)

    df <- read_pk(tmp_xlsx)

    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 131)
  })

  it("reads sas data correctly", {
    skip_if_not_installed("haven")
    skip_on_cran()

    tmp_sas <- withr::local_tempfile(fileext = ".sas7bdat")
    suppressWarnings(haven::write_sas(data_dummy, tmp_sas))

    df <- read_pk(tmp_sas)

    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 131)
  })

  it("reads xpt files correctly", {
    skip_if_not_installed("haven")
    skip_on_cran()

    tmp_xpt <- withr::local_tempfile(fileext = ".xpt")
    haven::write_xpt(data_dummy, tmp_xpt)

    df <- read_pk(tmp_xpt)

    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 131)
  })

  it("reads parquet files correctly", {
    skip_if_not_installed("arrow")
    skip_on_cran()

    tmp_parquet <- withr::local_tempfile(fileext = ".parquet")
    arrow::write_parquet(data_dummy, tmp_parquet)

    df <- read_pk(tmp_parquet)

    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 131)
  })

  it("throws an error if file does not exist", {
    expect_error(read_pk("invalid_path.csv"), "File does not exist: ")
  })

  it("throws an error if file with unsupported format is loaded", {
    unsupported_path <- withr::local_tempfile(fileext = ".txt")
    writeLines("test", unsupported_path)
    expect_error(read_pk(unsupported_path), "Invalid file type.")
  })

  it("throws an error if loaded object is not a data frame", {
    tmp_rds_list <- withr::local_tempfile(fileext = ".rds")
    saveRDS(list(a = 1), tmp_rds_list)

    expect_error(read_pk(tmp_rds_list), "Invalid data format. Data frame was expected")
  })

  it("throws an error if loaded data frame has no rows", {
    tmp_rds_empty_frame <- withr::local_tempfile(fileext = ".rds")
    saveRDS(data.frame(), tmp_rds_empty_frame)

    expect_error(
      read_pk(tmp_rds_empty_frame),
      "Empty data frame received, please check the input file."
    )
  })
})

describe("read_settings", {

  path <- testthat::test_path("data/test-settings.yaml")

  it("successfully reads a valid settings YAML", {

    res <- read_settings(path)

    expect_type(res, "list")
    expect_equal(names(res), c("settings", "slope_rules"))

  })

  ##################################################
  # TODO: Once the new implementation is integrated,
  # change the tests for slope rules
  ##################################################
  it("converts slope_rules to a data.frame", {
    res <- read_settings(path)
    expect_s3_class(res$slope_rules, "data.frame")
  })

  it("converts int_parameters to a data frame when present", {
    res <- read_settings(path)
    expect_s3_class(res$settings$int_parameters, "data.frame")
    expect_equal(nrow(res$settings$int_parameters), 2)
    expect_named(res$settings$int_parameters, c("parameter", "start_auc", "end_auc"))
    expect_equal(res$settings$int_parameters$parameter, c("AUCINT", "AUCINTD"))
    expect_equal(res$settings$int_parameters$start_auc, c(0, 0))
    expect_equal(res$settings$int_parameters$end_auc, c(24, 24))
  })

  it("returns NULL int_parameters when not in settings file", {
    tmp_yaml <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(list(settings = list(method = "linear")), tmp_yaml)
    res <- read_settings(tmp_yaml)
    expect_null(res$settings$int_parameters)
  })

  it("converts ratio_table to a data.frame when present", {
    res <- read_settings(path)
    expect_s3_class(res$settings$ratio_table, "data.frame")
    expect_equal(nrow(res$settings$ratio_table), 2)
    expect_named(res$settings$ratio_table, c(
      "TestParameter", "RefParameter", "RefGroups", "TestGroups",
      "AggregateSubject", "AdjustingFactor", "PPTESTCD"
    ))
    expect_equal(res$settings$ratio_table$PPTESTCD, c("MRCMAX", "FABS"))
    expect_equal(res$settings$ratio_table$AdjustingFactor, c(1, 1))
  })

  it("returns NULL ratio_table when not in settings file", {
    tmp_yaml <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(list(settings = list(method = "linear")), tmp_yaml)
    res <- read_settings(tmp_yaml)
    expect_null(res$settings$ratio_table)
  })

  it("keeps types_df as-is (only needed for R script generation)", {
    res <- read_settings(path)
    expect_type(res$settings$parameters$types_df, "list")
  })

  it("handles empty or null units/general_exclusions gracefully", {
    res <- read_settings(path)

    expect_null(res$settings$units)
  })

  it("throws error on invalid YAML structure", {
    bad_yaml <- tempfile(fileext = ".yaml")
    yaml::write_yaml(list(wrong_root = 1), bad_yaml)

    expect_error(read_settings(bad_yaml),
                 "not appear to be a valid settings YAML file")
  })

  it("successfully reads settings when optional sections are missing", {
    tmp_yaml <- withr::local_tempfile(fileext = ".yaml")
    # Minimal valid settings file (missing units, slope_rules, etc.)
    yaml::write_yaml(list(settings = list(other_param = "test")), tmp_yaml)

    res <- read_settings(tmp_yaml)

    expect_equal(res$settings$other_param, "test")
    # Ensure it didn't crash on the NULL checks for missing keys
    expect_null(res$content$slope_rules)
  })

  it("parses filters from YAML and converts values to vectors", {
    tmp_yaml <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(list(
      filters = list(
        list(column = "DOSEA", condition = "==", value = list("100", "200")),
        list(column = "AGE", condition = ">", value = list("18"))
      ),
      settings = list(method = "linear")
    ), tmp_yaml)

    res <- read_settings(tmp_yaml)

    expect_type(res$filters, "list")
    expect_length(res$filters, 2)
    expect_equal(res$filters[[1]]$column, "DOSEA")
    expect_equal(res$filters[[1]]$condition, "==")
    expect_equal(res$filters[[1]]$value, c("100", "200"))
    expect_equal(res$filters[[2]]$column, "AGE")
    expect_equal(res$filters[[2]]$condition, ">")
    expect_equal(res$filters[[2]]$value, "18")
  })

  it("returns NULL filters when not present in settings file", {
    tmp_yaml <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(list(settings = list(method = "linear")), tmp_yaml)

    res <- read_settings(tmp_yaml)

    expect_null(res$filters)
  })

  it("parses time_duplicate_keys as a data.frame", {
    tmp_yaml <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(list(
      settings = list(method = "linear"),
      time_duplicate_keys = list(
        list(AFRLT = 1, STUDYID = "S1", PCSPEC = "PLASMA",
             DOSETRT = "D1", USUBJID = "U1", PARAM = "P1", AVAL = 10),
        list(AFRLT = 2, STUDYID = "S1", PCSPEC = "PLASMA",
             DOSETRT = "D1", USUBJID = "U1", PARAM = "P1", AVAL = 20)
      )
    ), tmp_yaml)

    res <- read_settings(tmp_yaml)

    expect_s3_class(res$time_duplicate_keys, "data.frame")
    expect_equal(nrow(res$time_duplicate_keys), 2)
    expect_equal(res$time_duplicate_keys$USUBJID, c("U1", "U1"))
    expect_equal(res$time_duplicate_keys$AVAL, c(10, 20))
  })

  it("returns NULL time_duplicate_keys when not present", {
    tmp_yaml <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(list(settings = list(method = "linear")), tmp_yaml)

    res <- read_settings(tmp_yaml)

    expect_null(res$time_duplicate_keys)
  })

  it("round-trips time_duplicate_keys through versioned settings", {
    tmp_yaml <- withr::local_tempfile(fileext = ".yaml")
    keys_df <- data.frame(
      AFRLT = c(1, 2), STUDYID = c("S1", "S1"), PCSPEC = c("PLASMA", "PLASMA"),
      DOSETRT = c("D1", "D1"), USUBJID = c("U1", "U1"),
      PARAM = c("P1", "P1"), AVAL = c(10, 20),
      stringsAsFactors = FALSE
    )
    payload <- list(
      settings = list(method = "linear"),
      time_duplicate_keys = keys_df
    )
    version <- create_settings_version(payload, comment = "test")
    write_versioned_settings(list(version), tmp_yaml)

    res <- read_settings(tmp_yaml)

    expect_s3_class(res$time_duplicate_keys, "data.frame")
    expect_equal(nrow(res$time_duplicate_keys), 2)
    expect_equal(res$time_duplicate_keys$AVAL, c(10, 20))
  })
})

describe(".convert_list_to_df", {
  it("converts a list to a data.frame", {
    input <- list(list(a = 1, b = "x"), list(a = 2, b = "y"))
    result <- .convert_list_to_df(input)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
    expect_equal(result$a, c(1, 2))
  })

  it("returns NULL for NULL input", {
    expect_null(.convert_list_to_df(NULL))
  })

  it("returns non-list input unchanged", {
    expect_equal(.convert_list_to_df("text"), "text")
  })
})

describe(".convert_filter_values", {
  it("converts list values to vectors", {
    input <- list(
      list(column = "A", condition = "==", value = list("1", "2")),
      list(column = "B", condition = ">", value = list("10"))
    )
    result <- .convert_filter_values(input)
    expect_equal(result[[1]]$value, c("1", "2"))
    expect_equal(result[[2]]$value, "10")
  })

  it("returns NULL for NULL input", {
    expect_null(.convert_filter_values(NULL))
  })

  it("preserves other filter fields", {
    input <- list(list(column = "X", condition = "!=", value = list("a")))
    result <- .convert_filter_values(input)
    expect_equal(result[[1]]$column, "X")
    expect_equal(result[[1]]$condition, "!=")
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

# Source the validation helper from the Shiny app code
source(
  system.file("shiny/modules/tab_nca/setup/ratio_calculations_table.R",
              package = "aNCA"),
  local = TRUE
)

describe(".validate_ratio_row", {
  param_options <- c("CMAX", "AUCLAST", "AUCINF.OBS")
  ref_options <- c("ANALYTE: DrugA", "ANALYTE: DrugB", "ROUTE: INTRAVASCULAR")
  all_group_options <- c(ref_options, "(all other levels)")
  valid_agg <- c("yes", "no", "if-needed")

  valid_row <- data.frame(
    TestParameter = "CMAX",
    RefParameter = "CMAX",
    RefGroups = "ANALYTE: DrugA",
    TestGroups = "(all other levels)",
    AggregateSubject = "no",
    AdjustingFactor = 1,
    PPTESTCD = "MRCMAX",
    stringsAsFactors = FALSE
  )

  it("returns empty reasons for a valid row", {
    reasons <- .validate_ratio_row(
      valid_row, param_options, ref_options, all_group_options, valid_agg
    )
    expect_length(reasons, 0)
  })

  it("flags invalid TestParameter", {
    bad <- valid_row
    bad$TestParameter <- "FAKE"
    reasons <- .validate_ratio_row(
      bad, param_options, ref_options, all_group_options, valid_agg
    )
    expect_length(reasons, 1)
    expect_true(grepl("TestParameter", reasons))
  })

  it("flags invalid RefParameter", {
    bad <- valid_row
    bad$RefParameter <- "FAKE"
    reasons <- .validate_ratio_row(
      bad, param_options, ref_options, all_group_options, valid_agg
    )
    expect_length(reasons, 1)
    expect_true(grepl("RefParameter", reasons))
  })

  it("flags invalid RefGroups", {
    bad <- valid_row
    bad$RefGroups <- "UNKNOWN: X"
    reasons <- .validate_ratio_row(
      bad, param_options, ref_options, all_group_options, valid_agg
    )
    expect_length(reasons, 1)
    expect_true(grepl("RefGroups", reasons))
  })

  it("accepts (all other levels) as TestGroups", {
    reasons <- .validate_ratio_row(
      valid_row, param_options, ref_options, all_group_options, valid_agg
    )
    expect_length(reasons, 0)
  })

  it("flags invalid TestGroups", {
    bad <- valid_row
    bad$TestGroups <- "NONEXISTENT: X"
    reasons <- .validate_ratio_row(
      bad, param_options, ref_options, all_group_options, valid_agg
    )
    expect_length(reasons, 1)
    expect_true(grepl("TestGroups", reasons))
  })

  it("flags invalid AggregateSubject", {
    bad <- valid_row
    bad$AggregateSubject <- "invalid"
    reasons <- .validate_ratio_row(
      bad, param_options, ref_options, all_group_options, valid_agg
    )
    expect_length(reasons, 1)
    expect_true(grepl("AggregateSubject", reasons))
  })

  it("flags non-numeric AdjustingFactor", {
    bad <- valid_row
    bad$AdjustingFactor <- "abc"
    reasons <- .validate_ratio_row(
      bad, param_options, ref_options, all_group_options, valid_agg
    )
    expect_length(reasons, 1)
    expect_true(grepl("AdjustingFactor", reasons))
  })

  it("collects multiple reasons for a row with several issues", {
    bad <- valid_row
    bad$TestParameter <- "FAKE"
    bad$RefGroups <- "UNKNOWN: X"
    bad$AggregateSubject <- "invalid"
    reasons <- .validate_ratio_row(
      bad, param_options, ref_options, all_group_options, valid_agg
    )
    expect_length(reasons, 3)
  })
})
