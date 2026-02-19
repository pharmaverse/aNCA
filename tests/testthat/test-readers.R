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
    skip_if_not_installed("openxlsx2")
    skip_on_cran()

    tmp_xlsx <- withr::local_tempfile(fileext = ".xlsx")
    openxlsx2::write_xlsx(data_dummy, tmp_xlsx)

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

    res <- read_settings(path, "test-settings.yaml")

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

  it("keeps types_df as-is (only needed for R script generation)", {
    res <- read_settings(path)
    expect_type(res$settings$parameters$types_df, "list")
  })

  it("handles empty or null units/general_exclusions gracefully", {
    res <- read_settings(path)

    # units is ~ (null) in the test fixture, so it stays NULL
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

    res <- read_settings(tmp_yaml, "minimal_settings.yaml")

    expect_equal(res$settings$other_param, "test")
    # Ensure it didn't crash on the NULL checks for missing keys
    expect_null(res$content$slope_rules)
  })
})
