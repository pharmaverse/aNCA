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

  it("converts slope_rules elements into data frames", {
    res <- read_settings(path)
    expect_s3_class(res$slope_rules$manual_slopes, "data.frame")
    expect_s3_class(res$slope_rules$profiles_per_subject, "data.frame")
    # Check dimensions for profiles_per_subject
    expect_equal(nrow(res$slope_rules$profiles_per_subject), 23)
  })

  it("converts settings elements into data frames", {
    # Partial AUC definitions
    res <- read_settings(path)
    expect_s3_class(res$settings$partial_aucs, "data.frame")
    
    # Parameter selections by study type
    expect_s3_class(res$settings$parameters$types_df, "data.frame")
    expect_equal(nrow(res$settings$parameters$types_df), 3)
  })

  it("handles empty or null units/general_exclusions gracefully", {
    res <- read_settings(path)

    expect_s3_class(res$settings$units, "data.frame")
    expect_equal(nrow(res$settings$units), 0)
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
