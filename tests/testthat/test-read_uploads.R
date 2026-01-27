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

describe("read_uploaded_file", {
  data_dummy <- read.csv("data/test-multispec-ADNCA.csv")
  
  it("successfully reads a PK data file (csv)", {
    tmp_csv <- withr::local_tempfile(fileext = ".csv")
    write.csv(data_dummy, tmp_csv, row.names = FALSE)
    
    res <- read_uploaded_file(tmp_csv, "test.csv")
    
    expect_equal(res$status, "success")
    expect_equal(res$type, "data")
    expect_s3_class(res$data, "data.frame")
  })
  
  it("successfully reads a valid settings YAML", {
    path <- "data/test-settings.yaml"
    skip_if_not(file.exists(path))
    
    res <- read_uploaded_file(path, "test-settings.yaml")
    
    expect_equal(res$status, "success")
    expect_equal(res$type, "settings")
    expect_true("settings" %in% names(res$content))
    expect_s3_class(res$content$slope_rules$profiles_per_subject, "data.frame")
  })
  
  it("returns error status for invalid file types", {
    tmp_txt <- withr::local_tempfile(fileext = ".txt")
    writeLines("random text", tmp_txt)
    
    res <- read_uploaded_file(tmp_txt, "test.txt")
    
    expect_equal(res$status, "error")
    expect_match(res$msg, "Invalid file type.")
  })
  
  it("falls back to PK error if YAML parsing returns NULL (invalid schema)", {
    tmp_yaml <- withr::local_tempfile(fileext = ".yaml")
    # Valid YAML syntax but missing 'settings' key required by .parse_settings_yaml
    yaml::write_yaml(list(wrong_key = 123), tmp_yaml)
    
    res <- read_uploaded_file(tmp_yaml, "wrong_schema.yaml")
    
    expect_equal(res$status, "error")
    # Should report the error from the initial read_pk attempt
    expect_match(res$msg, "Invalid file type") 
  })
  
  it("successfully reads settings when optional sections are missing", {
    tmp_yaml <- withr::local_tempfile(fileext = ".yaml")
    # Minimal valid settings file (missing units, slope_rules, etc.)
    yaml::write_yaml(list(settings = list(other_param = "test")), tmp_yaml)
    
    res <- read_uploaded_file(tmp_yaml, "minimal_settings.yaml")
    
    expect_equal(res$status, "success")
    expect_equal(res$type, "settings")
    expect_equal(res$content$settings$other_param, "test")
    # Ensure it didn't crash on the NULL checks for missing keys
    expect_null(res$content$slope_rules)
  })
})