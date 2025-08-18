describe("read_pk", {
  # load dummy testing data avaialable in csv format, used to simulate other formats #
  data_dummy <- read.csv("data/adnca_dummy_sm_dataset.csv")

  it("reads csv data correctly", {
    df <- read_pk("data/adnca_dummy_sm_dataset.csv")
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
    tmp_xlsx <- withr::local_tempfile(fileext = ".xlsx")
    openxlsx2::write_xlsx(data_dummy, tmp_xlsx)

    df <- read_pk(tmp_xlsx)

    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 131)
  })

  it("reads sas data correctly", {
    tmp_sas <- withr::local_tempfile(fileext = ".sas7bdat")
    suppressWarnings(haven::write_sas(data_dummy, tmp_sas))

    df <- read_pk(tmp_sas)

    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 131)
  })

  it("reads xpt files correctly", {
    tmp_xpt <- withr::local_tempfile(fileext = ".xpt")
    haven::write_xpt(data_dummy, tmp_xpt)

    df <- read_pk(tmp_xpt)

    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 131)
  })

  it("reads parquet files correctly", {
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
