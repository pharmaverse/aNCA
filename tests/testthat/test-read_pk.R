describe("read_pk", {
  it("reads csv data correctly", {
    df <- read_pk("data/adnca_dummy_sm_dataset.csv")
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 131)
  })

  it("reads rds data correctly", {
    df <- read_pk("data/adnca_dummy_sm_dataset.rds")
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 131)
  })

  it("throws an error if file does not exist", {
    expect_error(read_pk("invalid_path.csv"), "File does not exist: ")
  })

  it("throws an error if file with unsupported format is loaded", {
    unsupported_path <- tempfile(fileext = ".txt")
    expect_error(read_pk(unsupported_path, "Invalid file type."))

  })
})