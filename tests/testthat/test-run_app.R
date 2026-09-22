describe("run_app input validation", {
  it("errors when datapath does not exist", {
    expect_error(
      run_app(datapath = "/nonexistent/file.csv"),
      "Data file does not exist"
    )
  })

  it("errors when datapath has wrong extension", {
    tmp <- tempfile(fileext = ".txt")
    writeLines("x", tmp)
    on.exit(unlink(tmp))

    expect_error(
      run_app(datapath = tmp),
      "Data file must have .csv or .rds extension"
    )
  })

  it("errors when settings file does not exist", {
    expect_error(
      run_app(settings = "/nonexistent/settings.yaml"),
      "Settings file does not exist"
    )
  })

  it("errors when settings file has wrong extension", {
    tmp <- tempfile(fileext = ".json")
    writeLines("{}", tmp)
    on.exit(unlink(tmp))

    expect_error(
      run_app(settings = tmp),
      "Settings file must have .yaml or .yml extension"
    )
  })

  it("errors when settings_version is not a single value", {
    expect_error(
      run_app(settings_version = c(1, 2)),
      "settings_version must be a single integer or character string"
    )
  })

  it("errors when settings_version is not numeric or character", {
    expect_error(
      run_app(settings_version = TRUE),
      "settings_version must be a single integer or character string"
    )
  })
})
