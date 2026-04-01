describe("run_app: settings validation", {
  it("rejects non-YAML extension", {
    tmp <- withr::local_tempfile(fileext = ".txt")
    writeLines("test", tmp)
    expect_error(
      run_app(settings = tmp),
      "Settings file must have .yaml or .yml extension"
    )
  })

  it("rejects non-existent file", {
    expect_error(
      run_app(settings = "nonexistent_file.yaml"),
      "Settings file does not exist"
    )
  })

  it("accepts .yml extension", {
    tmp <- withr::local_tempfile(fileext = ".yml")
    yaml::write_yaml(list(settings = list(method = "linear")), tmp)
    # Should not error on validation (will error on runApp since not interactive)
    expect_error(
      run_app(settings = tmp),
      "shiny"
    )
  })

  it("rejects invalid settings_version type (logical)", {
    tmp <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(list(settings = list(method = "linear")), tmp)
    expect_error(
      run_app(settings = tmp, settings_version = TRUE),
      "settings_version must be a single integer or character string"
    )
  })

  it("rejects multi-element settings_version", {
    tmp <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(list(settings = list(method = "linear")), tmp)
    expect_error(
      run_app(settings = tmp, settings_version = c(1, 2)),
      "settings_version must be a single integer or character string"
    )
  })

  it("sets and cleans up options correctly", {
    tmp <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(list(settings = list(method = "linear")), tmp)

    # Ensure options are NULL before
    old_s <- getOption("aNCA.settings")
    old_v <- getOption("aNCA.settings_version")
    on.exit({
      options(aNCA.settings = old_s, aNCA.settings_version = old_v)
    }, add = TRUE)

    # run_app will fail at shiny::runApp, but options should be set then cleaned
    tryCatch(
      run_app(settings = tmp, settings_version = 2),
      error = function(e) NULL
    )

    # After run_app returns (via error), options should be restored
    expect_null(getOption("aNCA.settings"))
    expect_null(getOption("aNCA.settings_version"))
  })
})
