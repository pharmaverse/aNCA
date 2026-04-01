describe("run_app: datapath validation", {
  it("rejects non-existent data file", {
    expect_error(
      run_app(datapath = "nonexistent_file.csv"),
      "Data file does not exist"
    )
  })

  it("rejects non-csv/rds extension", {
    tmp <- withr::local_tempfile(fileext = ".txt")
    writeLines("test", tmp)
    expect_error(
      run_app(datapath = tmp),
      "Data file must have .csv or .rds extension"
    )
  })

  it("accepts .csv extension", {
    tmp <- withr::local_tempfile(fileext = ".csv")
    writeLines("A,B\n1,2", tmp)
    expect_error(
      run_app(datapath = tmp),
      "shiny"
    )
  })

  it("accepts .rds extension", {
    tmp <- withr::local_tempfile(fileext = ".rds")
    saveRDS(data.frame(A = 1), tmp)
    expect_error(
      run_app(datapath = tmp),
      "shiny"
    )
  })
})

describe("run_app: settings validation", {
  it("rejects non-existent settings file", {
    expect_error(
      run_app(settings = "nonexistent_file.yaml"),
      "Settings file does not exist"
    )
  })

  it("rejects non-YAML extension", {
    tmp <- withr::local_tempfile(fileext = ".txt")
    writeLines("test", tmp)
    expect_error(
      run_app(settings = tmp),
      "Settings file must have .yaml or .yml extension"
    )
  })

  it("accepts .yml extension", {
    tmp <- withr::local_tempfile(fileext = ".yml")
    yaml::write_yaml(list(settings = list(method = "linear")), tmp)
    expect_error(
      run_app(settings = tmp),
      "shiny"
    )
  })

  it("accepts .yaml extension", {
    tmp <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(list(settings = list(method = "linear")), tmp)
    expect_error(
      run_app(settings = tmp),
      "shiny"
    )
  })

  it("sets and cleans up options correctly", {
    tmp <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(list(settings = list(method = "linear")), tmp)

    old_s <- getOption("aNCA.settings")
    old_v <- getOption("aNCA.settings_version")
    on.exit({
      options(aNCA.settings = old_s, aNCA.settings_version = old_v)
    }, add = TRUE)

    tryCatch(
      run_app(settings = tmp, settings_version = 2),
      error = function(e) NULL
    )

    # After run_app returns (via error), options should be restored
    expect_null(getOption("aNCA.settings"))
    expect_null(getOption("aNCA.settings_version"))
  })
})

describe("run_app: settings_version validation", {
  it("rejects invalid settings_version type (logical) even without settings", {
    expect_error(
      run_app(settings_version = TRUE),
      "settings_version must be a single integer or character string"
    )
  })

  it("rejects multi-element settings_version even without settings", {
    expect_error(
      run_app(settings_version = c(1, 2)),
      "settings_version must be a single integer or character string"
    )
  })

  it("rejects NULL settings_version", {
    expect_error(
      run_app(settings_version = NULL),
      "settings_version must be a single integer or character string"
    )
  })

  it("rejects list settings_version", {
    expect_error(
      run_app(settings_version = list(1)),
      "settings_version must be a single integer or character string"
    )
  })
})
