describe("create_settings_version", {
  it("creates a version entry with metadata and settings payload", {
    payload <- list(
      settings = list(method = "linear", analyte = "DrugA"),
      slope_rules = NULL,
      filters = NULL
    )
    v <- create_settings_version(
      payload,
      comment = "test run",
      dataset = "STUDY01",
      tab = "NCA"
    )
    expect_equal(v$comment, "test run")
    expect_equal(v$dataset, "STUDY01")
    expect_equal(v$tab, "NCA")
    expect_true(nzchar(v$datetime))
    expect_true(nzchar(v$anca_version))
    expect_equal(v$settings$method, "linear")
    expect_equal(v$settings$analyte, "DrugA")
  })

  it("uses empty strings as defaults", {
    v <- create_settings_version(list(settings = list()))
    expect_equal(v$comment, "")
    expect_equal(v$dataset, "")
    expect_equal(v$tab, "")
  })
})

describe("write_versioned_settings and read_versioned_settings", {
  it("round-trips a single version", {
    payload <- list(settings = list(method = "linear"), slope_rules = NULL)
    v <- create_settings_version(payload, comment = "v1")

    tmp <- tempfile(fileext = ".yaml")
    on.exit(unlink(tmp), add = TRUE)

    write_versioned_settings(list(v), tmp)
    result <- read_versioned_settings(tmp)

    expect_equal(result$format, "versioned")
    expect_length(result$versions, 1)
    expect_equal(result$versions[[1]]$comment, "v1")
    expect_equal(result$versions[[1]]$settings$method, "linear")
  })

  it("round-trips multiple versions", {
    v1 <- create_settings_version(
      list(settings = list(method = "linear")), comment = "first"
    )
    v2 <- create_settings_version(
      list(settings = list(method = "log")), comment = "second"
    )

    tmp <- tempfile(fileext = ".yaml")
    on.exit(unlink(tmp), add = TRUE)

    write_versioned_settings(list(v2, v1), tmp)
    result <- read_versioned_settings(tmp)

    expect_equal(result$format, "versioned")
    expect_length(result$versions, 2)
    expect_equal(result$versions[[1]]$comment, "second")
    expect_equal(result$versions[[2]]$comment, "first")
  })

  it("errors on empty versions list", {
    tmp <- tempfile(fileext = ".yaml")
    expect_error(
      write_versioned_settings(list(), tmp),
      "At least one version entry is required"
    )
  })
})

describe("read_versioned_settings with legacy format", {
  it("wraps legacy settings as a single version", {
    tmp <- tempfile(fileext = ".yaml")
    on.exit(unlink(tmp), add = TRUE)

    legacy <- list(
      settings = list(method = "linear"),
      slope_rules = NULL,
      filters = NULL
    )
    yaml::write_yaml(legacy, tmp)

    result <- read_versioned_settings(tmp)
    expect_equal(result$format, "legacy")
    expect_length(result$versions, 1)
    expect_equal(result$versions[[1]]$settings$method, "linear")
  })

  it("errors on invalid YAML structure", {
    tmp <- tempfile(fileext = ".yaml")
    on.exit(unlink(tmp), add = TRUE)

    yaml::write_yaml(list(foo = "bar"), tmp)
    expect_error(
      read_versioned_settings(tmp),
      "does not appear to be a valid settings YAML"
    )
  })
})

describe("add_settings_version", {
  it("prepends a new version", {
    v1 <- create_settings_version(list(settings = list()), comment = "old")
    v2 <- create_settings_version(list(settings = list()), comment = "new")

    result <- add_settings_version(list(v1), v2)
    expect_length(result, 2)
    expect_equal(result[[1]]$comment, "new")
    expect_equal(result[[2]]$comment, "old")
  })
})

describe("delete_settings_version", {
  it("removes a version by index", {
    v1 <- create_settings_version(list(settings = list()), comment = "a")
    v2 <- create_settings_version(list(settings = list()), comment = "b")
    v3 <- create_settings_version(list(settings = list()), comment = "c")

    result <- delete_settings_version(list(v1, v2, v3), 2)
    expect_length(result, 2)
    expect_equal(result[[1]]$comment, "a")
    expect_equal(result[[2]]$comment, "c")
  })

  it("errors when deleting the last version", {
    v1 <- create_settings_version(list(settings = list()), comment = "only")
    expect_error(
      delete_settings_version(list(v1), 1),
      "Cannot delete the last remaining version"
    )
  })

  it("errors on out-of-bounds index", {
    v1 <- create_settings_version(list(settings = list()), comment = "a")
    v2 <- create_settings_version(list(settings = list()), comment = "b")
    expect_error(
      delete_settings_version(list(v1, v2), 5),
      "Index out of bounds"
    )
  })
})

describe("settings_version_summary", {
  it("creates a summary data.frame", {
    v1 <- create_settings_version(
      list(settings = list()), comment = "first", dataset = "DS1", tab = "NCA"
    )
    v2 <- create_settings_version(
      list(settings = list()), comment = "second", dataset = "DS2", tab = "Setup"
    )

    df <- settings_version_summary(list(v1, v2))
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 2)
    expect_equal(df$comment, c("first", "second"))
    expect_equal(df$dataset, c("DS1", "DS2"))
    expect_equal(df$tab, c("NCA", "Setup"))
    expect_equal(df$index, c(1, 2))
  })
})

describe("extract_version_settings", {
  it("post-processes settings payload", {
    payload <- list(
      settings = list(
        method = "linear",
        units = list(
          list(PPTESTCD = "cmax", PPSTRESU = "ng/mL"),
          list(PPTESTCD = "tmax", PPSTRESU = "h")
        ),
        int_parameters = list(
          list(parameter = "AUCINT", start_auc = 0, end_auc = 24)
        )
      ),
      slope_rules = list(
        list(USUBJID = "S1", ATPTREF = "D1")
      ),
      filters = list(
        list(column = "USUBJID", condition = "==", value = list("S1", "S2"))
      )
    )

    version <- create_settings_version(payload)
    result <- extract_version_settings(version)

    expect_s3_class(result$slope_rules, "data.frame")
    expect_s3_class(result$settings$units, "data.frame")
    expect_s3_class(result$settings$int_parameters, "data.frame")
    expect_equal(result$settings$method, "linear")
    expect_equal(result$filters[[1]]$value, c("S1", "S2"))
  })

  it("returns NULL for version with only metadata", {
    version <- list(
      comment = "no data",
      datetime = "",
      dataset = "",
      anca_version = "",
      tab = ""
    )
    expect_warning(
      result <- extract_version_settings(version),
      "empty settings content"
    )
    expect_null(result)
  })
})

describe("read_settings with versioned format", {
  it("returns most recent version with versioned attribute", {
    v1 <- create_settings_version(
      list(settings = list(method = "linear"), slope_rules = NULL, filters = NULL),
      comment = "old"
    )
    v2 <- create_settings_version(
      list(settings = list(method = "log"), slope_rules = NULL, filters = NULL),
      comment = "new"
    )

    tmp <- tempfile(fileext = ".yaml")
    on.exit(unlink(tmp), add = TRUE)

    write_versioned_settings(list(v2, v1), tmp)
    result <- read_settings(tmp)

    expect_equal(result$settings$method, "log")

    va <- attr(result, "versioned")
    expect_false(is.null(va))
    expect_length(va$versions, 2)
    expect_equal(va$versions[[1]]$comment, "new")
  })
})

describe("extract_version_settings edge cases", {
  it("returns NULL and warns for version with no settings fields", {
    v <- list(
      comment = "empty",
      datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      dataset = "",
      anca_version = "",
      tab = ""
    )
    expect_warning(
      result <- extract_version_settings(v),
      "empty settings content"
    )
    expect_null(result)
  })
})

describe("read_versioned_settings edge cases", {
  it("sorts versions by timestamp descending", {
    old <- create_settings_version(
      list(settings = list(method = "old"), slope_rules = NULL, filters = NULL),
      comment = "first"
    )
    old$datetime <- "2020-01-01 00:00:00"

    new <- create_settings_version(
      list(settings = list(method = "new"), slope_rules = NULL, filters = NULL),
      comment = "second"
    )

    tmp <- tempfile(fileext = ".yaml")
    on.exit(unlink(tmp), add = TRUE)

    write_versioned_settings(list(old, new), tmp)
    result <- read_versioned_settings(tmp)

    expect_equal(result$versions[[1]]$comment, "second")
    expect_equal(result$versions[[2]]$comment, "first")
  })

  it("errors on YAML with no recognized format", {
    tmp <- tempfile(fileext = ".yaml")
    on.exit(unlink(tmp), add = TRUE)
    yaml::write_yaml(list(unrelated = "data"), tmp)
    expect_error(read_versioned_settings(tmp))
  })
})

describe("read_settings format detection", {
  it("treats a legacy file with a 'current' key but no datetime as legacy", {
    tmp <- tempfile(fileext = ".yaml")
    on.exit(unlink(tmp), add = TRUE)
    yaml::write_yaml(list(settings = list(method = "linear"), current = "v1"), tmp)
    result <- read_settings(tmp)
    expect_equal(result$settings$method, "linear")
    expect_null(attr(result, "versioned"))
  })
})

describe("settings_version_summary edge cases", {
  it("handles empty comment as empty string", {
    v <- create_settings_version(
      list(settings = list(method = "linear"), slope_rules = NULL, filters = NULL),
      comment = ""
    )
    summary_df <- settings_version_summary(list(v))
    expect_equal(summary_df$comment[1], "")
  })
})
