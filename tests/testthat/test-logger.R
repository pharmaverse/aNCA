describe("application log files", {
  logger_env <- new.env(parent = asNamespace("logger"))
  source(system.file("shiny/functions/utils.R", package = "aNCA"), local = logger_env)
  # Capture configuration locally instead of changing the test process's logger.
  ignore_config <- function(...) NULL
  logger_env$log_layout <- logger_env$log_formatter <- ignore_config
  logger_env$log_threshold <- ignore_config
  appenders <- list()
  logger_env$log_appender <- function(appender, index = 1) {
    appenders[[index]] <<- appender
  }

  it("uses a temporary directory and leaves the working directory untouched", {
    withr::local_envvar(aNCA_LOG_DIR = NA)
    working <- withr::local_tempdir()
    withr::local_dir(working)
    logger_env$setup_logger()
    appenders[[1]]("test log entry")
    expect_false(dir.exists(file.path(working, "log")))
    logs <- list.files(file.path(tempdir(), "aNCA", "logs"), full.names = TRUE)
    expect_true(any(vapply(logs, function(f) "test log entry" %in% readLines(f), logical(1))))
  })

  it("honors an explicit directory and rotates only aNCA log files", {
    directory <- withr::local_tempdir()
    logs <- file.path(directory, paste0("aNCA_app_230101-010101-", 1:7, ".log"))
    file.create(logs)
    for (i in seq_along(logs)) Sys.setFileTime(logs[i], as.POSIXct("2023-01-01") + i)
    other <- file.path(directory, c("notes.txt", "other.log"))
    file.create(other)
    withr::local_envvar(aNCA_LOG_DIR = directory)
    logger_env$setup_logger()
    appenders[[1]]("new entry")
    expect_true(all(file.exists(other)))
    expect_false(any(file.exists(logs[1:3])))
    expect_true(all(file.exists(logs[4:7])))
    expect_length(list.files(directory, pattern = "^aNCA_app_"), 5)
    logger_env$setup_logger()
    appenders[[1]]("second entry")
    expect_length(list.files(directory, pattern = "^aNCA_app_"), 5)
  })
})
