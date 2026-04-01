#' Run the Shiny app
#' @param datapath Full path to a single `.csv` or `.rds` data file.
#' @param settings Full path to a `.yaml` settings file.
#' @param settings_version Version selector for versioned settings files.
#'   Either an integer index (1 = most recent, 2 = second, etc.) or a
#'   character string matched against the version `comment` field.
#'   Defaults to `1L` (most recent). Ignored for non-versioned files.
#' @param ... Arguments passed to `shiny::runApp()`
#'
#' @returns No return value, called for side effects to launch the Shiny application.
#' @details
#' If a `datapath` is provided, the app will attempt to automatically load the
#' specified dataset on startup. This is achieved by setting an internal option
#' (`options(aNCA.datapath = datapath)`), which the app then reads.
#' **This pre-loaded dataset can be overwritten; if a new file is uploaded using
#' the widget within the app, it will replace the initial data for the current session.**
#'
#' If `datapath` is `NULL` (default), the app will launch without pre-loading any
#' data, and a file must be uploaded manually within the app.
#'
#' If a `settings` path is provided, the app will load the settings file on
#' startup and apply the saved configuration (analyte, method, units, etc.).
#' The pre-loaded settings can be overwritten by uploading a new settings file.
#'
#' For versioned settings files (containing `current` and `previous` entries),
#' the most recent version is loaded by default. Use `settings_version` to
#' select a different version by index or comment:
#'
#' ```
#' run_app(settings = "settings.yaml", settings_version = 2)
#' run_app(settings = "settings.yaml", settings_version = "NCA draft")
#' ```
#'
#' @examples
#' \donttest{
#'   # To actually launch the app, run interactively:
#'   if (interactive()) {
#'     run_app()
#'   }
#' }
#' @export
run_app <- function(datapath = NULL, settings = NULL,
                    settings_version = 1L, ...) {
  # Increase max upload size to 30 MB
  options(shiny.maxRequestSize = 30 * 1024^2)
  if (!is.null(datapath)) {
    opt <- options(aNCA.datapath = datapath)
    on.exit(options(opt), add = TRUE)
  }
  if (!is.null(settings)) {
    stopifnot(
      "Settings file must have .yaml or .yml extension" =
        tools::file_ext(settings) %in% c("yaml", "yml"),
      "Settings file does not exist" = file.exists(settings)
    )
    stopifnot(
      "settings_version must be a single integer or character string" =
        (is.numeric(settings_version) || is.character(settings_version)) &&
        length(settings_version) == 1
    )
    opt_s <- options(
      aNCA.settings = settings,
      aNCA.settings_version = settings_version
    )
    on.exit(options(opt_s), add = TRUE)
  }

  check_app_dependencies()
  shiny::runApp(system.file("shiny", package = "aNCA"), ...)
}

#' Check if all dependencies required to run shiny application are installed.
#' If not, install them.
#' This list of packages should also be provided as `Suggests` in the DESCRIPTION file.
#' @noRd
check_app_dependencies <- function() {
  deps <- c(
    "bslib",
    "dplyr",
    "htmlwidgets",
    "logger",
    "formatters",
    "magrittr",
    "plotly",
    "purrr",
    "reactable",
    "reactable.extras",
    "shiny",
    "shinycssloaders",
    "shinyjs",
    "shinyjqui",
    "shinyWidgets",
    "stats",
    "stringi",
    "tidyr",
    "tools",
    "utils",
    "rlang",
    "yaml"
  )

  missing_packages <- purrr::keep(deps, function(dep) !requireNamespace(dep, quietly = TRUE))

  if (length(missing_packages) != 0) {
    stop(paste0(
      "Some packages required for Shiny application are missing. ",
      "You can install them by running `install.packages(c(",
      paste0("'", missing_packages, "'", collapse = ", "),
      "))`"
    ))
  }
}
