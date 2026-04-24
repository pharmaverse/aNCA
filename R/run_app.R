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
#' \dontrun{
#'   # Launch the app without pre-loaded data or settings:
#'   run_app()
#'
#'   # Pre-load a settings file (use the full path to your YAML file):
#'   run_app(settings = system.file("www/templates/clinical_template.yaml", package = "aNCA"))
#'
#'   # Pre-load both data and settings:
#'   run_app(
#'     datapath = "/path/to/adpc.csv",
#'     settings = "/path/to/settings.yaml"
#'   )
#'
#'   # Load a specific version from a versioned settings file:
#'   run_app(settings = "/path/to/settings.yaml", settings_version = 2)
#'   run_app(settings = "/path/to/settings.yaml", settings_version = "NCA draft")
#' }
#' @export
run_app <- function(datapath = NULL, settings = NULL,
                    settings_version = 1L, ...) {
  # Increase max upload size to 30 MB
  options(shiny.maxRequestSize = 30 * 1024^2)
  if (!is.null(datapath)) {
    stopifnot(
      "Data file does not exist" = file.exists(datapath),
      "Data file must have .csv or .rds extension" =
        tools::file_ext(datapath) %in% c("csv", "rds")
    )
    opt <- options(aNCA.datapath = datapath)
    on.exit(options(opt), add = TRUE)
  }
  stopifnot(
    "settings_version must be a single integer or character string" =
      (is.numeric(settings_version) || is.character(settings_version)) &&
      length(settings_version) == 1
  )
  if (!is.null(settings)) {
    stopifnot(
      "Settings file does not exist" = file.exists(settings),
      "Settings file must have .yaml or .yml extension" =
        tools::file_ext(settings) %in% c("yaml", "yml")
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
#'
#' Reads the Imports field from DESCRIPTION and verifies each package is
#' available. This keeps the check in sync with DESCRIPTION automatically.
#' @noRd
check_app_dependencies <- function() {
  desc <- read.dcf(system.file("DESCRIPTION", package = "aNCA"), fields = "Imports")
  deps <- trimws(unlist(strsplit(desc[1, "Imports"], ",")))
  deps <- gsub("\\s*\\(.*\\)", "", deps) # strip version constraints
  deps <- deps[nzchar(deps)]

  missing_packages <- deps[!vapply(deps, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop(
      "Some packages required for the Shiny application are missing. ",
      "You can install them by running:\n",
      "  install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))",
      call. = FALSE
    )
  }
}
