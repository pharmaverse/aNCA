#' Run the Shiny app
#' @param datapath Full path to a single `.csv` or `.rds` data file.
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
#' @examples
#' \donttest{
#'   adnca_path <- system.file("shiny/data/Dummy_data.csv", package = "aNCA")
#'
#'   # Run the app with pre-loaded data
#'   run_app(datapath = adnca_path)
#'   # Run the app without pre-loading data (standard usage)
#'   run_app()
#' }
#'
#' @export
run_app <- function(datapath = NULL, ...) {
  # Increase max upload size to 30 MB
  options(shiny.maxRequestSize = 30 * 1024^2)
  if (!is.null(datapath)) {
    opt <- options(aNCA.datapath = datapath)
    on.exit(options(opt), add = TRUE)
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
    "sass",
    "shiny",
    "shinycssloaders",
    "shinyjs",
    "shinyjqui",
    "shinyWidgets",
    "stats",
    "stringi",
    "stringr",
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
