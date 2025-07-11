#' Run the Shiny app
#' @param datapath Full path to a single `.csv` or `.rds` data file.
#' @param ... Arguments passed to `shiny::runApp()`
#'
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
#' \dontrun{
#'   # Create a dummy data file
#'   temp_data_path <- tempfile(fileext = ".csv")
#'   write.csv(data.frame(USUBJID = 1, AFRLT = 0:4, AVAL = c(0, 10, 8, 4, 1)),
#'             file = temp_data_path, row.names = FALSE)
#'
#'   # Run the app, automatically loading the dummy data
#'   run_app(datapath = temp_data_path)
#'
#'   # Run the app without pre-loading data (standard usage)
#'   run_app()
#' }
#'
#' @export
run_app <- function(datapath = NULL, ...) {
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
    "DT",
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

  missing_packages <- purrr::keep(deps, \(dep) !requireNamespace(dep, quietly = TRUE))

  if (length(missing_packages) != 0) {
    warning(
      "The following packages are required for shiny application, but are missing:\n",
      paste0(missing_packages, collapse = "\n"), "\n",
      "The required packages will be installed."
    )

    if (!requireNamespace("pak", silently = TRUE)) install.packages("pak")
    pak::pak(missing_packages)
  }
}
