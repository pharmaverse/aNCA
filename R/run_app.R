#' Run the Shiny app
#' @param datapath Path to the directory containing the data files.
#' @param ... Arguments passed to `shiny::runApp()`
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
