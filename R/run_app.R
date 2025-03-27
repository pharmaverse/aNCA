#' Run the Shiny app
#' @param ... Arguments passed to `shiny::runApp()`
#' @export
run_app <- function(...) {
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

  purrr::walk(deps, \(dep) {
    if (!requireNamespace(dep, quietly = TRUE)) {
      if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
      warning("Package required for shiny application, but not available, installing: ", dep)
      pak::pak(dep)
    }
  })
}