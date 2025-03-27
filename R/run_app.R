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
  # TODO: check those packages over and make sure they are needed
  deps <- c(
    "bslib",
    "checkmate",
    "dplyr",
    "DT",
    "ggplot2",
    "haven",
    "htmlwidgets",
    "logger",
    "magrittr",
    "PKNCA",
    "plotly",
    "purrr",
    "reactable",
    "reactable.extras",
    "rio",
    "rmarkdown",
    "scales",
    "shiny",
    "shinyBS",
    "shinycssloaders",
    "shinyFiles",
    "shinyjqui",
    "shinyjs",
    "shinyWidgets",
    "stats",
    "stringi",
    "stringr",
    "tern",
    "tidyr",
    "tools",
    "utils",
    "units",
    "rlang",
    "yaml",
    "zip"
  )

  purrr::walk(deps, \(dep) {
    if (!requireNamespace(dep, quietly = TRUE)) {
      if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
      warning("Package required for shiny application, but not available, installing: ", dep)
      pak::pak(dep)
    }
  })
}