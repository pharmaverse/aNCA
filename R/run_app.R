#' Run the Shiny app
#' Please do not add import roxygen comments here. This function
#' should require every namespace that is documented in the NAMESPACE.
#'
#' Apart from the dependencies, this function should also load the package
#' itself, in case it is run via `aNCA::run_app()`.
#'
#' @param ... Arguments passed to `shiny::runApp()`
#'
#' @export
run_app <- function(...) {
  require("aNCA")
  require("dplyr")
  require("DT")
  require("forcats")
  require("ggplot2")
  require("grid")
  require("haven")
  require("htmlwidgets")
  require("logger")
  require("nestcolor")
  require("PKNCA")
  require("plotly")
  require("reactable.extras")
  require("reactable")
  require("rio")
  require("rmarkdown")
  require("shiny")
  require("shinyBS")
  require("shinyFiles")
  require("shinyjqui")
  require("shinyWidgets")
  require("stats")
  require("tern")
  require("tidyr")
  require("tools")
  require("utils")
  require("zip")
  shiny::runApp(system.file("shiny", package = "aNCA"), ...)
}
