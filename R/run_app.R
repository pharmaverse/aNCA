#' Run the Shiny app
#' @param ... Arguments passed to `shiny::runApp()`
#' @export
run_app <- function(...) {
  require(aNCA)

  require(shiny)
  require(bslib)

  require(dplyr)
  require(DT)
  require(ggpplot2)
  require(htmlwidgets)
  require(PKNCA)
  require(plotly)
  require(reactable)
  require(rio)
  require(rmarkdown)
  require(shinyBS)
  require(shinycssloaders)
  require(shinyjqui)
  require(shinyWidgets)
  require(stringi)
  require(tools)
  require(utils)
  require(zipr)
  shiny::runApp(system.file("shiny", package = "aNCA"), ...)
}
