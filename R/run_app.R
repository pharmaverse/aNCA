#' Run the Shiny app
#'
#' @param ... Arguments passed to `shiny::runApp()`
#'
#' List of packages imported for the shiny application.
#' When adding new imports, please keep the alphabetical order, at lest for packages.
#' @import shiny
#' @importFrom dplyr mutate filter select group_by summarise pull arrange ungroup
#' @importFrom dplyr rename_with across case_when left_join rename
#' @importFrom DT DTOutput renderDataTable datatable formatStyle styleEqual
#' @importFrom ggplot2 ggplot geom_errorbar geom_point geom_line labs aes facet_wrap
#' @importFrom htmlwidgets JS
#' @importFrom PKNCA PKNCAconc PKNCAdose PKNCAdata pk.nca PKNCA.options pknca_units_table
#' @importFrom plotly plotlyOutput renderPlotly plotly_build event_data ggplotly
#' @importFrom reactable reactable reactableOutput renderReactable colDef reactableTheme
#' @importFrom reactable getReactableState
#' @importFrom reactable.extras text_extra dropdown_extra
#' @importFrom rio export_list
#' @importFrom rmarkdown render
#' @importFrom shinyBS bsModal
#' @importFrom shinyFiles shinyDirChoose
#' @importFrom shinyjqui orderInput updateOrderInput
#' @importFrom shinyWidgets dropdown pickerInput switchInput updatePickerInput
#' @importFrom tools file_ext
#' @importFrom utils read.csv write.csv
#' @importFrom zip zipr
#'
#' @export
run_app <- function(...) {
  # Load all packages mentioned in the NAMESPACE
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
