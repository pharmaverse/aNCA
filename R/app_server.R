#' Server function for the Shiny app
#'
#' @import shiny
#' @importFrom shiny observeEvent reactiveVal reactiveValues insertUI removeUI updateSelectInput updateNavbarPage updateTabsetPanel updateCheckboxInput updateNumericInput showModal modalDialog textInput actionButton modalButton numericInput renderUI req
#' @importFrom dplyr mutate filter select group_by summarise pull arrange ungroup rename_with across case_when left_join rename
#' @importFrom tools file_ext
#' @importFrom DT renderDataTable datatable formatStyle styleEqual
#' @importFrom PKNCA PKNCAconc PKNCAdose PKNCAdata pk.nca PKNCA.options pknca_units_table
#' @importFrom utils read.csv write.csv
#' @importFrom plotly renderPlotly plotlyOutput plotly_build event_data
#' @importFrom shinyWidgets pickerInput switchInput updatePickerInput
#' @importFrom shinyjqui orderInput
#' @importFrom ggplot2 ggplot geom_errorbar geom_point geom_line labs aes facet_wrap
#' @importFrom zip zipr
#' @importFrom rio export_list
#' @importFrom rmarkdown render
#' @importFrom shinyFiles shinyDirChoose
#' @importFrom htmlwidgets JS
#' 
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @export
app_server <- function(input, output, session) {
  source(system.file("shiny/server.R", package = "aNCA"), local = TRUE)$value(input, output, session)
}
