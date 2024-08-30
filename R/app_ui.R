#' UI function for the Shiny app
#'
#' @import shiny
#' @importFrom shiny tags icon navbarPage tabPanel sidebarLayout sidebarPanel mainPanel fluidPage fluidRow column selectInput actionButton fileInput conditionalPanel numericInput checkboxInput radioButtons downloadButton uiOutput helpText plotOutput
#' @importFrom shinyBS bsModal
#' @importFrom shinyWidgets dropdown pickerInput
#' @importFrom shinyjqui orderInput updateOrderInput
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#'
#' @export
app_ui <- function() {
  source(system.file("shiny/ui.R", package = "aNCA"), local = TRUE)$value
}
