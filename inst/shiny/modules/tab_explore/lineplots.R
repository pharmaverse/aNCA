#' Reusable Plot Sidebar UI
#'
#' @param id A character string specifying the module ID.
#' @param is_mean_plot A logical value indicating whether to show options
#'                     specific to the mean plot. Default is FALSE.
#'
#' @return A UI definition for the plot sidebar.
#' @export
plot_sidebar_ui <- function(id, is_mean_plot = FALSE) {
  ns <- NS(id)
  
  sidebar(
    position = "right",
    open = TRUE,
    selectInput(
      ns("palette_theme"),
      "Select Color Theme:",
      choices = c(
        "Default (ggplot2)" = "default",
        "Viridis" = "viridis",
        "Spectral" = "spectral"
      ),
      selected = "default"
    ),
    pickerInput(
      inputId = ns("param"),
      label = "Select Parameters:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    pickerInput(
      inputId = ns("pcspec"),
      label = "Select Matrix:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    if (!is_mean_plot) {
      pickerInput(
        inputId = ns("usubjid"),
        label = "Select Subjects:",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    },
    pickerInput(
      inputId = ns("colorby"),
      label = "Choose the variables to color by:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    pickerInput(
      inputId = ns("facetby"),
      label = "Choose the variables to facet by:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    radioButtons(
      ns("log"),
      "Select the Y-axis scale:",
      choices = c("Linear" = "lin", "Logarithmic" = "log"),
      selected = "lin"
    ),
    radioButtons(
      ns("timescale"),
      "Choose the Timescale",
      choices = c("All Time", "By Dose Profile")
    ),
    conditionalPanel(
      condition = "input.timescale == 'By Dose Profile'",
      uiOutput(ns("cycle_select_ui")),
      ns = ns
    ),
    checkboxInput(ns("show_threshold"), label = "Show Threshold"),
    conditionalPanel(
      condition = "input.show_threshold == true",
      numericInput(ns("threshold_value"), label = "Threshold Value", value = 0),
      ns = ns
    ),
    checkboxInput(ns("show_dose"), label = "Show Dose Times"),
    # --- Conditional UI Elements ---
    if (is_mean_plot) {
      tagList(
        checkboxInput(ns("sd_max"), label = "+SD", value = TRUE),
        checkboxInput(ns("sd_min"), label = "-SD", value = TRUE),
        checkboxInput(ns("ci"), label = "Show 95% CI", value = FALSE),
        helpText("Mean values are not displayed if n < 3 for a time point.")
      )
    }
  )
}
