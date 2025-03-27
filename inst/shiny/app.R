require(aNCA)

require(bslib)
require(checkmate)
require(dplyr)
require(DT)
require(ggplot2)
require(htmlwidgets)
require(logger)
require(magrittr)
require(PKNCA)
require(plotly)
require(purrr)
require(reactable)
require(reactable.extras)
require(rio)
require(rmarkdown)
require(scales)
require(shiny)
require(shinyBS)
require(shinycssloaders)
require(shinyFiles)
require(shinyjqui)
require(shinyjs)
require(shinyWidgets)
require(stats)
require(stringi)
require(stringr)
require(tern)
require(tidyr)
require(tools)
require(utils)
require(units)
require(rlang)
require(yaml)
require(zip)

lapply(list.files("modules", pattern = "\\.R$", full.names = TRUE, recursive = TRUE), source)
lapply(list.files("functions", pattern = "\\.R$", full.names = TRUE, recursive = TRUE), source)

LABELS <<- read.csv(system.file("shiny/data/adnca_labels.csv", package = "aNCA"))
assets <- system.file("shiny/www", package = "aNCA")

# setup logger #
log_layout(layout_glue_colors)
log_formatter(formatter_glue)
log_threshold(TRACE)
log_appender(appender_console, namespace = "global")

ui <- function() {
  # Define UI
  fluidPage(
    tags$script("
      Shiny.addCustomMessageHandler('update', function(value) {
      Shiny.setInputValue('update', value);
      }); "),

    tags$script("
      Shiny.addCustomMessageHandler('increment', function(value) {
        var newValue = value + 1;
        Shiny.setInputValue('update', newValue, {priority: 'event'});
      });
    "),

    includeCSS(file.path(assets, "style.css")),
    includeScript(file.path(assets, "index.js")),

    page_navbar(
      id = "page",
      title = "aNCA",
      # DATA ----
      nav_panel(
        "Data",
        value = "data",
        fluid = TRUE,
        tab_data_ui("data")
      ),
      # NCA ----
      nav_panel("NCA", value = "nca", fluid = TRUE,
        tab_nca_ui("nca")
      ),

      # VISUALISATION ----
      nav_panel("Visualisation", value = "visualisation",
        fluid = TRUE,
        tab_visuals_ui("visuals")
      ),
      # New TLG tab
      nav_panel("TLG", value = "tlg",
        tab_tlg_ui("tlg")
      )
    ),
    shinyjs::useShinyjs(),
    reactable.extras::reactable_extras_dependency()
  )
}

server <- function(input, output, session) {
  log_info("Startup")

  # Initially disable all tabs except the 'Data' tab
  shinyjs::disable(selector = "#page li a[data-value=nca]")
  shinyjs::disable(selector = "#page li a[data-value=visualisation]")
  shinyjs::disable(selector = "#page li a[data-value=tlg]")

  # DATA ----
  data_module <- tab_data_server("data")
  # Data set for analysis
  data <- data_module$data
  #' Create global data object. This is accessible by all modules, without the need to pass
  #' data reactive directly.
  session$userData$data <- reactive(data())
  # Grouping Variables
  grouping_vars <- data_module$grouping_variables

  # NCA ----
  res_nca <- tab_nca_server("nca", data, grouping_vars)
  # VISUALISATION ----
  tab_visuals_server("visuals", data, grouping_vars, res_nca)

  # TLG
  tab_tlg_server("tlg")
}

shiny::shinyApp(ui, server)