require(aNCA)

require(bslib)
require(dplyr)
require(DT)
require(htmlwidgets)
require(logger)
require(magrittr)
require(plotly)
require(purrr)
require(reactable)
require(reactable.extras)
require(shiny)
require(shinycssloaders)
require(shinyjs)
require(shinyjqui)
require(shinyWidgets)
require(stats)
require(stringi)
require(stringr)
require(tidyr)
require(tools)
require(utils)
require(rlang)
require(yaml)

lapply(list.files("modules", pattern = "\\.R$", full.names = TRUE, recursive = TRUE), source)
lapply(list.files("functions", pattern = "\\.R$", full.names = TRUE, recursive = TRUE), source)

LABELS <<- read.csv(system.file("shiny/data/adnca_labels.csv", package = "aNCA"))
assets <- system.file("shiny/www", package = "aNCA")

setup_logger()

ui <- function() {
  page_sidebar(
    id = "sidebar",
    title = "aNCA",
    theme = bs_theme(bootswatch = "flatly"),

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

    sidebar = navset_pill_list(
      id = "page",
      selected = "data",
      # DATA ----
      nav_panel(
        "Data",
        value = "data",
        fluid = TRUE
      ),
      # NCA ----
      nav_panel(
        "NCA",
        value = "nca",
        fluid = TRUE
      ),
      # VISUALISATION ----
      nav_panel(
        "Visualisation",
        value = "visualisation",
        fluid = TRUE
      ),
      # New TLG tab
      nav_panel(
        "TLG",
        value = "tlg"
      )
    ),
    div(
      conditionalPanel("input.page == 'data'", tab_data_ui("data")),
      conditionalPanel("input.page == 'nca'", tab_nca_ui("nca")),
      conditionalPanel("input.page == 'visualisation'", tab_visuals_ui("visuals")),
      conditionalPanel("input.page == 'tlg'", tab_tlg_ui("tlg"))
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
  adnca_data <- data_module$data
  #' Create global data object. This is accessible by all modules, without the need to pass
  #' data reactive directly.

  # Grouping Variables
  grouping_vars <- data_module$grouping_variables

  # NCA ----
  res_nca <- tab_nca_server("nca", adnca_data, grouping_vars)
  # VISUALISATION ----
  tab_visuals_server("visuals", adnca_data, grouping_vars, res_nca)

  # TLG
  tab_tlg_server("tlg", adnca_data)
}

shiny::shinyApp(ui, server)
