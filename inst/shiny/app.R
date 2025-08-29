require(aNCA)

require(bslib)
require(dplyr)
require(htmlwidgets)
require(logger)
require(formatters)
require(magrittr)
require(plotly)
require(purrr)
require(reactable)
require(reactable.extras)
require(sass)
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

assets <- system.file("shiny/www", package = "aNCA")

sass(
  sass_file(file.path(assets, "styles/main.scss")),
  output = file.path(assets, "main.css")
)

setup_logger()

ui <- function() {
  page_sidebar(
    id = "sidebar",
    title = tagList(
      span("aNCA"),
      div(
        class = "project-name-container",
        textInput("project_name", label = NULL, placeholder = "Project Name"),
        icon("file", class = "project-name-icon"),
        download_code_ui("download_code")
      )
    ),

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

    includeCSS(file.path(assets, "main.css")),
    includeScript(file.path(assets, "index.js")),

    sidebar = navset_pill_list(
      id = "page",
      well = FALSE,
      selected = "data",
      # DATA ----
      nav_panel(
        "Data",
        value = "data",
        icon = icon("database"),
        fluid = TRUE
      ),
      # VISUALISATION ----
      nav_panel(
        "Exploration",
        value = "exploration",
        icon = icon("chart-line"),
        fluid = TRUE
      ),
      # NCA ----
      nav_panel(
        "NCA",
        value = "nca",
        icon = icon("microscope"),
        fluid = TRUE
      ),
      # New TLG tab
      nav_panel(
        "TLG",
        value = "tlg",
        icon = icon("table-list")
      )
    ),
    div(
      class = "content-container",
      conditionalPanel(
        class = "page-container",
        condition = "input.page == 'data'",
        tab_data_ui("data")
      ),
      conditionalPanel(
        class = "page-container",
        condition = "input.page == 'nca'",
        tab_nca_ui("nca")
      ),
      conditionalPanel(
        class = "page-container",
        condition = "input.page == 'exploration'",
        tab_explore_ui("explore")
      ),
      conditionalPanel(
        class = "page-container",
        condition = "input.page == 'tlg'",
        tab_tlg_ui("tlg")
      )
    ),

    shinyjs::useShinyjs(),
    reactable.extras::reactable_extras_dependency()
  )
}

server <- function(input, output, session) {
  log_info("Startup")

  # Store globally the name of the project
  session$userData$project_name <- reactive({
    if (input$project_name != "") input$project_name else "Unnamed_Project"
  })

  download_code_server("download_code")

  # Initially disable all tabs except the 'Data' tab
  shinyjs::disable(selector = "#page li a[data-value=nca]")
  shinyjs::disable(selector = "#page li a[data-value=exploration]")
  shinyjs::disable(selector = "#page li a[data-value=tlg]")

  # DATA ----
  data_module <- tab_data_server("data")
  # Data set for analysis
  adnca_data <- data_module$data
  #' Create global data object. This is accessible by all modules, without the need to pass
  #' data reactive directly.

  # Grouping Variables
  grouping_vars <- data_module$grouping_variables

  # EXPLORATION ----
  tab_explore_server("explore", adnca_data, grouping_vars)

  # NCA ----
  list_tab_nca <- tab_nca_server("nca", adnca_data, grouping_vars)

  # TLG
  tab_tlg_server("tlg", list_tab_nca$processed_pknca_data)
}

shiny::shinyApp(ui, server)
