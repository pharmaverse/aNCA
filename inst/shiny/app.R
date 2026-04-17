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
require(shiny)
require(shinycssloaders)
require(shinyjs)
require(shinyjqui)
require(shinyWidgets)
require(stats)

require(tidyr)
require(tools)
require(utils)
require(rlang)
require(yaml)

lapply(list.files("modules", pattern = "\\.R$", full.names = TRUE, recursive = TRUE), source)
lapply(list.files("functions", pattern = "\\.R$", full.names = TRUE, recursive = TRUE), source)

assets <- system.file("shiny/www", package = "aNCA")

shiny::addResourcePath("logos", system.file("man/figures", package = "aNCA"))

setup_logger()

ui <- function() {
  page_sidebar(
    id = "sidebar",
    title = tagList(
      div(
        style = "display: flex; align-items: center; gap: 10px;",
        tags$img(
          src = "logos/aNCA_logo.png", # Ensure file is in www/ or resource path
          alt = "aNCA logo",
          width = "40px" # Adjusted for sidebar header scale
        )
      ),
      div(
        class = "project-name-container",
        textInput("project_name", label = NULL, placeholder = "Project Name"),
        icon("file", class = "project-name-icon"),
        zip_ui("zip_modal")
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

    tags$script("
      Shiny.addCustomMessageHandler('copy_to_clipboard', function(text) {
        navigator.clipboard.writeText(text).catch(function() {
          var ta = document.createElement('textarea');
          ta.value = text;
          document.body.appendChild(ta);
          ta.select();
          document.execCommand('copy');
          document.body.removeChild(ta);
        });
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
      ),
      # ABOUT ----
      nav_panel(
        "About",
        value = "about",
        icon = icon("circle-info")
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
      ),
      conditionalPanel(
        class = "page-container",
        condition = "input.page == 'about'",
        tab_about_ui("about")
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
    if (input$project_name != "") input$project_name else ""
  })

  # Track active tab for settings versioning
  session$userData$active_tab <- reactive({
    input$page %||% ""
  })

  # Versioned settings storage (list of version entries)
  session$userData$settings_versions <- reactiveVal(list())

  # Uploaded dataset filename (set by data_upload module)
  session$userData$dataset_filename <- NULL

  # Helper (plain function, not reactive): prepend project name with separator
  session$userData$project_prefix <- function(sep = "-") {
    pn <- session$userData$project_name()
    if (pn == "") "" else paste0(pn, sep)
  }

  # Initially disable all tabs except the 'Data' tab
  shinyjs::disable(selector = "#page li a[data-value=nca]")
  shinyjs::disable(selector = "#page li a[data-value=exploration]")
  shinyjs::disable(selector = "#page li a[data-value=tlg]")

  # DATA ----
  tab_data_outputs <- tab_data_server("data")

  # Extract up to 3 unique STUDYIDs from uploaded data (shared by auto-populate and zip fallback)
  session$userData$study_ids_label <- reactive({
    raw <- tab_data_outputs$adnca_raw()
    req(raw, "STUDYID" %in% names(raw))
    study_ids <- unique(raw[["STUDYID"]])
    study_ids <- study_ids[!is.na(study_ids)]
    if (length(study_ids) > 3) study_ids <- study_ids[1:3]
    if (length(study_ids) > 0) paste(study_ids, collapse = "_") else ""
  })

  # Auto-populate project name with STUDYID when data is uploaded
  observeEvent(session$userData$study_ids_label(), {
    label <- session$userData$study_ids_label()
    if (label != "") {
      updateTextInput(session, "project_name", value = label)
    }
  })

  # EXPLORATION ----
  tab_explore_server(
    "explore",
    tab_data_outputs$pknca_data,
    tab_data_outputs$extra_group_vars
  )

  # NCA ----
  tab_nca_outputs <- tab_nca_server(
    "nca",
    tab_data_outputs$pknca_data,
    tab_data_outputs$extra_group_vars,
    tab_data_outputs$settings_override
  )

  # TLG
  tab_tlg_server("tlg", tab_nca_outputs$processed_pknca_data)

  # ABOUT ----
  tab_about_server("about")

  # ZIP export
  zip_server(
    "zip_modal",
    res_nca = tab_nca_outputs$res_nca,
    adnca_data = tab_data_outputs$pknca_data,
    settings = session$userData$settings,
    grouping_vars = tab_data_outputs$extra_group_vars
  )
}

shiny::shinyApp(ui, server)
