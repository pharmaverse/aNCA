assets <- system.file("shiny/www", package = "aNCA")
source(system.file("shiny/modules/tab_tlg.R", package = "aNCA"))

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
      fluidPage(
        actionButton("nca", "Run NCA", class = "run-nca-btn"),
        downloadButton("settings_save", "Save Project Settings"),

        navset_tab(id = "ncapanel",
          nav_panel("Setup", fluid = TRUE,

            navlistPanel(
              tabPanel("NCA settings", nca_settings_ui("nca_settings")),
              tabPanel("Slope Selector", slope_selector_ui("slope_selector"))

            )
          ),
          nav_panel("Results", fluid = TRUE,
            navset_pill_list(
              nca_results_ui("nca_results"),
              nav_panel(
                "Slopes",
                DTOutput("preslopesettings")
              ),
              nav_panel(
                "Exclusions",
                tableOutput("manual_slopes2")
              ),
              tabPanel("Parameter Datasets", parameter_datasets_ui("parameter_datasets"))
            )
          ),
          tabPanel("Additional Analysis", additional_analysis_ui("non_nca"))
        )
      )
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
