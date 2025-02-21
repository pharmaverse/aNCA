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
