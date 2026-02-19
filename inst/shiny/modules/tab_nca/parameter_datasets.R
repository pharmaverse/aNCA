parameter_datasets_ui <- function(id) {
  ns <- NS(id)
  navset_pill(
    nav_panel("PP", card(reactable_ui(ns("pp_dataset")), class = "border-0 shadow-none")),
    nav_panel("ADPP", card(reactable_ui(ns("adpp_dataset")), class = "border-0 shadow-none")),
    nav_panel("ADNCA", card(reactable_ui(ns("adnca_dataset")), class = "border-0 shadow-none"))
  )
}

parameter_datasets_server <- function(id, res_nca) {
  moduleServer(id, function(input, output, session) {
    CDISC <- reactive({
      req(res_nca())
      export_cdisc(res_nca())
    })

    reactable_server(
      "pp_dataset",
      reactive(CDISC()$pp),
      download_buttons = c("csv", "xlsx"),
      file_name = function() paste0(session$userData$project_prefix("_"), "pp")
    )
    reactable_server(
      "adpp_dataset",
      reactive(CDISC()$adpp),
      download_buttons = c("csv", "xlsx"),
      file_name = function() paste0(session$userData$project_prefix("_"), "adpp")
    )
    reactable_server(
      "adnca_dataset",
      reactive(CDISC()$adnca),
      download_buttons = c("csv", "xlsx"),
      file_name = function() paste0(session$userData$project_prefix("_"), "adnca")
    )

    # Save the results in the output folder
    observeEvent(CDISC(), {
      session$userData$results$CDISC <- CDISC()[c("pp", "adpp", "adnca")]
    })
  })
}
