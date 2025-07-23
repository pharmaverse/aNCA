parameter_datasets_ui <- function(id) {
  ns <- NS(id)
  navset_pill(
    nav_panel("PP",   reactable_ui(ns("pp_dataset"))),
    nav_panel("ADPP", reactable_ui(ns("adpp_dataset"))),
    nav_panel("ADPC", reactable_ui(ns("adpc_dataset")))
  )
}

parameter_datasets_server <- function(id, res_nca) {
  moduleServer(id, function(input, output, session) {
    CDISC <- reactive({
      req(res_nca())
      export_cdisc(res_nca())
    })

    # paste0(session$userData$project_name(), "_pp")

    reactable_server(
      "pp_dataset",
      reactive(CDISC()$pp),
      download_buttons = c("csv", "xlsx"),
      file_name = function() { paste0(session$userData$project_name(), "_pp") }
    )
    reactable_server(
      "adpp_dataset",
      reactive(CDISC()$adpp),
      download_buttons = c("csv", "xlsx"),
      file_name = function() { paste0(session$userData$project_name(), "_adpp") }
    )
    reactable_server(
      "adpc_dataset",
      reactive(CDISC()$adpc),
      download_buttons = c("csv", "xlsx"),
      file_name = function() { paste0(session$userData$project_name(), "_adpc") }
    )

    # Save the results in the output folder
    observeEvent(CDISC(), {
      session$userData$results$CDISC <- CDISC()[c("pp", "adpp", "adpc")]
    })
  })
}
