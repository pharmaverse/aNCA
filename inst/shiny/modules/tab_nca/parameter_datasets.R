parameter_datasets_ui <- function(id) {
  ns <- NS(id)
  navset_pill(
    nav_panel("PP", DTOutput(ns("pp_dataset"))),
    nav_panel("ADPP", DTOutput(ns("adpp_dataset"))),
    nav_panel("ADPC", DTOutput(ns("adpc_dataset")))
  )
}

parameter_datasets_server <- function(id, res_nca) {
  moduleServer(id, function(input, output, session) {
    CDISC <- reactive({
      req(res_nca())
      export_cdisc(res_nca())
    })

    output$pp_dataset <- DT::renderDataTable(
      .parameters_datatable(
        CDISC()$pp,
        paste0(session$userData$project_name(), "_pp")
      )
    )

    output$adpp_dataset <- DT::renderDataTable(
      .parameters_datatable(
        CDISC()$adpp,
        paste0(session$userData$project_name(), "_adpp")
      )
    )

    output$adpc_dataset <- DT::renderDataTable(
      .parameters_datatable(
        CDISC()$adpc,
        paste0(session$userData$project_name(), "_adpc")
      )
    )

    # Save the results in the output folder
    observeEvent(CDISC(), {
      session$userData$results$CDISC <- CDISC()[c("pp", "adpp", "adpc")]
    })
  })
}

# Helper function to create a datatable for a parameter dataset
.parameters_datatable <- function(data, filename) {
  DT::datatable(
    data = data,
    rownames = FALSE,
    extensions = c("FixedHeader", "Buttons"),
    options = list(
      scrollX = TRUE,
      scrollY = "80vh",
      searching = TRUE,
      fixedColumns = TRUE,
      fixedHeader = TRUE,
      autoWidth = TRUE,
      pageLength = -1,
      lengthMenu = -1,
      dom = "Bfrtip",
      buttons = list(
        list(
          extend = "copy",
          title = paste0(filename, "_", Sys.Date())
        ),
        list(
          extend = "csv",
          filename = paste0(filename, "_", Sys.Date())
        ),
        list(
          extend = "excel",
          title = NULL,
          header = colnames(data),
          filename = paste0(filename, "_", Sys.Date())
        )
      )
    )
  )
}
