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
        CDISC()$studyid,
        "PP"
      )
    )

    output$adpp_dataset <- DT::renderDataTable(
      .parameters_datatable(
        CDISC()$adpp,
        CDISC()$studyid,
        "ADPP"
      )
    )

    output$adpc_dataset <- DT::renderDataTable(
      .parameters_datatable(
        CDISC()$adpc,
        CDISC()$studyid,
        "ADPC"
      )
    )

    # Save the results in the output folder
    observeEvent(CDISC(), {
      save_output(
        output = CDISC()[c("pp", "adpp", "adpc")],
        output_path = paste0(results_dir(), "/cdisc")
      )
    })
  })
}

# Helper function to create a datatable for a parameter dataset
.parameters_datatable <- function(data, studyid, prefix) {
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
          title = paste0(project_name(), "-", prefix, "_", Sys.Date())
        ),
        list(
          extend = "csv",
          filename = paste0(project_name(), "-", prefix, "_", Sys.Date())
        ),
        list(
          extend = "excel",
          title = NULL,
          header = colnames(data),
          filename = paste0(project_name(), "-", prefix, "_", Sys.Date())
        )
      )
    )
  )
}