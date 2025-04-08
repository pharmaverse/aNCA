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

    output$pp_dataset <- DT::renderDataTable({
      DT::datatable(
        data = CDISC()$pp,
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
              title = paste0("PP_", CDISC()$studyid, "_", Sys.Date())
            ),
            list(
              extend = "csv",
              filename = paste0("PP_", CDISC()$studyid, "_", Sys.Date())
            ),
            list(
              extend = "excel",
              title = NULL,
              header = colnames(CDISC()$pp),
              filename = paste0("PP_", CDISC()$studyid, "_", Sys.Date())
            )
          )
        )
      )
    })

    output$adpp_dataset <- DT::renderDataTable({
      DT::datatable(
        data = CDISC()$adpp,
        rownames = FALSE,
        extensions = c("FixedHeader", "Buttons"),
        options = list(
          scrollX = TRUE,
          scrollY = "80vh",
          pageLength = -1,
          lengthMenu = -1,
          fixedHeader = TRUE,
          dom = "Bfrtip",
          buttons = list(
            list(
              extend = "copy",
              title = paste0("ADPP_", CDISC()$studyid, "_", Sys.Date())
            ),
            list(
              extend = "csv",
              filename = paste0("ADPP_", CDISC()$studyid, "_", Sys.Date())
            ),
            list(
              extend = "excel",
              title = NULL,
              header = colnames(CDISC()$adpp),
              filename = paste0("ADPP_", CDISC()$studyid, "_", Sys.Date())
            )
          )
        )
      )
    })
    
    output$adpc_dataset <- DT::renderDataTable({
      DT::datatable(
        data = CDISC()$adpc,
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
              title = paste0("ADPC_", CDISC()$studyid, "_", Sys.Date())
            ),
            list(
              extend = "csv",
              filename = paste0("ADPC_", CDISC()$studyid, "_", Sys.Date())
            ),
            list(
              extend = "excel",
              title = NULL,
              header = colnames(CDISC()$adpc),
              filename = paste0("ADPC_", CDISC()$studyid, "_", Sys.Date())
            )
          )
        )
      )
    })
  })
}
