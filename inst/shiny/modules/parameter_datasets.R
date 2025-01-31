parameter_datasets_ui <- function(id) {
  ns <- NS(id)
  navset_pill(
    nav_panel("PP", DTOutput(ns("pp_dataset"))),
    nav_panel("ADPP", DTOutput(ns("adpp_dataset")))
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
          scrollY = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          fixedHeader = TRUE,
          autoWidth = TRUE,
          dom = "Bfrtip",
          buttons = list(
            list(
              extend = "copy",
              title = paste0("PP_Dataset", "_", Sys.Date())
            ),
            list(
              extend = "csv",
              title = paste0("PP_Dataset", "_", Sys.Date())
            ),
            list(
              extend = "excel",
              title = paste0("PP_Dataset", "_", Sys.Date())
            )
          )
        )
      )
    })

    output$adpp_dataset <- DT::renderDataTable({
      DT::datatable(
        data = CDISC()$adpp,
        extensions = c("FixedHeader", "Buttons"),
        options = list(
          scrollX = TRUE,
          scrollY = TRUE,
          lengthMenu = list(c(10, 25, -1), c("10", "25", "All")),
          fixedHeader = TRUE,
          dom = "Bfrtip",
          buttons = list(
            list(
              extend = "copy",
              title = paste0("ADPP_Dataset", "_", Sys.Date())
            ),
            list(
              extend = "csv",
              title = paste0("ADPP_Dataset", "_", Sys.Date())
            ),
            list(
              extend = "excel",
              title = paste0("ADPP_Dataset", "_", Sys.Date())
            )
          )
        )
      )
    })
  })
}