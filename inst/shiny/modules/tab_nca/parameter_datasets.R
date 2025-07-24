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

      # Only select from results the requested parameters by the user
      ############################################################################
      # TODO (Gerardo): Once PKNCA non covered parameters start being covered,
      # this can be done instead using filter_requested = TRUE
      res_nca_req <- res_nca()
      params_not_requested <- res_nca_req$data$intervals %>%
        select(any_of(setdiff(names(PKNCA::get.interval.cols()), c("start", "end")))) %>%
        # For all logical columns, mutate FALSE to NA
        mutate(across(where(is.logical), ~ ifelse(.x, TRUE, NA))) %>%
        # Only select column that are only NA
        select(where(~ all(is.na(.x)))) %>%
        names()
      res_nca_req$result <- res_nca_req$result %>%
        filter(!PPTESTCD %in% translate_terms(params_not_requested, "PKNCA", "PPTESTCD"))
      ############################################################################

      export_cdisc(res_nca_req)
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
      fixedHeader = TRUE,
      dom = "Blfrtip",
      buttons = list(
        list(extend = "copy", title = paste0(filename, "_", Sys.Date())),
        list(extend = "csv", filename = paste0(filename, "_", Sys.Date()))
      ),
      headerCallback = DT::JS(
        "function(thead) {",
        "  $(thead).css('font-size', '0.75em');",
        "  $(thead).find('th').css('text-align', 'center');",
        "}"
      ),
      columnDefs = list(
        list(className = "dt-center", targets = "_all")
      ),
      lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
      paging = TRUE
    ),
    class = "row-border compact"
  ) %>%
    DT::formatStyle(columns = seq_len(ncol(data)), fontSize = "75%")
}
