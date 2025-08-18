#' NCA Settings summary module
#'
#' @details
#' Provides a data table with summary of processed NCA data, based on settings provided
#' by the user.
#'
#' @param processed_pknca_data PKNCA data that was processed in accordance to setup rules.
#'
summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactable_ui(ns("study_types")),
    reactableOutput(ns("nca_parameters")),
    reactable_ui(ns("nca_intervals_summary"))
  )
}

summary_server <- function(id, processed_pknca_data) {
  moduleServer(id, function(input, output, session) {
    summary_data <- reactive({
      req(processed_pknca_data())

      conc_group_columns <- group_vars(processed_pknca_data()$conc)
      dose_group_columns <- group_vars(processed_pknca_data()$dose)

      data <- processed_pknca_data()$intervals %>%
        apply_labels(type = "ADPC") %>%
        select(where(~!is.logical(.) | any(. == TRUE))) %>%
        arrange(!!!syms(c(conc_group_columns, "type_interval", "start", "end")))
    })

    study_types <- reactive({
      req(processed_pknca_data())
      detect_study_types(processed_pknca_data()$conc$data,
                         route_column = processed_pknca_data()$dose$columns$route,
                         volume_column = processed_pknca_data()$conc$columns$volume)
    })
    
    DEFAULT_PARAMS <- c(
      "aucinf.obs", "aucinf.obs.dn",
      "auclast", "auclast.dn",
      "cmax", "cmax.dn",
      "clast.obs", "clast.obs.dn",
      "tlast", "tmax",
      "half.life", "cl.obs", "vss.obs", "vz.obs",
      "mrt.last", "mrt.obs",
      "lambda.z",
      "lambda.z.n.points", "r.squared",
      "adj.r.squared", "lambda.z.time.first",
      "aucpext.obs", "aucpext.pred",
      "ae", "fe"
    )
    
    nca_params <- reactive({
      selected_rows <- getReactableState("nca_parameters", "selected")
      if (is.null(selected_rows) || length(selected_rows) == 0) return(NULL)
      
      params_data <- metadata_nca_parameters %>%
        filter(TYPE != "PKNCA-not-covered")
      selected_terms <- params_data[selected_rows, , drop = FALSE]
      
      # Return PKNCA column names
      selected_terms$PKNCA
    })
    
    output$nca_parameters <- renderReactable({
      #remove parameters that are currently unavailable in PKNCA
      params_data <- metadata_nca_parameters %>%
        filter(TYPE != "PKNCA-not-covered")
      
      default_row_indices <- which(params_data$PKNCA %in% DEFAULT_PARAMS)
      
      reactable(
        params_data %>%
          select(TYPE, PPTESTCD, PPTEST, CAT),
        groupBy = c("TYPE"),
        pagination = FALSE,
        filterable = TRUE,
        compact = TRUE,
        onClick = "select",
        height = "49vh",
        selection = "multiple",
        defaultSelected = default_row_indices
      )
    })

    reactable_server(
      "study_types",
      study_types,
      height = "28vh"
    )

    reactable_server(
      "nca_intervals_summary",
      summary_data,
      height = "98vh"
    )
    
    nca_params
  })
}
