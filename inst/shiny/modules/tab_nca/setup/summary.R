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
        apply_labels(LABELS, "ADPC") %>%
        select(where(~!is.logical(.) | any(. == TRUE))) %>%
        arrange(!!!syms(c(conc_group_columns, "type_interval", "start", "end")))
    })


    study_types <- reactive ({
      req(processed_pknca_data())
      detect_study_types(processed_pknca_data()$conc$data,
                         route_column = processed_pknca_data()$dose$columns$route,
                         volume_column = processed_pknca_data()$conc$columns$volume)
    })
    
    reactable_server(
      "study_types",
      study_types,
      height = "28vh"
    )

    reactable_server(
      "nca_intervals_summary",
      summary_data,
      columns = generate_col_defs,
      height = "98vh"
    )
  })
}
