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
    p("The following study types were detected in the data:"),
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
        apply_labels(type = "ADNCA") %>%
        select(where(~!is.logical(.) | any(. == TRUE))) %>%
        arrange(!!!syms(c(conc_group_columns, "type_interval", "start", "end")))
    })

    study_types_df <- reactive({
      req(processed_pknca_data())

      conc_group_columns <- group_vars(processed_pknca_data()$conc)
      dose_group_columns <- group_vars(processed_pknca_data()$dose)
      group_columns <- unique(c(conc_group_columns, dose_group_columns))

      groups <- group_columns %>%
        purrr::keep(\(col) {
          !is.null(col) && length(unique(processed_pknca_data()$conc$data[[col]])) > 1
        })

      detect_study_types(processed_pknca_data()$conc$data,
                         groups,
                         metabfl_column = "METABFL",
                         route_column = processed_pknca_data()$dose$columns$route,
                         volume_column = processed_pknca_data()$conc$columns$volume)
    })

    study_types_summary <- reactive({
      req(study_types_df())

      conc_group_columns <- group_vars(processed_pknca_data()$conc)
      dose_group_columns <- group_vars(processed_pknca_data()$dose)
      group_columns <- unique(c(conc_group_columns, dose_group_columns))

      groups <- group_columns %>%
        purrr::keep(\(col) {
          !is.null(col) && col != "USUBJID" &&
            length(unique(processed_pknca_data()$conc$data[[col]])) > 1
        })

      study_types_df()  %>%
        #summarise each unique type and group with number of USUBJID
        group_by(!!!syms(groups), type) %>%
        summarise(USUBJID_Count = n_distinct(USUBJID), .groups = "drop")
    })

    reactable_server(
      "study_types",
      study_types_summary,
      height = "28vh"
    )

    reactable_server(
      "nca_intervals_summary",
      summary_data,
      height = "48vh"
    )
  })
}
