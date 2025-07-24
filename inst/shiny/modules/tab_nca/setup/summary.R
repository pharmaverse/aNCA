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
  reactableOutput(ns("nca_intervals_summary"))
}

summary_server <- function(id, processed_pknca_data) {
  moduleServer(id, function(input, output, session) {
    output$nca_intervals_summary <- renderReactable({
      req(processed_pknca_data())

      conc_group_columns <- group_vars(processed_pknca_data()$conc)
      dose_group_columns <- group_vars(processed_pknca_data()$dose)
      time_dose <- processed_pknca_data()$dose$columns$time

      data <- processed_pknca_data()$intervals %>%
        apply_labels(type = "ADPC") %>%
        select(where(~!is.logical(.) | any(. == TRUE))) %>%
        left_join(
          processed_pknca_data()$dose$data %>%
            mutate(TIME_DOSE = .[[time_dose]]) %>%
            select(any_of(c(dose_group_columns, "TIME_DOSE")))
        ) %>%
        arrange(!!!syms(c(conc_group_columns, "type_interval", "start", "end"))) %>%
        mutate(start = start - TIME_DOSE, end = end - TIME_DOSE)

      reactable(
        data,
        columns = generate_col_defs(data),
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        compact = TRUE,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        bordered = TRUE,
        defaultPageSize = 25,
        height = "50vh",
        class = "reactable-table",
        style = list(fontSize = "0.75em")
      )
    })
  })
}