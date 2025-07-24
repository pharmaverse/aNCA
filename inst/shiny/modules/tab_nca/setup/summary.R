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
  reactable_ui(ns("nca_intervals_summary"))
}

summary_server <- function(id, processed_pknca_data) {
  moduleServer(id, function(input, output, session) {
    summary_data <- reactive({
      req(processed_pknca_data())

      conc_group_columns <- group_vars(processed_pknca_data()$conc)
      dose_group_columns <- group_vars(processed_pknca_data()$dose)
      time_dose <- processed_pknca_data()$dose$columns$time

      data <- processed_pknca_data()$intervals %>%
        apply_labels(LABELS, "ADPC") %>%
#<<<<<<< 527-enhancement-add-option-to-download-files-to-reactable-tables
        select(where(~!is.logical(.) | any(. == TRUE)))

      route_column <- "ROUTE"
      std_route_column <- "std_route"
      col_groups <- unname(unlist(processed_pknca_data()$dose$columns$groups))

      data %>%
#=======
        select(where(~!is.logical(.) | any(. == TRUE))) %>%
#>>>>>>> main
        left_join(
          processed_pknca_data()$dose$data %>%
            mutate(TIME_DOSE = .[[time_dose]]) %>%
            select(any_of(c(dose_group_columns, "TIME_DOSE")))
        ) %>%
#<<<<<<< 527-enhancement-add-option-to-download-files-to-reactable-tables
        group_by(across(all_of(unname(unlist(processed_pknca_data()$dose$columns$groups))))) %>%
        arrange(!!!syms(unname(unlist(processed_pknca_data()$conc$columns$groups))), TIME_DOSE) %>%
        mutate(start = start - TIME_DOSE, end = end - TIME_DOSE) %>%
        select(!!!syms(colnames(data)), all_of(c(route_column, std_route_column)))
#=======
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
#>>>>>>> main
    })

    reactable_server(
      "nca_intervals_summary",
      summary_data,
      columns = generate_col_defs,
      height = "98vh"
    )
  })
}