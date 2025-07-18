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

      data <- processed_pknca_data()$intervals %>%
        apply_labels(LABELS, "ADPC") %>%
        select(where(~!is.logical(.) | any(. == TRUE)))

      route_column <- "ROUTE"
      std_route_column <- "std_route"
      col_groups <- unname(unlist(processed_pknca_data()$dose$columns$groups))

      data <- data %>%
        left_join(
          processed_pknca_data()$dose$data %>%
            select(all_of(c(
              col_groups, route_column, std_route_column, "TIME_DOSE", "NCA_PROFILE", "DOSNOA"
            ))),
          by = c(col_groups, "TIME_DOSE", "NCA_PROFILE", "DOSNOA")
        ) %>%
        group_by(across(all_of(unname(unlist(processed_pknca_data()$dose$columns$groups))))) %>%
        arrange(!!!syms(unname(unlist(processed_pknca_data()$conc$columns$groups))), TIME_DOSE) %>%
        mutate(start = start - TIME_DOSE, end = end - TIME_DOSE) %>%
        select(!!!syms(colnames(data)), all_of(c(route_column, std_route_column)))

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