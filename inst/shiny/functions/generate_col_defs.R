#' Generate Column Definitions for Tables
#'
#' This function generates column definitions for a reactable table,
#' including both the column heading and the label (if available) as a tooltip.
#'
#' @param data A data frame containing the data to be displayed in the reactable table.
#'
#' @return A named list of column definitions for the reactable table.
#'
#' @examples
#' \dontrun{
#'   data <- data.frame(
#'     USUBJID = c(1, 2, 3),
#'     AVAL = c(4, 5, 6)
#'   )
#'   attr(data$USUBJID, "label") <- "Unique Subject Identifier"
#'   attr(data$AVAL, "label") <- "Analysis Value"
#'   col_defs <- generate_col_defs(data)
#' }
#'
#' @export
generate_col_defs <- function(data) {
  # Extract labels from the dataset
  labels <- sapply(data, function(col) attr(col, "label"))

  # Generate column definitions
  col_defs <- lapply(names(data), function(col) {
    label <- labels[[col]]
    if (!is.null(label)) {
      reactable::colDef(
        html = TRUE,
        header = htmltools::tags$span(
          col,
          `data-toggle` = "tooltip",
          `data-placement` = "top",
          title = label
        )
      )
    } else {
      reactable::colDef(name = col)
    }
  }) |>
    setNames(names(data))
}
