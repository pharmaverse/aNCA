#' Generate Column Definitions for Tables
#'
#' This function generates column definitions for a reactable table,
#' including both the column heading and the label (if available) as a tooltip.
#'
#' @param data A data frame containing the data to be displayed in the reactable table.
#'
#' @return A named list of column definitions for the reactable table.
#'
#' @importFrom htmltools tags
#' @examples
#' data <- data.frame(
#'   USUBJID = c(1, 2, 3),
#'   AVAL = c(4, 5, 6)
#' )
#' attr(data$USUBJID, "label") <- "Unique Subject Identifier"
#' attr(data$AVAL, "label") <- "Analysis Value"
#' col_defs <- generate_col_defs(data)
#' }
#'
#' @export
generate_col_defs <- function(data) {

  purrr::imap(data, ~{
    label <- unname(attr(.x, "label"))
    col_name <- .y

    if (!is.null(label)) {
      reactable::colDef(
        html = TRUE,
        header = tags$span(
          col_name,
          `data-toggle` = "tooltip",
          `data-placement` = "top",
          title = label
        )
      )
    } else {
      reactable::colDef(name = col_name)
    }
  })
}
