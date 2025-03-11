#' Apply Filters to a Dataset
#'
#' This function applies a set of filters to a dataset. Each filter specifies a column,
#' condition, and value to filter the dataset.
#'
#' @param data A data frame containing the raw data to be filtered.
#' @param filters  A list of filters, where each filter is a list containing
#'                 the column, condition, and value.
#'
#' @returns A data frame containing the filtered data.
#'
#' @details
#' The function iterates over the list of filters and applies each filter to the dataset.
#' The supported conditions are `==`, `>`, `<`, `>=`, and `<=`.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   data <- mtcars
#'   filters <- list(
#'     list(column = "mpg", condition = ">", value = "20"),
#'     list(column = "cyl", condition = "==", value = "6")
#'   )
#'   filtered_data <- apply_filters(data, filters)
#' }
#'
#' @export
apply_filters <- function(data, filters) {
  for (filter in filters) {
    # check if filter is valid #
    if (is.null(filter)) next
    missing_fields <- setdiff(c("column", "condition", "value"), names(filter))
    if (length(missing_fields) > 0) {
      stop("Missing required filter fields: ", paste0(missing_fields, collapse = ", "))
    }
    
    column <- filter$column
    value <- filter$value
    
    # check if data if correct #
    if (!column %in% names(data)) stop("Data is missing filtered column: ", column)
    
    if (is.numeric(data[[column]])) {
      value <- as.numeric(value)
    }
    
    switch(
      filter$condition,
      "==" = {
        data <- dplyr::filter(data, !!sym(column) == value)
      },
      ">" = {
        data <- dplyr::filter(data, !!sym(column) > value)
      },
      "<" = {
        data <- dplyr::filter(data, !!sym(column) < value)
      },
      ">=" = {
        data <- dplyr::filter(data, !!sym(column) >= value)
      },
      "<=" = {
        data <- dplyr::filter(data, !!sym(column) <= value)
      }
    )
  }
  
  data
}
