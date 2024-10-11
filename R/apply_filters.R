
#' Apply Filters to a Dataset
#'
#' This function applies a set of filters to a dataset. Each filter specifies a column, condition, and value to filter the dataset.
#'
#' @param raw_data A data frame containing the raw data to be filtered.
#' @param filters A list of filters, where each filter is a list containing the column, condition, and value.
#'
#' @return A data frame containing the filtered data.
#'
#' @details
#' The function iterates over the list of filters and applies each filter to the dataset. The supported conditions are `==`, `>`, `<`, `>=`, and `<=`.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   raw_data <- mtcars
#'   filters <- list(
#'     filter1 = list(column = "mpg", condition = ">", value = "20"),
#'     filter2 = list(column = "cyl", condition = "==", value = "6")
#'   )
#'   filtered_data <- apply_filters(raw_data, filters)
#' }
#'
#' @import dplyr
#' @export

#apply filters function
apply_filters <- function(raw_data, filters) {  
  for (filter_id in names(filters)) {  
    filter_info <- filters[[filter_id]]  
    
    if (!is.null(filter_info)) {  
      column <- filter_info$column  
      condition <- filter_info$condition  
      value <- filter_info$value  
      
      value <- as.numeric(value)  
      if (is.na(value)) {  
        value <- filter_info$value  
      }  
      
      if (condition == "==") {  
        raw_data <- raw_data %>% dplyr::filter(!!sym(column) == value)  
      } else if (condition == ">") {  
        raw_data <- raw_data %>% dplyr::filter(!!sym(column) > value)  
      } else if (condition == "<") {  
        raw_data <- raw_data %>% dplyr::filter(!!sym(column) < value)  
      } else if (condition == ">=") {  
        raw_data <- raw_data %>% dplyr::filter(!!sym(column) >= value)  
      } else if (condition == "<=") {  
        raw_data <- raw_data %>% dplyr::filter(!!sym(column) <= value)  
      }  
    }  
  }  
  return(raw_data)  
}  
