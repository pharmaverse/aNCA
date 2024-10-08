#' Create a Filter UI Component
#'
#' This function generates a UI component for creating a filter in a Shiny application. The filter allows users to select a column, condition, and value to filter a dataset.
#'
#' @param filter_id A unique identifier for the filter.
#' @param dataset A data frame containing the dataset to be filtered.
#'
#' @return A Shiny UI component for creating a filter.
#'
#' @details
#' The function creates a UI component with the following elements:
#' \itemize{
#'   \item A horizontal rule (`<hr>`).
#'   \item A dropdown menu for selecting a column from the dataset.
#'   \item A dropdown menu for selecting a condition (e.g., `==`, `>`, `<`, `>=`, `<=`).
#'   \item A text input for specifying the filter value.
#'   \item A submit button to confirm the filter.
#'   \item A remove button to remove the filter.
#' }
#'
#' @examples
#' \dontrun{
#'   # Example usage in a Shiny app:
#'   ui <- fluidPage(
#'     create_filter("filter1", mtcars)
#'   )
#'   server <- function(input, output, session) {}
#'   shinyApp(ui, server)
#' }
#'
#' @import shiny
#' @export

#create filters function
create_filter <- function(filter_id, dataset) {  
  tags$div(  
    id = filter_id,  
    tags$hr(),  
    fluidRow(
      column(5, selectInput(paste0(filter_id, "_column"), "Column:", choices = colnames(dataset))),
      column(3, selectInput(paste0(filter_id, "_condition"), "cond.", choices = c("==", ">", "<", ">=", "<="))),
      column(4, textInput(paste0(filter_id, "_value"), "Value:"))
    ),
    fluidRow(
      column(4, actionButton(paste0(filter_id, "_confirm"), "Submit")),
      column(4,  actionButton(paste0(filter_id, "_remove"), "Remove Filter"))
    )
  )  
}   

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
