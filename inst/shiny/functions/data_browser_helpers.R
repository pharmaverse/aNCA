#' Helper functions
#' Inform missing values in a variable
#' @param x A vector to check for missing values
#' @returns A string with the number of missing values and their percentage
var_missings_info <- function(x) {
  sprintf("%s [%s%%]", sum(is.na(x)), round(mean(is.na(x) * 100), 2))
}

#' Validate input for plotting
#' @param input The Shiny input object
#' @param plot_var A reactive list containing the variable to be plotted
#' @param data_list_reactive A reactive list containing the datasets
#' @param dataset_name The name of the dataset to be validated
#' 
#' @returns A reactive expression that validates the input and returns TRUE if valid
validate_input <- function(input, plot_var, data_list_reactive, dataset_name) {
  reactive({
    req(dataset_name)
    varname <- plot_var$variable[[dataset_name]]
    
    validate(need(dataset_name, "No data selected"))
    validate(need(varname, "No variable selected"))
    df <- data_list_reactive[[dataset_name]]
    # These functions are in the `teal` package, which is now loaded.
    teal::validate_has_data(df, 1)
    teal::validate_has_variable(varname = varname, data = df, "Variable not available")
    
    TRUE
  })
}

#' Check if the variable is a numeric variable that should be treated as a factor
#' @param .unique_records_for_factor Integer defining the minimum number of unique records for factor variables
#' @param input The Shiny input object
#' @param data_for_analysis A reactive function returning the data for analysis
#' 
#' @returns TRUE if the variable is a numeric variable that should be treated as a factor, FALSE otherwise
is_num_var_short <- function(.unique_records_for_factor, input, data_for_analysis) {
  length(unique(data_for_analysis()$data)) < .unique_records_for_factor && !is.null(input$numeric_as_factor)
}


#' Get the data and variable description for plotting
#' @param input The Shiny input object
#' @param plot_var A reactive list containing the variable to be plotted
#' @param data_list_reactive A reactive list containing the datasets
#' @param dataset_name The name of the dataset to be plotted
#' 
#' @returns A list containing the data for plotting and the variable description
get_plotted_data <- function(input, plot_var, data_list_reactive, dataset_name) {
  varname <- plot_var$variable[[dataset_name]]
  df <- data_list_reactive[[dataset_name]]
  
  var_description <- attr(df[[varname]], "label") %||% varname
  list(data = df[[varname]], var_description = var_description)
}


#' Establish updating selection for variable browser
#' @param dataset_name The name of the dataset
#' @param input The Shiny input object
#' @param plot_var A reactive list containing the variable to be plotted
#' @param columns_names A list of column names for each dataset
#' 
#' @returns NULL
establish_updating_selection <- function(dataset_name, input, plot_var, columns_names) {
  table_ui_id <- paste0("variable_browser_", dataset_name)
  table_id_sel <- paste0(table_ui_id, "_rows_selected")
  observeEvent(input[[table_id_sel]], {
    plot_var$data <- dataset_name
    plot_var$variable[[dataset_name]] <- columns_names[[dataset_name]][input[[table_id_sel]]]
  })
}

#' Get the bin width for histograms
#' @param x_vec A numeric vector for which to calculate the bin width
#' @param scaling_factor A scaling factor to adjust the bin width
#' 
#' @returns A numeric value representing the bin width
get_bin_width <- function(x_vec, scaling_factor = 2) {
  x_vec <- x_vec[!is.na(x_vec)]
  qntls <- stats::quantile(x_vec, probs = c(0.1, 0.25, 0.75, 0.9), type = 2)
  iqr <- qntls[3] - qntls[2]
  binwidth <- max(scaling_factor * iqr / length(x_vec) ^ (1 / 3), sqrt(qntls[4] - qntls[1]))
  binwidth <- ifelse(binwidth == 0, 1, binwidth)
  x_span <- diff(range(x_vec))
  if (isTRUE(x_span / binwidth >= 2)) binwidth else x_span / 2
}


#' Remove outliers from a variable
#' @param var A numeric vector from which to remove outliers
#' @param outlier_definition A numeric value defining the outlier threshold, typically a multiplier of the interquartile range (IQR)
#' 
#' @returns A numeric vector with outliers removed based on the specified definition
remove_outliers_from <- function(var, outlier_definition) {
  if (outlier_definition == 0) {
    return(var)
  }
  q1_q3 <- stats::quantile(var, probs = c(0.25, 0.75), type = 2, na.rm = TRUE)
  iqr <- q1_q3[2] - q1_q3[1]
  var[var >= q1_q3[1] - outlier_definition * iqr & var <= q1_q3[2] + outlier_definition * iqr]
}

#' Replace variable types with icons
#' @param x A character vector containing variable types
#' 
#' @returns A character vector with variable types replaced by corresponding Font Awesome icons
variable_type_icons <- function(x) {
  x <- gsub("numeric", '<i class="fa fa-arrow-up-1-9"></i>', x, fixed = TRUE)
  x <- gsub("factor", '<i class="fa fa-list-ol"></i>', x, fixed = TRUE)
  x <- gsub("character", '<i class="fa fa-font"></i>', x, fixed = TRUE)
  x <- gsub("Date", '<i class="fa fa-calendar"></i>', x, fixed = TRUE)
  x <- gsub("POSIXct", '<i class="fa fa-calendar"></i>', x, fixed = TRUE)
  x <- gsub("POSIXlt", '<i class="fa fa-calendar"></i>', x, fixed = TRUE)
  x <- gsub("logical", '<i class="fa fa-toggle-on"></i>', x, fixed = TRUE)
  x <- gsub("primary_key", '<i class="fa fa-key"></i>', x, fixed = TRUE)
  x
}