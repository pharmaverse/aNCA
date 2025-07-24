#' Apply Labels to a dataset
#'
#' This function adds "label" attributes to all columns in a dataset
#'
#' @param data The dataset to which labels will be applied.
#' @param labels_df A data frame with three columns: Variable, Label, and Dataset,
#'  for the dataset you are applying it .
#' @param type The type variable in labels_df for which the labels are to be applied.
#'
#' @return The same dataset with label attributes applied to all columns.
#' If a column is not present in the labels list, it will be assigned the name of the col.
#' If label already exists in the original data, it will be preserved.
#'
#' @examples
#'  data <- data.frame(USUBJID = c(1, 2, 3), AVAL = c(4, 5, 6))
#'  labels <- data.frame(
#'    Variable = c("USUBJID", "AVAL"),
#'    Label = c("Unique Subject Identifier", "Analysis Value"),
#'    Dataset = c("ADPC", "ADPC")
#'  )
#'  data <- apply_labels(data, labels, "ADPC")
#'  print(attr(data$USUBJID, "label")) # "Unique Subject Identifier"
#'  print(attr(data$AVAL, "label"))    # "Analysis Value"
#'
#' @importFrom dplyr filter
#' @importFrom magrittr `%>%`
#' @importFrom stats setNames
#'
#' @export
apply_labels <- function(data, labels_df, type) {
  labels_df <- dplyr::filter(labels_df, Dataset == type)
  labels_reference <- stats::setNames(labels_df$Label, labels_df$Variable)

  for (col in colnames(data)) {
    if (!is.null(attr(data[[col]], "label"))) next # Preserve existing labels

    if (col %in% names(labels_reference)) {
      base::attr(data[[col]], "label") <- labels_reference[[col]]
    } else {
      base::attr(data[[col]], "label") <- col
    }

    # Check if the column is a factor and keep the levels order
    if (is.factor(data[[col]])) {
      data[[col]] <- as_factor_preserve_label(data[[col]])
    }
  }

  data
}

#' Convert to Factor While Preserving Label
#'
#' This function converts a vector to a factor while preserving its "label" attribute.
#'
#' @param x A vector to be converted to a factor.
#'
#' @return A factor with the original "label" attribute preserved.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   vec <- c("A", "B", "C")
#'   attr(vec, "label") <- "Example Label"
#'   factor_vec <- as_factor_preserve_label(vec)
#'   print(factor_vec)
#'   print(base::attr(factor_vec, "label"))
#' }
#'
#' @export
as_factor_preserve_label <- function(x) {
  label <- attr(x, "label")
  x <- as.factor(x)
  attr(x, "label") <- label
  return(x)
}

#' Check if a Vector Has a Label
#'
#' This function checks if a vector has a "label" attribute.
#'
#' @param x A vector to be checked for a "label" attribute.
#'
#' @return A logical value indicating whether the vector has a "label" attribute.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   vec <- c("A", "B", "C")
#'   attr(vec, "label") <- "Example Label"
#'   has_label(vec)  # Returns TRUE
#' }
#'
#' @export
has_label <- function(x) {
  return(!is.null(attr(x, "label")))
}

#' Set an Empty Label if None Exists
#'
#' This function sets an empty "label" attribute for a vector if it does not already have one.
#'
#' @param x A vector to be checked and potentially assigned an empty "label" attribute.
#'
#' @return The vector with an empty "label" attribute if it did not already have one.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   vec <- c("A", "B", "C")
#'   vec <- set_empty_label(vec)
#'   print(attr(vec, "label"))  # Returns ""
#' }
#'
#' @export
set_empty_label <- function(x) {
  if (is.null(attr(x, "label"))) {
    attr(x, "label") <- ""
  }
  return(x)
}

#' Get the Label of a Heading
#'
#' This function retrieves the label of a heading from a labels file.
#'
#' @param variable The variable for which the label is to be retrieved.
#' @param type The type of the dataset for which the label is to be retrieved.
#' @param labels_df A data frame with three columns: Variable, Label, and Dataset.
#'
#' @return The label of the heading if it exists in the labels file,
#' otherwise the variable name.
#'
#' @examples
#' \dontrun{
#'  # Example usage:
#'  LABELS <- data.frame(
#'  Variable = c("USUBJID", "AVAL"),
#'  Label = c("Unique Subject Identifier", "Analysis Value"),
#'  Dataset = c("ADPC", "ADPC")
#'  )
#'  get_label(LABELS, "USUBJID", "ADPC")  # Returns "Unique Subject Identifier"
#'  get_label(LABELS, "AGE", "ADPC")  # Returns "AGE"
#' }
#'
#' @export
get_label <- function(labels_df, variable, type) {
  label <- labels_df$Label[labels_df$Variable == variable & labels_df$Dataset == type]
  if (length(label) == 0) {
    return(variable)
  }
  label
}

#' Generate HTML Tooltip Text
#'
#' @details
#' Creates a character vector of HTML tooltips for each row of a data frame,
#' suitable for use with `ggplotly`.
#'  The output vector of this function should be added to original plotting data as a column,
#' which then can be used as tooltip argument in the plotting function.
#'
#' @param data A data.frame with the source data.
#' @param labels_df A data.frame used by `get_label()` to find variable labels.
#' @param tooltip_vars A character vector of column names to include in the tooltip.
#' @param type A character string specifying the label type for `get_label()`.
#'
#' @return A character vector of formatted HTML tooltip strings.
#'
#' @importFrom purrr pmap_chr map_chr
#'
#' @examples
#' # Sample data
#' my_data <- data.frame(
#'   USUBJID = c("Subject-01", "Subject-02"),
#'   DOSE = c(100, 200),
#'   RESPONSE = c(5.4, 8.1)
#'   )
#'
#' my_labels <- data.frame() # Dummy labels object
#'
#' vars_to_show <- c("USUBJID", "DOSE", "RESPONSE")
#'
#' # Generate the tooltip text vector
#' tooltips <- generate_tooltip_text(my_data, my_labels, vars_to_show, "ADPC")
#' my_data$tooltip <- tooltips
#'
#' @export
generate_tooltip_text <- function(data, labels_df, tooltip_vars, type) {

  tooltip_vars <- tooltip_vars[tooltip_vars %in% names(data)]

  if (length(tooltip_vars) == 0) {
    return(rep("", nrow(data)))
  }

  pmap_chr(
    .l = select(data, all_of(tooltip_vars)),
    .f = function(...) {

      row_values <- list(...)

      # For each variable, create a formatted line retrieving its label
      lines <- map_chr(tooltip_vars, ~ paste0(
        "<b>", get_label(labels_df, .x, type), "</b>: ", row_values[[.x]]
      ))

      # Paste all lines together with HTML line breaks
      paste(lines, collapse = "<br>")
    }
  )
}