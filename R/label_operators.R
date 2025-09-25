#' Apply Labels to a dataset
#'
#' This function adds "label" attributes to all columns in a dataset
#'
#' @param data The dataset to which labels will be applied.
#' @param labels_df A data frame containing at least the columns "Variable", "Label", and "Dataset".
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
#' @importFrom formatters var_labels
#'
#' @export
apply_labels <- function(data, labels_df = metadata_nca_variables, type = "ADPC") {
  formatters::var_labels(data) <- ifelse(
    is.na(formatters::var_labels(data)),
    get_label(names(data), type = type, labels_df = labels_df),
    formatters::var_labels(data)
  )

  data
}

#' Get the Label of a Heading
#'
#' This function retrieves the label of a heading from a labels file.
#'
#' @param variable The variable for which the label is to be retrieved.
#' @param type The type of the dataset for which the label is to be retrieved.
#' @param labels_df A data frame containing at least the columns "Variable", "Label", and "Dataset".
#'
#' @return The label of the heading if it exists in the labels file,
#' otherwise the variable name.
#'
#' @examples
#' \dontrun{
#'  # Example usage:
#'  LABELS <- data.frame(
#'    Variable = c("USUBJID", "AVAL"),
#'    Label = c("Unique Subject Identifier", "Analysis Value"),
#'    Dataset = c("ADPC", "ADPC")
#'  )
#'  get_label("USUBJID", "ADPC", LABELS)  # Returns "Unique Subject Identifier"
#'  get_label("AGE", "ADPC", LABELS)  # Returns "AGE"
#'  # Using a custom mapping column:
#'  LABELS2 <- data.frame(
#'    ColName = c("USUBJID", "AVAL"),
#'    Label = c("Unique Subject Identifier", "Analysis Value"),
#'    Dataset = c("ADPC", "ADPC")
#'  )
#'  get_label("USUBJID", "ADPC", LABELS2)
#'  # Returns "Unique Subject Identifier"
#' }
#'
#' @export
get_label <- function(variable, type = "ADPC", labels_df = metadata_nca_variables) {
  translate_terms(
    variable,
    "Variable",
    "Label",
    metadata = dplyr::filter(labels_df, Dataset == type)
  )
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
#' @examples
#' # Sample data
#' my_data <- data.frame(
#'   USUBJID = c("Subject-01", "Subject-02"),
#'   DOSE = c(100, 200),
#'   RESPONSE = c(5.4, 8.1)
#'   )
#'
#' my_labels <- data.frame(
#'   Dataset = "ADPC",
#'   Variable = "USUBJID",
#'   Label = "Unique Subject ID"
#'   ) # Dummy labels object
#'
#' vars_to_show <- c("USUBJID", "DOSE", "RESPONSE")
#'
#' # Generate the tooltip text vector
#' tooltips <- generate_tooltip_text(my_data, my_labels, vars_to_show, "ADPC")
#' my_data$tooltip <- tooltips
#'
#' @export
generate_tooltip_text <- function(data, labels_df, tooltip_vars, type) {

  if (nrow(data) == 0) {
    return(character())
  }

  tooltip_vars <- tooltip_vars[tooltip_vars %in% names(data)]

  if (length(tooltip_vars) == 0) {
    return(rep("", nrow(data)))
  }

  # Get all labels
  labels <- purrr::map_chr(tooltip_vars, ~ get_label(.x, type, labels_df = labels_df))

  # Create a list where each element is a vector of "Label: Value"
  # strings for an entire column
  tooltip_components <- purrr::map(seq_along(tooltip_vars), function(i) {
    var_name <- tooltip_vars[i]
    label <- labels[i]
    paste0("<b>", label, "</b>: ", data[[var_name]])
  })

  # Combine the components for each row
  do.call(paste, c(tooltip_components, sep = "<br>"))

}
