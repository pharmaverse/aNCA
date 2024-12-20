#' Apply Labels to a dataset
#'
#' This function adds "label" attributes to all columns in a dataset
#' Using the labels_app.csv file which contains common labels for ADPP
#' and ADPC column names
#'
#' @param data The dataset to which labels will be applied.
#'
#' @return The same dataset with label attributes applied to all columns.
#' If a column is not present in the labels list, it will be assigned the name of the col.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   data <- data.frame(USUBJID = c(1, 2, 3), AVAL = c(4, 5, 6))
#'   data <- apply_labels(data)
#'   print(attr(data$A, "label"))
#' }
#'
#' @export
apply_labels <- function(data) {

  labels_app <- read.csv(system.file("shiny/www/data/labels_app.csv", package = "aNCA"))

  # Create the label_ADNCA named vector from labels_app
  label_adnca <- setNames(labels_app$Label, labels_app$Variable)

  log_info("Applying labels to data")

  for (col in colnames(data)) {
    if (col %in% names(label_adnca)) {
      attr(data[[col]], "label") <- label_adnca[[col]]
    } else {
      attr(data[[col]], "label") <- col
    }

    # Check if the column is a factor and keep the levels order
    if (is.factor(data[[col]])) {
      data[[col]] <- as_factor_preserve_label(data[[col]])
    }
  }

  return(data)
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
#'   print(attr(factor_vec, "label"))
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
