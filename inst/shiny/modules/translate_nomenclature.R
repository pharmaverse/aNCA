#' Translate Terms from One Nomenclature to Another
#'
#' This function translates a character vector of terms from one nomenclature to another using a specified mapping.
#'
#' @param input_terms A character vector of terms to be translated.
#' @param mapping_col A single character specifying the column name in the metadata file that contains the input terms. Default is "PKNCA".
#' @param target_col A single character specifying the column name in the metadata file that contains the target terms. Default is "CDISC".
#' @return A character vector of translated terms. If a term from the input is not in the mapping column, it returns the original value.
#' @examples
#' input_terms <- c("adj.r.squared", "ae", "nonexistent_term")
#' translate_terms(input_terms)
translate_terms <- function(input_terms, mapping_col = "PKNCA", target_col = "CDISC") {
  # Load the metadata file
  metadata <- read.csv("www/metadata/Parameter_labels_PKNCA-CDISC.csv")

  # Create a named vector for translation
  translation_vector <- setNames(metadata[[target_col]], metadata[[mapping_col]])

  # Translate terms
  translated_terms <- ifelse(input_terms %in% names(translation_vector), translation_vector[input_terms], input_terms)

  return(translated_terms)
}