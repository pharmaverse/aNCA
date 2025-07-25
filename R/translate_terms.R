#' Translate Terms from One Nomenclature to Another
#'
#' This function translates a character vector of terms from one nomenclature to another using a
#' mapping file.
#'
#' @param input_terms A character vector of terms to be translated.
#' @param mapping_col Character indicating the column name in the metadata file of the input terms.
#' Default is "PKNCA".
#' @param target_col Character indicating the column name in the metadata file of the target terms.
#' Default is "PPTESTCD".
#' @param metadata Dataset used to do the mapping that contains the mapping and target columns.
#' @returns A character vector of translated terms. Input terms not available in mapping_col will be
#' returned with the same value.
#' @examples
#' input_terms <- c("adj.r.squared", "ae", "nonexistent_term")
#' translate_terms(input_terms)
#' @export
translate_terms <- function(input_terms,
                            mapping_col = "PKNCA",
                            target_col = "PPTESTCD",
                            metadata = metadata_nca_parameters) {

  # Check if mapping_col and target_col are single character strings
  if (length(c(mapping_col, target_col)) != 2 ||
        !is.character(mapping_col) || !is.character(target_col)) {
    stop("mapping_col and target_col must be single character strings.")
  }

  # Check if mapping_col and target_col exists in metadata
  if (!mapping_col %in% colnames(metadata) || !target_col %in% colnames(metadata)) {
    stop("mapping_col and target_col must exist in metadata.")
  }

  # Create a named vector for translation
  translation_vector <- setNames(metadata[[target_col]], metadata[[mapping_col]])

  # Translate terms
  translated_terms <- ifelse(input_terms %in% names(translation_vector),
                             translation_vector[input_terms],
                             input_terms)

  translated_terms
}
