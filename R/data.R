
#' pknca_cdisc_terms
#'
#' A dataset containing the mapping between PKNCA terms and CDISC terms. It mainly involves:
#' - Parameters: PPTEST, PPTESTCD
#' @format A data frame with 120 rows and 6 variables:
#' \describe{
#'   \item{PKNCA}{PKNCA term}
#'   \item{PPTESTCD}{CDISC term}
#'   \item{PPTEST}{Official CDISC term}
#'   \item{input_names}{Combination of PPTESTCD + ": " + PPTEST. Used for App inputs}
#'   \item{FUN}{PKNCA function used to calculate the parameter}
#'   \item{description}{PKNCA description of the term}
#'   \item{is_cdisc_sure}{Logical indicating if the term is a CDISC official name}
#'   \item{unit_type}{Type of unit associated with the term}
#'   \item{TYPE}{Type of data associated with the parameter/term}
#'   \item{CAT}{Arbitrary assigned subclass for the parameter/term}
#'   \item{Depends}{PKNCA derived. Designates all directly used parameters in calculation}\
#' }
#' @source Generated for use in the `translate_nomenclature` function.
"pknca_cdisc_terms"
