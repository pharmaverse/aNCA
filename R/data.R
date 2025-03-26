
#' dict_pknca_cdisc
#'
#' A dataset containing the mapping between PKNCA terms and CDISC terms. It mainly involves:
#' - Parameters: PPTEST, PPTESTCD
#' @format A data frame with 120 rows and 6 variables:
#' \describe{
#'   \item{PKNCA}{PKNCA term}
#'   \item{PPTESTCD}{CDISC term}
#'   \item{Label}{Description of the term}
#'   \item{PPTEST}{Official CDISC term}
#'   \item{PPTESTCD}{Official CDISC short code term}
#'   \item{unit_type}{Type of unit associated with the term}
#'   \item{pretty_name}{PKNCA pretty name of the term}
#'   \item{description}{PKNCA description of the term}
#'   \item{FUN}{PKNCA function used to calculate the parameter}
#'   \item{input_names}{Combination of PPTESTCD + ": " + PPTEST. Used for App inputs}
#'   \item{is_cdisc_sure}{Logical indicating if the term is a CDISC official name}\
#' }
#' @source Generated for use in the `translate_nomenclature` function.
"dict_pknca_cdisc"
