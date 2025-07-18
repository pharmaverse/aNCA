
#' metadata_nca_parameters
#'
#' A dataset containing the mapping between PKNCA terms and CDISC terms. It mainly involves:
#' - Parameters: PPTEST, PPTESTCD
#' @format A data frame with 123 rows and 6 variables:
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
"metadata_nca_parameters"


#' metadata_nca_variables
#'
#' A dataset containing pharmacokinetic variable specifications.
#'
#' @format A data frame with 361 rows and 14 variables:
#' \describe{
#'   \item{Dataset}{Character. Indicates the dataset the variable belongs to (PP, ADPC, ADPP).}
#'   \item{Order}{Numeric. Variable order within its domain, based on Role, Core and Variable}
#'   \item{Variable}{Character. The short name of the variable.}
#'   \item{Label}{Character. A descriptive label for the variable.}
#'   \item{Type}{Character. Data type of the variable (Char, Num, text, integer, float, dateTime).}
#'   \item{Role}{Character. The CDISC role of the variable (e.g., Identifier, Topic, Timing...).}
#'   \item{Core}{Character. Indicates the core status of the variable
#'   (Req = Required, Perm = Permissible, Exp = Expected, Cond = Conditional).}
#'   \item{company_specific}{Logical. Indicates if the variable is company-specific (not CDISC).}
#'   \item{is.core}{Logical. TRUE if the variable is a core variable (always needed to be present).}
#'   \item{Length}{Numeric. The maximum length of the variable.}
#'   \item{Controlled_Terms}{Character. Reference to controlled terminology (e.g., C85839, C66731).}
#'   \item{is.used}{Logical. TRUE if the variable is meant to be included.}
#'   \item{Values}{Character. Possible values for the variable (if applicable).}
#'   \item{is.mapped}{Logical. TRUE if the variable is mapped in ADNCA (App's input).}
#' }
#' @source Used for PP and ADPP mapping rules and checks in the export_cdisc function
"metadata_nca_variables"
