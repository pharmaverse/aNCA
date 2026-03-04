
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
#'   \item{Depends}{PKNCA derived. Designates all directly used parameters in calculation}
#'   \item{can_excretion}{Logical. Indicates if the parameter can be used in excretion analysis}
#'   \item{can_non_excretion}{Logical.
#'    Indicates if the parameter can be used in non-excretion analysis}
#'   \item{can_single_dose}{Logical.
#'    Indicates if the parameter can be used in single dose analysis}
#'   \item{can_multiple_dose}{Logical.
#'    Indicates if the parameter can be used in multiple dose analysis}
#'   \item{can_extravascular}{Logical.
#'    Indicates if the parameter can be used in extravascular analysis}
#'   \item{can_metabolite}{Logical.
#'    Indicates if the parameter can be used in metabolite analysis}
#' }
#' @source Generated for use in the `translate_nomenclature` function.
"metadata_nca_parameters"


#' metadata_nca_variables
#'
#' A dataset containing pharmacokinetic variable specifications.
#'
#' @format A data frame with 361 rows and 14 variables:
#' \describe{
#'   \item{Dataset}{Character. Indicates the dataset the variable belongs to (PP, ADNCA, ADPP).}
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
#'   \item{Values}{Character. Possible values (if applicable) for the variable separated by ', '.}
#'   \item{is.mapped}{Logical. TRUE if the variable is mapped in ADNCA (App's input).}
#'   \item{mapping_tooltip}{Character. Tooltip text for mapping guidance in the App.}
#'   \item{mapping_section}{Character. Mapping section where the variable is classified in the App.}
#'   \item{mapping_alternatives}{Character. Alternative column names for the variable.}
#'   \item{mapping_order}{Numeric. Defines the mapped variables order in the mapped dataset}
#' }
#' @source Used for PP and ADPP mapping rules and checks in the export_cdisc function
"metadata_nca_variables"


#' adnca_example
#'
#' A pharmacokinetic concentration-time dataset containing multiple dose administration data
#' with specimen types (SERUM and URINE), dosing information, and subject demographics.
#' This is an example dataset for demonstrating pharmacokinetic analysis workflows.
#' It follows the ADaMIG-ADNCA v1.0 structure.
#'
#' @format A data frame with 76 rows and 32 variables:
#' \describe{
#'   \item{STUDYID}{Character. Study identifier.}
#'   \item{USUBJID}{Character. Unique subject identifier.}
#'   \item{PCSPEC}{Character. Specimen type (SERUM or URINE).}
#'   \item{PARAM}{Character. Parameter name (e.g., DrugA).}
#'   \item{METABFL}{Character. Metabolite flag indicator.}
#'   \item{AFRLT}{Numeric. Actual relative time to first dose (hours).}
#'   \item{NFRLT}{Numeric. Nominal relative time to first dose (hours).}
#'   \item{ARRLT}{Numeric. Actual relative time to reference dose (hours).}
#'   \item{NRRLT}{Numeric. Nominal relative time to reference dose (hours).}
#'   \item{TRTRINT}{Numeric. Treatment interval in hours.}
#'   \item{RRLTU}{Character. Relative reference time units.}
#'   \item{AVAL}{Numeric. Actual analysis value (concentration).}
#'   \item{AVALU}{Character. Analysis value units (ug/mL or mg/mL).}
#'   \item{VOLUME}{Numeric. Volume of specimen collected (mL for urine).}
#'   \item{VOLUMEU}{Character. Volume units.}
#'   \item{DOSETRT}{Character. Dose treatment identifier.}
#'   \item{TRT01A}{Character. Actual treatment received (formatted dose and route).}
#'   \item{DOSEA}{Numeric. Actual dose amount (mg).}
#'   \item{DOSEU}{Character. Dose units.}
#'   \item{ROUTE}{Character. Route of administration.}
#'   \item{ADOSEDUR}{Numeric. Actual dose duration (hours).}
#'   \item{ATPTREF}{Character. Analysis time point reference (e.g., DOSE 1).}
#'   \item{AGE}{Numeric. Subject age.}
#'   \item{AGEU}{Character. Age units.}
#'   \item{RACE}{Character. Race/ethnicity of subject.}
#'   \item{SEX}{Character. Sex of subject.}
#'   \item{NCA1XRS}{Character. NCA result code 1 (if applicable).}
#'   \item{NCA2XRS}{Character. NCA result code 2 (if applicable).}
#'   \item{NEFRLT}{Numeric. Nominal relative time to first dose (alternative column).}
#'   \item{NERRLT}{Numeric. Nominal relative time to reference dose (alternative column).}
#'   \item{AEFRLT}{Numeric. Actual relative time to first dose (alternative column).}
#'   \item{AERRLT}{Numeric. Actual relative time to reference dose (alternative column).}
#' }
#' @details
#' This dataset contains pharmacokinetic data from multiple subjects receiving
#' multiple dose regimens with different levels and routes of administration at 24-hour intervals.
#' Concentration measurements are collected at multiple time points post-dose for both serum
#' and urine samples. Analytes include parent drug and a metabolite.
#'
#' @source Example data created for package demonstration and testing purposes.
"adnca_example"
