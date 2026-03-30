
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
#' @format A data frame with 359 rows and 19 variables:
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
#'   \item{allow_create_numeric}{Logical.
#'    Indicates if the mapping input should allow the user to introduce custom numbers}
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


#' pc_example
#'
#' An SDTM PC-like pharmacokinetic concentrations dataset derived from
#' \code{\link{adnca_example}}. Intended to support and test the PC+EX upload
#' workflow (see GitHub issue #624).
#'
#' @format A data frame with the same number of rows as \code{adnca_example}
#'   and 11 variables:
#' \describe{
#'   \item{DOMAIN}{Character. Domain Abbreviation.}
#'   \item{STUDYID}{Character. Study identifier.}
#'   \item{USUBJID}{Character. Unique subject identifier.}
#'   \item{PCTEST}{Character. Name of the measured analyte (mapped from PARAM).}
#'   \item{PCSPEC}{Character. Specimen type (e.g., SERUM, URINE).}
#'   \item{PCSTRESN}{Numeric. Numeric concentration result (mapped from AVAL).}
#'   \item{PCSTRESU}{Character. Concentration units (mapped from AVALU).}
#'   \item{PCDTC}{Character. Sample collection datetime (ISO 8601), synthesized
#'     from AFRLT using an arbitrary reference origin.}
#'   \item{PCRFTDTC}{Character. Reference dose datetime (ISO 8601), synthesized
#'     from AFRLT - ARRLT.}
#'   \item{PCELTM}{Character. Planned elapsed time from reference dose in
#'     ISO 8601 duration format (e.g., "PT0.5H"), derived from NFRLT.}
#'   \item{VOLUME}{Numeric. Volume of specimen collected (mL), for urine samples.}
#'   \item{VOLUMEU}{Character. Volume units.}
#' }
#' @source Derived from \code{\link{adnca_example}} and \code{\link{dm_example}}
#'   via \code{data-raw/sdtm_example.R}.
"pc_example"


#' ex_example
#'
#' An SDTM EX-like exposure/dosing dataset derived from
#' \code{\link{adnca_example}}. Contains one row per dosing event per subject.
#' Intended to support and test the PC+EX upload workflow (see GitHub issue #624).
#'
#' @format A data frame with 11 variables:
#' \describe{
#'   \item{DOMAIN}{Character. Domain abbreviation.}
#'   \item{STUDYID}{Character. Study identifier.}
#'   \item{USUBJID}{Character. Unique subject identifier.}
#'   \item{EXTRT}{Character. Name of the treatment (mapped from DOSETRT).}
#'   \item{EXDOSE}{Numeric. Dose amount (mapped from DOSEA).}
#'   \item{EXDOSU}{Character. Dose units (mapped from DOSEU).}
#'   \item{EXROUTE}{Character. Route of administration (mapped from ROUTE).}
#'   \item{EXSTDTC}{Character. Dosing start datetime (ISO 8601), synthesized
#'     from AFRLT - ARRLT using the subject's RFXSTDTC as reference origin.}
#'   \item{EXENDTC}{Character. Dosing end datetime (ISO 8601), computed as
#'     EXSTDTC + ADOSEDUR.}
#'   \item{EXDUR}{Character. Duration of dose in ISO 8601 duration format
#'     (e.g., "PT2.9H"), derived from ADOSEDUR.}
#'   \item{EXELTM}{Character. Planned elapsed time from first dose in ISO 8601
#'     duration format (e.g., "PT0H"), derived from NFRLT - NRRLT relative to
#'     the subject's first dose.}
#' }
#' @source Derived from \code{\link{adnca_example}} and \code{\link{dm_example}}
#'   via \code{data-raw/sdtm_example.R}.
"ex_example"


#' dm_example
#'
#' An SDTM DM-like demographics dataset derived from
#' \code{\link{adnca_example}}. Contains one row per subject with demographic
#' variables and a synthetic treatment start date (RFXSTDTC) that serves as the
#' reference origin for datetimes in \code{\link{pc_example}} and
#' \code{\link{ex_example}}.
#'
#' @format A data frame with one row per subject and 9 variables:
#' \describe{
#'   \item{DOMAIN}{Character. Domain Abbreviation.}
#'   \item{STUDYID}{Character. Study identifier.}
#'   \item{USUBJID}{Character. Unique subject identifier.}
#'   \item{AGE}{Numeric. Age of the subject.}
#'   \item{AGEU}{Character. Age units (e.g., "Years").}
#'   \item{SEX}{Character. Sex of the subject.}
#'   \item{RACE}{Character. Race of the subject.}
#'   \item{ARM}{Character. Planned treatment arm (mapped from TRT01A).}
#'   \item{ACTARM}{Character. Actual treatment arm (mapped from TRT01A).}
#'   \item{RFXSTDTC}{Character. Date/time of first study treatment (ISO 8601).
#'     Synthetic, staggered by 3 days per subject to simulate different
#'     enrollment dates.}
#' }
#' @source Derived from \code{\link{adnca_example}} via \code{data-raw/sdtm_example.R}.
"dm_example"
