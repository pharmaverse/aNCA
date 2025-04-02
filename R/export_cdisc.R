#' Export CDISC Data
#'
#' This function processes the results from a PKNCA and exports them into CDISC compliant datasets.
#' Attention: All parameters that do no match pptest dataframe will be lost in this pipeline!
#'
#'@details Outputs are the following:
#'  * pknca_result Output from function call `pk.nca()` (formatted)
#'  * pknca_result_raw Output from function call `pk.nca()` (needs to be merged with upper later
#'                      on but now we avoid merge conflict)
#'
#' @param res_nca Object with results of the NCA analysis.
#'
#' @return A list with two data frames:
#' \describe{
#' \item{pp}{A data frame containing the PP (Pharmacokinetic Parameters) domain data.}
#' \item{adpp}{A data frame containing the ADPP (Analysis Dataset for Pharmacokinetic Parameters)
#'            domain data.}
#' }
#'
#'
#' @import dplyr
#' @export
export_cdisc <- function(res_nca) {

  # Define group columns in the data
  group_cols <- unique(
    unlist(
      c(res_nca$data$conc$columns$groups,
        res_nca$data$dose$columns$groups,
        res_nca$data$dose$columns$route)
    )
  )

  # define columns needed for pp
  pp_cols <- c(
    "STUDYID",
    "DOMAIN",
    "USUBJID",
    "PPSEQ",
    "PPCAT",
    "PPGRPID",
    "PPSPID",
    "PPTESTCD",
    "PPTEST",
    "PPSCAT",
    "PPORRES",
    "PPORRESU",
    "PPSTRESC",
    "PPSTRESN",
    "PPSTRESU",
    "PPSTAT",
    "PPREASND",
    "PPSPEC",
    "PPRFTDTC",
    "PPSTINT",
    "PPENINT"
  )

  # define columns needed for adpp
  adpp_cols <- c("STUDYID",
                 "USUBJID",
                 "PPSEQ",
                 "PPGRPID",
                 "PPSPID",
                 "PARAMCD",
                 "PARAM",
                 "PARAMCAT",
                 "PPSCAT",
                 "PPREASND",
                 "PPSPEC",
                 "PPDTC",
                 "PPSTINT",
                 "PPENINT",
                 "SUBJID",
                 "SITEID",
                 "SEX",
                 "RACE",
                 "ACTARM",
                 "AAGE",
                 "AAGEU",
                 "TRT01P",
                 "TRT01A",
                 "AVAL",
                 "AVALC",
                 "AVALU")

  pp_info <- res_nca$result  %>%
    left_join(res_nca$data$dose$data,
              by = unname(unlist(res_nca$data$dose$columns$groups)),
              suffix = c("", ".y")) %>%
    group_by(
      across(all_of(c(
        unname(unlist(res_nca$data$conc$columns$groups)), "start", "end", "PPTESTCD"
      )))
    )  %>%
    arrange(USUBJID, DOSNO, !is.na(PPSTRES)) %>%
    # Identify all dulicates (fromlast and fromfirst) and keep only the first one
    filter(!duplicated(paste0(USUBJID, DOSNO, PPTESTCD))) %>%
    ungroup() %>%
    #  Recode PPTESTCD PKNCA names to CDISC abbreviations
    mutate(
      PPTESTCD = translate_terms(PPTESTCD, mapping_col = "PKNCA", target_col = "PPTESTCD"),
      PPTEST = translate_terms(PPTESTCD, mapping_col = "PPTESTCD", target_col = "PPTEST"),
      DOMAIN = "PP",
      # Group ID
      PPGRPID = {
        if ("PCGRPID" %in% names(.)) PCGRPID
        else if ("AVISIT" %in% names(.)) paste0(ANALYTE, DRUG, PCSPEC, AVISIT, collapse = "-")
        else paste0(DRUG, PCSPEC, DOSNO, collapse = "-")
      },
      # Parameter Category
      PPCAT = if ("PARAM" %in% names(.)) PARAM else ANALYTE,
      PPSCAT = "NON-COMPARTMENTAL",
      PPDOSNO = DOSNO,
      PPSPEC = PCSPEC,
      # Specific ID variables
      PPSPID = {
        if ("PCSPID" %in% names(.)) PCSPID
        else if ("EXSPID" %in% names(.)) EXSPID
        else NA
      },
      SUBJID = {
        if ("SUBJID" %in% names(.)) SUBJID
        else if ("USUBJID" %in% names(.)) {
          if ("STUDYID" %in% names(.)) stringr::str_remove(as.character(USUBJID),
                                                           paste0(as.character(STUDYID),
                                                                  "\\W?"))
          else gsub(find_common_prefix(USUBJID), "", USUBJID)
        }
      },
      # Parameter Variables
      PPORRES = round(as.numeric(PPORRES), 12),
      PPSTRESN = round(as.numeric(PPSTRES), 12),
      PPSTRESC = as.character(PPSTRESN),
      PPSTRESU = PPSTRESU,
      # Status and Reason for Exclusion
      PPSTAT = ifelse(is.na(PPSTRES) | (PPSTRES == 0 & PPTESTCD == "CMAX"), "NOT DONE",  ""),
      PPREASND = case_when(
        !is.na(exclude) ~ exclude,
        is.na(PPSTRES) ~ "Unspecified",
        TRUE ~ ""
      ),
      # Datetime
      PPDTC = Sys.time() %>% format("%Y-%m-%dT%H:%M"),
      PPRFTDTC = {
        if ("PCRFTDTC" %in% names(.)) {
          PCRFDTC
        } else if ("PCRFTDTM" %in% names(.)) {
          strptime(PCRFTDTM, format = "%Y-%m-%d %H:%M:%S") %>% format("%Y-%m-%dT%H:%M")
        } else {
          NA
        }
      },
      # Matrix
      PPSPEC = PCSPEC,
      # TODO start and end intervals in case of partial aucs -> see oak file in templates
      PPSTINT = ifelse(
        startsWith(PPTESTCD, "AUCINT"),
        convert_to_iso8601_duration(start, RRLTU),
        NA
      ),
      PPENINT = ifelse(
        startsWith(PPTESTCD, "AUCINT"),
        convert_to_iso8601_duration(end, RRLTU),
        NA
      ),
      PPREASND = substr(exclude, 1, 200)
    ) %>%
    # Map PPTEST CDISC descriptions using PPTESTCD CDISC names
    group_by(USUBJID)  %>%
    mutate(PPSEQ = if ("PCSEQ" %in% names(.)) PCSEQ else row_number())  %>%
    ungroup()

  # select pp columns
  pp <- pp_info %>%  select(all_of(pp_cols))

  adpp <- pp_info %>%
    # Rename/mutate variables from PP
    mutate(AVAL = PPSTRESN, AVALC = PPSTRESC, AVALU = PPSTRESU,
           PARAMCD = PPTESTCD, PARAM = PPTEST, PARAMCAT = PPCAT) %>%
    select(any_of(c(group_cols, adpp_cols, "RACE", "SEX", "AGE", "AGEU", "AVISIT")))

  # Keep StudyID value to use for file naming
  studyid <- if ("STUDYID" %in% names(pp_info)) unique(pp_info$STUDYID)[1] else ""

  list(pp = pp, adpp = adpp, studyid = studyid)
}


#' @title
#' @description Function to identify the common prefix in a character vector.
#' @details Assumes all characters share the same prefix for sure. .
#' @noRd
#' @examples
#' # Example usage of the function (if applicable)
#' # find_common_prefix(c("abc-100", "abc-102", "abc-103"))
#' 
#' @tests
#' # Add test cases to validate the function's behavior
#' # testthat::expect_equal(find_common_prefix(c("abcd", "abce")), "abc")
#' # testthat::expect_equal(find_common_prefix(c("X01-111", "X01-222")), "X01")
#' 
.find_common_prefix <- function(strings) {
  # Get the strings with the greatest prefix mismatch
  diff_strings <- sort(strings)[c(1, length(strings))] %>%
    # For the comparison make all have same number of letters
    sapply(function(x) substr(x, 0, min(nchar(strings)))) %>%
    # Separate the strings by letters
    strsplit("")
  
  # Get the common prefix by using the first letter mismatch between the two strings
  first_mistmatch <- which(diff_strings[[1]] != diff_strings[[2]])[1]
  substr(strings[1], 0, first_mistmatch - 1)
}
