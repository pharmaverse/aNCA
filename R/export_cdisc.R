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

  cdisc_info <- res_nca$result  %>%
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
        if ("AVISIT" %in% names(.)) paste(ANALYTE, PCSPEC, AVISIT, sep = "-")
        else if ("VISIT" %in% names(.)) paste(ANALYTE, PCSPEC, VISIT, sep = "-")
        else paste(ANALYTE, PCSPEC, DOSNO, sep = "-")
      },
      # Parameter Category
      PPCAT = ANALYTE,
      PPSCAT = "NON-COMPARTMENTAL",
      PPDOSNO = DOSNO,
      PPSPEC = PCSPEC,
      # Specific ID variables
      PPSPID = paste0("/F:EDT-", STUDYID, "_PKPARAM_aNCA"),
      SUBJID = get_subjid(.),
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
  pp <- cdisc_info %>%
    select(all_of(CDISC_COLS$PP))

  adpp <- cdisc_info %>%
    # Rename/mutate variables from PP
    mutate(AVAL = PPSTRESN, AVALC = PPSTRESC, AVALU = PPSTRESU,
           PARAMCD = PPTESTCD, PARAM = PPTEST) %>%
    select(any_of(c(CDISC_COLS$ADPP, "RACE", "SEX", "AGE", "AGEU", "AVISIT")))

  adpc <- res_nca$data$conc$data %>%
    mutate(
      ANL01FL = ifelse(is.excluded.hl, NA_character_, "Y"),
      SUBJID = get_subjid(.),
      ATPT = {
        if ("PCTPT" %in% names(.)) PCTPT
        else NA_character_
      },
      ATPTN = {
        if ("PCTPTNUM" %in% names(.)) PCTPTNUM
        else NA
      },
      ATPTREF = {
        if ("PCTPTREF" %in% names(.)) PCTPTREF
        else NA_character_
      }
    ) %>%
    # Order columns using a standard, and then put the rest of the columns
    select(any_of(CDISC_COLS$ADPC), everything())  %>%
    # Deselect columns that are only used internally in the App
    select(-any_of(INTERNAL_ANCA_COLS))

  # Keep StudyID value to use for file naming
  studyid <- if ("STUDYID" %in% names(cdisc_info)) unique(cdisc_info$STUDYID)[1] else ""

  list(pp = pp, adpp = adpp, adpc = adpc, studyid = studyid)
}

#' Function to identify the common prefix in a character vector.
#' @details
#' Checks the common prefix for all provided strings. If no
#' common prefix is detected, returns empty string.
#'get
#' @param strings Character vector with strings to check.
#' @returns A character string with common prefix.
#'
#' @examples
#' find_common_prefix(c("abc-100", "abc-102", "abc-103")) # "abc-10"
#' @noRd
#' @keywords internal
find_common_prefix <- function(strings) {
  # Get the strings with the greatest prefix mismatch
  letters <- sort(strings) %>%
    .[c(1, length(.))] %>%
    # For the comparison make all have same number of letters
    sapply(\(x) substr(x, 0, min(nchar(.)))) %>%
    strsplit("")

  mismatch <- letters[[1]] != letters[[2]]

  substr(strings[[1]], 0, which(mismatch)[1] - 1)
}

#' Helper function to extract SUBJID from data
#'
#' This function extracts the `SUBJID` from the provided dataset. If `SUBJID` is not available,
#' it attempts to derive it from `USUBJID` by removing the `STUDYID` prefix or the common prefix
#' shared by all `USUBJID` values.
#'
#' @details
#' The function first checks if `SUBJID` exists in the dataset. If not, it derives `SUBJID` from
#' `USUBJID` by:
#' \itemize{
#'   \item Removing the `STUDYID` prefix if it exists.
#'   \item Removing the longest common prefix shared by all `USUBJID` values.
#' }
#' If neither `SUBJID` nor `USUBJID` is available, the function returns `NA`.
#'
#' @param data A data frame containing `SUBJID`, `USUBJID`, and optionally `STUDYID`.
#'
#' @return A vector of `SUBJID` values.
#'
#' @examples
#' data <- data.frame(
#'   STUDYID = c("STUDY1", "STUDY1"),
#'   USUBJID = c("STUDY1-001", "STUDY1-002")
#' )
#' get_subjid(data)
#'
#' @noRd
#' @keywords internal
get_subjid <- function(data) {
  if ("SUBJID" %in% names(data)) {
    data$SUBJID
  } else if ("USUBJID" %in% names(data)) {
    if ("STUDYID" %in% names(data)) {
      stringr::str_remove(as.character(data$USUBJID), paste0(as.character(data$STUDYID), "\\W?"))
    } else {
      gsub(find_common_prefix(data$USUBJID), "", data$USUBJID)
    }
  } else {
    NA
  }
}


CDISC_COLS <- list(
  ADPC = c(
    "STUDYID",
    "SUBJID",
    "USUBJID",
    "SITEID",
    "VISITNUM",
    "VISIT",
    "AVISIT",
    "AVISITN",
    "PCSTRESC",
    "PCSTRESN",
    "PCSTRESU",
    "PCORRES",
    "PCORRESU",
    "PCTPT",
    "PCTPTNUM",
    "ATPT",
    "ATPTN",
    "PCSTRESC",
    "PCSTRESN",
    "PCSTRESU",
    "AVAL",
    "ANL01FL"
  ),

  ADPP = c(
    "STUDYID",
    "USUBJID",
    "PPSEQ",
    "PPGRPID",
    "PPSPID",
    "PARAMCD",
    "PARAM",
    "PPCAT",
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
    "AVALU"
  ),

  PP = c(
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
)

INTERNAL_ANCA_COLS <- c(
  "exclude", "is.excluded.hl", "volume", "std_route",
  "duration", "TIME", "IX", "exclude_half.life", "is.included.hl",
  "conc_groups", "REASON"
)
