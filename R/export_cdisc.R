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
                 "DOMAIN",
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
      PPGRPID =  {
        if ("PCGRPID" %in% names(.)) PCGRPID
        else if ("AVISIT" %in% names(.)) paste0(DRUG, "-", PCSPEC, "-", AVISIT)
        else paste0(DRUG, "-", PCSPEC, "-", DOSNO)
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
      PPSTINT = ifelse(startsWith(PPTESTCD, "AUCINT"),
                       NA,
                       convert_to_iso8601_duration(start, RRLTU)
                       ),
      PPENINT = ifelse(startsWith(PPTESTCD, "AUCINT"),
                       NA,
                       convert_to_iso8601_duration(end, RRLTU)
                       )
    )  %>%
    # Map PPTEST CDISC descriptions using PPTESTCD CDISC names
    # mutate(PPTEST = translate_terms(PPTESTCD, "PPTESTCD", "PPTEST")) %>%
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

  return(list(pp = pp, adpp = adpp, studyid = studyid))
}

find_common_prefix <- function(strings) {
  # Sort the vector of strings
  first_last_str <- sort(strings)[c(1, length(sorted_strings))]

  # Split the first and last elements into individual characters
  first_last_chars <- strsplit(sorted_strings, "")

  # Extract the two character vectors
  first_chars <- first_last_chars[[1]]
  last_chars <- first_last_chars[[2]]

  # Determine the length of the common prefix by comparing characters element-wise
  common_prefix_length <- 0
  for (i in seq_len(min(length(first_chars), length(last_chars)))) {
    if (first_chars[i] != last_chars[i]) {
      break
    }
    common_prefix_length <- i
  }

  # Return the common prefix if it exists, otherwise return an empty character vector
  if (common_prefix_length == 0) {
    return(character(0))
  } else {
    return(substr(sorted_strings[1], 1, common_prefix_length))
  }
}
