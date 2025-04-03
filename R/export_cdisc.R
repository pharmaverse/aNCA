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

  # define columns needed for pp
  pp_col <- c(
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
  adpp_col <- c("STUDYID",
                "DOMAIN",
                "USUBJID",
                "PPSEQ",
                "PPCAT",
                "PPGRPID",
                "PPSPID",
                "PPTESTCD",
                "PPTEST",
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
    filter(is.infinite(end) | PPTESTCD == "auclast") %>%
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
      DOMAIN = "PP",
      # Group ID
      PPGRPID =  paste(PARAM, PCSPEC, paste("CYCLE", DOSNO,  sep = " "), sep = "-"),
      # Parameter Category
      PPCAT = PARAM,
      PPSCAT = "NON-COMPARTMENTAL",
      PPDOSNO = DOSNO,
      PPSPEC = PCSPEC,
      # Specific ID variables
      PPSPID = "TBD",
      # TODO Results in Standard Units if ORRESU is not in standard units
      PPSTRESN = as.numeric(PPSTRES),
      PPSTRESC = as.character(PPSTRES),
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
        if ("PCRFTDM" %in% names(.)) {
          strptime(PCRFTDTM, format = "%Y-%m-%d %H:%M:%S") %>% format("%Y-%m-%dT%H:%M")
        } else {
          NA
        }
      },
      # Matrix
      PPSPEC = PCSPEC,
      # TODO start and end intervals in case of partial aucs -> see oak file in templates
      PPSTINT = ifelse(start != Inf, start, NA),
      PPENINT = ifelse(end != Inf, end, NA)
    )  %>%
    # Map PPTEST CDISC descriptions using PPTESTCD CDISC names
    mutate(PPTEST = translate_terms(PPTESTCD, "PPTESTCD", "PPTEST")) %>%
    group_by(USUBJID)  %>%
    mutate(PPSEQ = if ("PCSEQ" %in% names(.)) PCSEQ else row_number())  %>%
    ungroup()

  # select pp columns
  pp <- pp_info %>%  select(all_of(pp_col))

  adpp <- pp_info %>%
    # Elude potential collapse cases with PC variables
    select(-any_of(c("AVAL", "AVALC", "AVALU"))) %>%
    rename(AVAL = PPSTRESN, AVALC = PPSTRESC, AVALU = PPSTRESU)  %>%
    left_join(
      res_nca$data$dose$data %>%
        select(-any_of(setdiff(names(pp_info), "USUBJID"))),
      by = "USUBJID"
    ) %>%
    select(any_of(adpp_col))

  return(list(pp = pp, adpp = adpp))
}
