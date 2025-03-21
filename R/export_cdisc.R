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
  if (FALSE) {
    # uncomment in case of more added variables
    # load metadata
    pptestcd <- read.csv("data/pptestcd.csv") %>%
      # add descriptions not available from oak metadata
      bind_rows(
        data.frame(
          PPTESTCD = c("CLSTP", "LAMZSPNR"),
          STD_PPTEST = c(
            "Last Non Zero Concentration Predicted",
            "Ratio of Half-Life to Time used for Half-Life Calculation"
          )
        )
      )
  }


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
      PPTESTCD = recode(
        PPTESTCD %>% toupper,
        "AUCLAST" = "AUCLST",
        "TLAST" = "TLST",
        "CLAST.OBS" = "CLST",
        "LAMBDA.Z" = "LAMZ",
        "R.SQUARED" = "R2",
        "ADJ.R.SQUARED" = "R2ADJ",
        # This one does not exist right? I don't see its parameter use
        "LAMBDA.Z.TIME.FIRST" = "LAMZLL",
        "LAMBDA.Z.N.POINTS" = "LAMZNPT",        # The same with this one
        "CLAST.PRED" = "CLSTP",
        "HALF.LIFE" = "LAMZHL",
        "SPAN.RATIO" = "LAMZSPNR",              # Is this code name correct/standard?
        "AUCINF.OBS" = "AUCIFO",
        "AUCINF.PRED" = "AUCIFP",
        "AUCPEXT.OBS" = "AUCPEO",
        "AUCPEXT.PRED" = "AUCPEP",
        "TMAX" = "TMAX",
        "CMAX" = "CMAX"
      ),
      DOMAIN = "PP",
      # Group ID
      PPGRPID =  paste(ANALYTE, PCSPEC, paste("CYCLE", DOSNO,  sep = " "), sep = "-"),
      # Parameter Cathegory
      PPCAT = if ("PARAM" %in% names(.)) PARAM else ANALYTE,
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
    mutate(PPTEST = .pptestcd_dict[PPTESTCD])  %>%
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

.pptestcd_dict <- setNames(
  c(
    "Total CL Obs by F", "Time of Last Nonzero Conc", "Max Conc", "Vz Obs by F", "AUC Infinity Obs",
    "Last Nonzero Conc", "Time of CMAX", "R Squared", "R Squared Adjusted", "Max Conc Norm by Dose",
    "AUC to Last Nonzero Conc Norm by Dose", "Lambda z", "AUC to Last Nonzero Conc",
    "Half-Life Lambda z", "Number of points used for Lambda z", "Last Nonzero Conc Predicted",
    "Span Ratio", "Lambda z lower limit (time)",

    # Manually filled
    "Trough Concentration", "Average Concentration", "AUC Infinity Predicted",
    "AUMC Infinity Observed", "AUC Percent Extrapolated Observed",
    "AUC Percent Extrapolated Predicted", "Clearance Observed",
    "Clearance Predicted", "Mean Residence Time Intravenous Observed",
    "Volume of Distribution Observed",
    "Steady-State Volume of Distribution Intravenous Observed",
    "AUC Infinity Observed Dose-Normalized", "Maximum Concentration Dose-Normalized"
  ),
  c("CLFO", "TLST", "CMAX", "VZFO", "AUCIFO", "CLST", "TMAX", "R2", "R2ADJ", "CMAXD", "AUCLSTD",
    "LAMZ", "AUCLST", "LAMZHL", "LAMZNPT", "CLSTP", "LAMZSPNR", "LAMZLL",
    "CTROUGH", "CAV", "AUCIFP", "AUMCINF.OBS", "AUCPEO", "AUCPEP", "CL.OBS", "CL.PRED",
    "MRT.IV.OBS", "VZ.OBS", "VSS.IV.OBS", "AUCINF.OBS.DN", "CMAX.DN"
  )
)