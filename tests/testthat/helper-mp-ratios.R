# Paired ordinary NCA values, with no manually configured ratio rows.
mp_adpp_fixture <- function() {
  parent <- expand.grid(
    USUBJID = c("S1", "S2"), PARAMCD = c("CMAX", "AUCLST"),
    ATPTREF = c("DOSE 1", "DOSE 2"), stringsAsFactors = FALSE
  )
  parent$STUDYID <- "STUDY1"
  parent$TRT01A <- "10mg"
  parent$DOSETRT <- "DrugA"
  parent$PPCAT <- "DrugA"
  parent$PPSPEC <- "PLASMA"
  parent$PARAM <- ifelse(parent$PARAMCD == "CMAX", "Cmax", "AUClast")
  parent$AVAL <- ifelse(parent$USUBJID == "S1", 10, 30) *
    ifelse(parent$PARAMCD == "CMAX", 1, 10)
  parent$AVALU <- ifelse(parent$PARAMCD == "CMAX", "ng/mL", "ng*h/mL")
  parent$PPSUMXF <- ""
  metabolite <- parent
  metabolite$PPCAT <- "Metab-DrugA"
  metabolite$AVAL <- parent$AVAL * ifelse(
    parent$ATPTREF == "DOSE 1",
    ifelse(parent$USUBJID == "S1", 0.5, 0.3),
    ifelse(parent$USUBJID == "S1", 0.25, 0.75)
  )
  data <- rbind(parent, metabolite)
  data$PPSTRESN <- data$AVAL
  data$PPSTRESU <- data$AVALU
  data
}

mp_adnca_fixture <- function() {
  data.frame(
    STUDYID = "STUDY1", DOSETRT = "DrugA",
    PARAM = c("DrugA", "Metab-DrugA"), METABFL = c("", "Y")
  )
}
