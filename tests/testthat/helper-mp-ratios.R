# Ordinary NCA values plus the configured ratios exported in ADPP.
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
  parent$PPANMETH <- NA_character_
  metabolite <- parent
  metabolite$PPCAT <- "Metab-DrugA"
  metabolite$AVAL <- parent$AVAL * ifelse(
    parent$ATPTREF == "DOSE 1",
    ifelse(parent$USUBJID == "S1", 0.5, 0.3),
    ifelse(parent$USUBJID == "S1", 0.25, 0.75)
  )
  ratios <- metabolite
  ratios$PARAM <- paste("M/P", ratios$PARAM)
  ratios$PARAMCD <- paste0("RA", ratios$PARAMCD)
  ratios$AVAL <- ifelse(
    ratios$ATPTREF == "DOSE 1",
    ifelse(ratios$USUBJID == "S1", 0.5, 0.3),
    ifelse(ratios$USUBJID == "S1", 0.25, 0.75)
  )
  ratios$AVALU <- "fraction"
  ratios$PPANMETH <- paste0(
    parent$PARAMCD, " TO ", parent$PARAMCD, " [PARAM: DrugA]"
  )
  data <- rbind(parent, metabolite, ratios)
  data$PPSTRESN <- data$AVAL
  data$PPSTRESU <- data$AVALU
  data
}

# Distinct dose events can share the collection-reference label and treatment.
# A missing first-dose Cmax must stay missing rather than borrowing dose 2.
mp_same_label_fixture <- function(dose_numbers = TRUE) {
  data <- subset(mp_adpp_fixture(), !is.na(PPANMETH))
  dose <- match(data$ATPTREF, c("DOSE 1", "DOSE 2"))
  data$DOSNOA <- if (dose_numbers) dose else NULL
  data$DOSEA <- if (dose_numbers) 10 else c(10, 20)[dose]
  data$DOSEU <- "mg"
  data$ATPTREF <- "POST DOSE"
  data$TRT01A <- "DrugA"
  data$AVAL[data$USUBJID == "S1" & dose == 1 & data$PARAMCD == "RACMAX"] <- NA_real_
  data
}
