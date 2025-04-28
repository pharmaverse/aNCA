library(testthat)
library(dplyr)

# Simplified test_pknca_res object for testing with additional variables
test_pknca_res <- list(
  result = data.frame(
    USUBJID = rep(c("SUBJ001", "SUBJ002"), each = 3),
    DOSNO = c(1, 1, 1, 1, 1, 1),
    PPTESTCD = c("AUCINF", "AUCIFO", "R2", "AUCINF", "AUCINT", "R2"),
    PPORRES = c(100, 95, 0.98, 120, 115, 0.96),
    PPSTRES = c(100, 95, 0.98, 120, 115, 0.96),
    PPSTRESU = c("ng*h/mL", "ng*h/mL", "", "ng*h/mL", "ng*h/mL", ""),
    PPORRESU = c("ng*h/mL", "ng*h/mL", "", "ng*h/mL", "ng*h/mL", ""),
    exclude = c(NA, NA, NA, "Excluded due to protocol deviation", NA, NA),
    PARAM = c("Analyte1", "Analyte1", "Analyte1", "Analyte1", "Analyte1", "Analyte1"),
    PCSPEC = c("Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma"),
    STUDYID = c("STUDY001", "STUDY001", "STUDY001", "STUDY001", "STUDY001", "STUDY001"),
    start = c(0, 0, 0, 0, 2, 2),  # Start times for intervals
    end = c(5, 5, 5, 5, 4, 4)     # End times for intervals
  ),
  data = list(
    dose = list(
      data = data.frame(
        USUBJID = c("SUBJ001", "SUBJ002"),
        DOSNO = c(1, 1),
        ROUTE = c("Oral", "Oral"),
        RRLTU = c("h", "h"),
        DOSEA = c(100, 200),
        DOSEU = c("mg", "mg"),
        # Extra variables only added if present (PP / ADPP)
        SEX = c("M", "F"),
        RACE = c("White", "Black"),
        AGE = c(30, 25),
        AGEU = c("years", "years"),
        SITEID = c("Site1", "Site2"),
        ACTARM = c("Treatment A", "Treatment B"),
        TRT01P = c("Drug A", "Drug B"),
        TRT01A = c("Drug A", "Drug B")
      ),
      columns = list(
        groups = c("USUBJID", "DOSNO"),
        route = "ROUTE"
      )
    ),
    conc = list(
      data = data.frame(
        USUBJID = rep(c("SUBJ001", "SUBJ002"), each = 5),
        PARAM = rep("Analyte1", 10),
        AVAL = c(0, 10, 20, 30, 40, 0, 15, 25, 35, 45),
        AFRLT = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
        ARRLT = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
        NFRLT = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
        NRRLT = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
        PCSPEC = rep("Plasma", 10),
        AVALU = rep("ng/mL", 10),
        is.excluded.hl = rep(FALSE, 10),
        # Extra variables only added if present (ADPC)
        SEX = c("M", "F", "M", "F", "M", "M", "F", "M", "F", "M"),
        RACE = c("White", "Black", "White", "Black", "White", "White", "Black", "White", "Black", "White"),
        AGE = c(30, 25, 30, 25, 30, 30, 25, 30, 25, 30),
        AGEU = rep("years", 10),
        ACTARM = rep(c("Treatment A", "Treatment B"), each = 5),
        VISITNUM = rep(1:5, 2),
        VISIT = rep(c("Visit 1", "Visit 2", "Visit 3", "Visit 4", "Visit 5"), 2),
        AVISIT = rep(c("Visit 1", "Visit 2", "Visit 3", "Visit 4", "Visit 5"), 2),
        AVISITN = rep(1:5, 2),
        STUDYID = rep("STUDY001", 10),
        ATPT = rep(c("Pre-dose", "Post-dose"), each = 5),
        ATPTN = rep(c(0, 1), each = 5),
        ANL01FL = rep("Y", 10),
        SITEID = rep(c("Site1", "Site2"), each = 5),
        # These variables are only added if present but could be derived
        PCSTRESC = c("BLQ", "10", "20", "30", "40", "BLQ", "15", "25", "35", "45"),
        PCSTRESN = c(0, 10, 20, 30, 40, 0, 15, 25, 35, 45),
        PCSTRESU = rep("ng/mL", 10),
        PCORRES = c(0, 10, 20, 30, 40, 0, 15, 25, 35, 45),
        PCORRESU = rep("ng/mL", 10),
        PCTPT = rep(c("Pre-dose", "Post-dose"), each = 5),
        PCTPTNUM = rep(c(0, 1), each = 5)
      ),
      columns = list(
        groups = c("USUBJID", "PARAM")
      )
    )
  )
)

describe("export_cdisc", {
  it("exports CDISC-compliant datasets with multiple variables per subject", {
    result <- export_cdisc(test_pknca_res)

    # Check that the result contains the expected components
    expect_type(result, "list")
    expect_named(result, c("pp", "adpp", "adpc", "studyid"))

    # Check the pp dataset
    pp <- result$pp
    expect_s3_class(pp, "data.frame")
    expect_true(all(CDISC_COLS$PP %in% names(pp)))
    expect_equal(nrow(pp), 6) # Ensure all variables are included

    # Check the adpp dataset
    adpp <- result$adpp
    expect_s3_class(adpp, "data.frame")
    expect_true(all(CDISC_COLS$ADPP %in% names(adpp)))
    expect_equal(nrow(adpp), 6) # Ensure all variables are included

    # Check the adpc dataset
    adpc <- result$adpc
    expect_s3_class(adpc, "data.frame")
    expect_true(all(CDISC_COLS$ADPC %in% names(adpc)))
    expect_equal(nrow(adpc), 10) # Ensure all concentration data is included

    # Check the studyid
    expect_equal(result$studyid, "STUDY001")
  })
})
