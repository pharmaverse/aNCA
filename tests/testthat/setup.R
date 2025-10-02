# Global Testing Data for aNCA Package
# ------------------------------------
# This file creates global testing datasets that can be used across all testthat tests.
# Developers can either:
# 1) Modify this file to add new cases as new lines in FIXTURE_CONC_DATA & TEST_DOSE_DATA.
# 2) Modify the testing file directly there, to derive the PKNCA objects for particular cases.
#
# The App currently performs certain modification on the PKNCA objects, numbered here:
# Assumptions and Modifications:
# 1) CDISC denomination of actual and nominal time variables (AFRLT, ARRLT, NFRLT, NRRLT).
#    - Relevant for functions that rely on time variables (e.g., interval calculations).
# 2) Intervals include a column (`type_interval`) differentiating custom AUC ("manual")
#    and main parameter calculations ("main").
#    - Relevant for interval-related functions.
# 3) PKNCA results include `PPSTRES` and `PPSTRESU` variables.
#    - Ensures compatibility with CDISC standards.
# 4) `start_dose` and `end_dose` columns express the actual start and end times of the dose,
#    relative to the first dose given to the subject.
#    - Used for dose interval calculations.
# 5) CDISC denomination of PK parameters (e.g., LAMZNPT, LAMZLL, LAMZ).
#    - Temporarily needed to derive LAMZNPT and LAMZMTD.

base::local({
  # Create Testing Concentration Data
  FIXTURE_CONC_DATA <<- data.frame(
    # Columns that are mapped from the data
    AVAL = c(                 # USUBJID.NCA_PROFILE
      0:4,                    # 1.1 (Extravascular, linear & sample at dose)
      c(1, 2, 1.5, 1, 0.5),   # 2.1 (Extravascular eq, with max)
      c(1, 2, 1.5, 1, 0.5),   # 2.2 (Extravascular eq, 2nd dose)
      5:1,                    # 3.1 (IV bolus)
      5:1,                    # 3.2 (IV bolus, 2nd dose)
      rep(2, 5),              # 4.1 (IV bolus, abnormal profile)
      c(1, 2.5, 1, 0.7, 0.5), # 5.1 (Infusion)
      5:1,                    # 6.1 (IV bolus, metabolite)
      c(1:3, NA, 0.5),        # 7.1 (Extravascular, with NA)
      c(1:3, NA, 0.5),        # 8.1 (A)
      c(0:2, NA, 0.5),        # 8.1 (B)
      c(5:3, NA, 0.5),        # 8.2 (A)
      c(4:2, NA, 0.5)         # 8.2 (B)
    ),
    AFRLT = c(                # Assumption 1: CDISC time variables
      0:4,                   # 1.1
      seq(0.5, 4.5, 1),      # 2.1
      seq(5.5, 9.5, 1),      # 2.2
      seq(0.5, 4.5, 1),      # 3.1
      seq(5.5, 9.5, 1),      # 3.2
      seq(0.5, 4.5, 1),      # 4.1
      seq(0.5, 4.5, 1),      # 5.1
      seq(0.5, 4.5, 1),      # 6.1
      seq(0.5, 4.5, 1),      # 7.1

      seq(0.5, 4.5, 1),        # 8.1 (A)
      seq(0.5, 4.5, 1),        # 8.1 (B)
      seq(5.5, 9.5, 1),        # 8.2 (A)
      seq(5.5, 9.5, 1)         # 8.2 (B)
    ),
    ARRLT = c(                # Assumption 1: CDISC time variables
      0:4,                   # 1.1
      seq(0.5, 4.5, 1),      # 2.1
      seq(0.5, 4.5, 1),      # 2.2
      seq(0.5, 4.5, 1),      # 3.1
      seq(0.5, 4.5, 1),      # 3.2
      seq(0.5, 4.5, 1),      # 4.1
      seq(0.5, 4.5, 1),      # 5.1
      seq(0.5, 4.5, 1),      # 6.1
      seq(0.5, 4.5, 1),      # 7.1

      seq(0.5, 4.5, 1),        # 8.1 (A)
      seq(0.5, 4.5, 1),        # 8.1 (B)
      seq(0.5, 4.5, 1),        # 8.2 (A)
      seq(0.5, 4.5, 1)         # 8.2 (B)
    ),
    NFRLT = c(                # Assumption 1: CDISC time variables
      0:4,                   # 1.1
      seq(0.5, 4.5, 1),      # 2.1
      seq(5.5, 9.5, 1),      # 2.2
      seq(0.5, 4.5, 1),      # 3.1
      seq(5.5, 9.5, 1),      # 3.2
      seq(0.5, 4.5, 1),      # 4.1
      seq(0.5, 4.5, 1),      # 5.1
      seq(0.5, 4.5, 1),      # 6.1
      seq(0.5, 4.5, 1),      # 7.1

      seq(0.5, 4.5, 1),      # 8.1 (A)
      seq(0.5, 4.5, 1),      # 8.1 (B)
      seq(5.5, 9.5, 1),      # 8.2 (A)
      seq(5.5, 9.5, 1)       # 8.2 (B)
    ),
    NRRLT = c(                # Assumption 1: CDISC time variables
      0:4,                   # 1.1
      seq(0.5, 4.5, 1),      # 2.1
      seq(0.5, 4.5, 1),      # 2.2
      seq(0.5, 4.5, 1),      # 3.1
      seq(0.5, 4.5, 1),      # 3.2
      seq(0.5, 4.5, 1),      # 4.1
      seq(0.5, 4.5, 1),      # 5.1
      seq(0.5, 4.5, 1),      # 6.1
      seq(0.5, 4.5, 1),      # 7.1
      seq(0.5, 4.5, 1),      # 8.1 (A)
      seq(0.5, 4.5, 1),      # 8.1 (B)
      seq(0.5, 4.5, 1),      # 8.2 (A)
      seq(0.5, 4.5, 1)       # 8.2 (B)
    ),
    ROUTE = c(
      rep("extravascular", 5 * 3), # 1.1 - 2.2
      rep("intravascular", 5 * 6),  # 3.1 - 7.1
      rep("extravascular", 5), # 8.1 (A)
      rep("extravascular", 5), # 8.1 (B)
      rep("intravascular", 5), # 8.2 (A)
      rep("intravascular", 5)  # 8.2 (B)
    ),
    PARAM = c(
      rep("A", 5 * 7),          # 1.1 - 5.1
      rep("B", 5 * 1),          # 6.1 (IV bolus, metabolite)
      rep("A", 5 * 1),          # 7.1 (Extravascular, with NA)
      rep("A", 5),              # 8.1 (A)
      rep("B", 5),              # 8.1 (B)
      rep("A", 5),              # 8.2 (A)
      rep("B", 5)               # 8.2 (B)
    ),
    PCSPEC = "SERUM",
    USUBJID = c(
      rep(1, 5),
      rep(2, 5 * 2),
      rep(3, 5 * 2),
      rep(4, 5),
      rep(5, 5),
      rep(6, 5),
      rep(7, 5),
      rep(8, 20)                # 8.1 (A,B), 8.2 (A,B)
    ),
    NCA_PROFILE = c(
      rep(1, 5),
      rep(1:2, each = 5),
      rep(1:2, each = 5),
      rep(1, 5),
      rep(1, 5),
      rep(1, 5),
      rep(1, 5),
      rep(1, 10),               # 8.1 (A,B)
      rep(2, 10)                # 8.2 (A,B)
    ),
    # Included by aNCA internally
    is.excluded.hl = FALSE,
    is.included.hl = FALSE,
    exclude_half.life = FALSE,
    # Units
    AVALU = c(
      rep("ng/mL", 5 * 7),     # 1.1 - 5.1 (A)
      rep("ug/mL", 5),         # 6.1 (B)
      rep("ng/mL", 5),         # 7.1 (A)
      rep("ng/mL", 5),         # 8.1 (A)
      rep("ug/mL", 5),         # 8.1 (B)
      rep("ng/mL", 5),         # 8.2 (A)
      rep("ug/mL", 5)          # 8.2 (B)
    ),
    RRLTU = "hr",
    STUDYID = "S1"
  ) %>%
    # Needed for pivot_wider_pknca_results (dose_profile_duplicates)
    # TODO (Gerardo): Kill this assumption
    mutate(
      DOSNOA = NCA_PROFILE
    )

  # Create Testing Dose Data
  FIXTURE_DOSE_DATA <<- data.frame(
    AFRLT = c(
      0,
      c(0, 5),
      c(0, 5),
      0,
      0,
      0,
      0,
      0,
      5
    ),
    ARRLT = c(
      0,
      c(0, 0),
      c(0, 0),
      0,
      0,
      0,
      0,
      0,
      0
    ),
    NFRLT = c(
      0,
      c(0, 5),
      c(0, 5),
      0,
      0,
      0,
      0,
      0,
      5
    ),
    NRRLT = c(
      0,
      c(0, 0),
      c(0, 0),
      0,
      0,
      0,
      0,
      0,  # 8.1 (A,B)
      0   # 8.2 (A,B)
    ),
    ROUTE = c(
      rep("extravascular", 3), # 1.1, 2.1, 2.2
      rep("intravascular", 5), # 2.2 - 6.1
      "extravascular",  # 7.1 (A)
      "extravascular",  # 8.1 (A,B)
      "intravascular"   # 8.2 (A,B)
    ),
    DOSEA = c(
      1,
      c(1, 2),
      c(1, 2),
      1,
      1,
      1,
      1,
      10,             # 8.1 (A,B)
      5               # 8.2 (A,B)
    ),
    DRUG = "A",
    ADOSEDUR = c(
      rep(0, 6),
      1,            # 5.1 (Infusion)
      0,
      0,
      0,
      0
    ),
    USUBJID = c(
      1,
      rep(2, 2),
      rep(3, 2),
      4,
      5,
      6,
      7,
      rep(8, 2)
    ),
    NCA_PROFILE = c(
      1,
      c(1, 2),
      c(1, 2),
      1,
      1,
      1,
      1,
      c(1, 2)
    ),
    DOSNOA = c(
      1,
      c(1, 2),
      c(1, 2),
      1,
      1,
      1,
      1,
      c(1, 2)
    ),
    DOSEU = "mg/kg"
  )
  # Perform NCA Analysis
  all_params <- setdiff(names(PKNCA::get.interval.cols()),
                        c("start", "end"))
  main_intervals <- data.frame(
    start = c(0, 5),
    end = c(5, 10),
    type_interval = "main",  # Assumption 2: Include type_interval column
    NCA_PROFILE = c(1, 2)
  ) %>%
    left_join(
      FIXTURE_DOSE_DATA %>%
        select(USUBJID, NCA_PROFILE, DOSNOA) %>%
        unique()
    )
  main_intervals[, all_params] <- FALSE
  main_intervals <- main_intervals %>%
    mutate(
      half.life = TRUE,
      cmax = TRUE,
      auclast = TRUE,
      aucinf.obs = TRUE
    )
  auc_intervals <- data.frame(
    # Intervals for AUC_0-2, AUC_2-4 in each dose
    start = c(0, 2, 5, 7),
    end = c(2, 4, 7, 9),
    type_interval = "manual",  # Assumption 2: Include type_interval column
    NCA_PROFILE = c(1, 1, 2, 2)
  ) %>%
    left_join(
      FIXTURE_DOSE_DATA %>%
        select(USUBJID, NCA_PROFILE, DOSNOA) %>%
        unique(),
      by = "NCA_PROFILE",
      relationship = "many-to-many"
    )
  auc_intervals[, all_params] <- FALSE
  auc_intervals <- auc_intervals %>%
    mutate(
      aucint.last = TRUE
    )
  FIXTURE_INTERVALS <<- rbind(main_intervals, auc_intervals) %>%
    mutate(impute = case_when(
      USUBJID == 1 & NCA_PROFILE == 1 ~ NA_character_,
      USUBJID == 2 & NCA_PROFILE == 1 ~ "start_conc0",
      USUBJID == 2 & NCA_PROFILE == 2 ~ "start_predose",
      USUBJID == 3 & NCA_PROFILE == 1 ~ "start_logslope",
      USUBJID == 3 & NCA_PROFILE == 2 ~ "start_logslope",
      USUBJID == 4 & NCA_PROFILE == 1 ~ "start_c1",
      USUBJID == 4 & NCA_PROFILE == 2 ~ "start_c1",
      USUBJID == 5 & NCA_PROFILE == 1 ~ "start_conc0",
      USUBJID == 5 & NCA_PROFILE == 2 ~ "start_conc0",
      USUBJID == 6 & NCA_PROFILE == 1 ~ "start_conc0",
      USUBJID == 6 & NCA_PROFILE == 2 ~ "start_conc0",
      USUBJID == 7 & NCA_PROFILE == 1 ~ "start_conc0",
      USUBJID == 7 & NCA_PROFILE == 2 ~ "start_conc0",
      TRUE ~ NA_character_
    ))


  units_table <- rbind(
    PKNCA::pknca_units_table(
      concu = "ng/mL",
      timeu = "hr",
      doseu = "mg/kg"
    )  %>%  mutate(PARAM = "A"),
    PKNCA::pknca_units_table(
      concu = "ug/mL",
      timeu = "hr",
      doseu = "mg/kg"
    )  %>%  mutate(PARAM = "B")
  )

  FIXTURE_PKNCA_DATA <<- suppressWarnings({
    PKNCA::PKNCAdata(
      data.conc = PKNCA::PKNCAconc(FIXTURE_CONC_DATA, AVAL ~ AFRLT | PCSPEC + USUBJID / PARAM,
                                 concu = "AVALU", timeu = "RRLTU"),
      data.dose = PKNCA::PKNCAdose(FIXTURE_DOSE_DATA, DOSEA ~ AFRLT | USUBJID,
                                 route = "ROUTE", duration = "ADOSEDUR"),
      units = units_table
      )
  })
  
  FIXTURE_PKNCA_DATA$intervals <<- FIXTURE_INTERVALS

  FIXTURE_PKNCA_DATA$options <<- list(keep_interval_cols = c("NCA_PROFILE",
                                                             "DOSNOA",
                                                             "type_interval"))

  # Add start_dose and end_dose columns
  FIXTURE_PKNCA_RES <<- withCallingHandlers(
    PKNCA::pk.nca(FIXTURE_PKNCA_DATA),
    warning = function(w) {
      # Suppress warnings matching the regex "Too few points for half-life"
      if (grepl("^Too few points for half-life|^Requesting an AUC range starting",
                conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  #####################################################################
  # Temporarily for some odd reason we cannot use keep_interval_cols,
  # so we are manually making it
  #
  # TODO: Substitute this dirty hard coded trick with the proper way
  FIXTURE_PKNCA_RES$result <<- FIXTURE_PKNCA_RES$result %>%
    mutate(
      type_interval = if ("type_interval" %in% names(.)) {
        type_interval
      } else {
        ifelse((end - start) == 2, "manual", "main")
      },
      NCA_PROFILE = if ("NCA_PROFILE" %in% names(.)) {
        NCA_PROFILE
      } else {
        ifelse(
          start < 5,
          1,
          2
        )
      },
      DOSNOA = if ("DOSNOA" %in% names(.)) {
        DOSNOA
      } else {
        ifelse(
          start < 5,
          1,
          2
        )
      }
    )
  #####################################################################
  dose_data_to_join_fixture <- select(
    FIXTURE_PKNCA_RES$data$dose$data,
    -exclude,
    -FIXTURE_PKNCA_RES$data$dose$data$conc$columns$groups$group_analyte
  )
  FIXTURE_PKNCA_RES$result <<- FIXTURE_PKNCA_RES$result %>%
    inner_join(
      dose_data_to_join_fixture,
      by = intersect(names(.), names(dose_data_to_join_fixture))
    ) %>%
    mutate(
      # Assumption 4: start_dose &  end_dose relative to the dose time
      start_dose = start - !!sym(FIXTURE_PKNCA_RES$data$dose$columns$time),
      end_dose = end - !!sym(FIXTURE_PKNCA_RES$data$dose$columns$time),
      # Assumption 3: PPSTRESU & PPSTRES are always in the results object
      PPSTRESU = ifelse(PPORRESU %in% c("fraction", "unitless"), "", PPORRESU),
      PPSTRES = PPORRES,
      # Assumption 5: PPTESTCD column folllows CDISC format
      PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD")
    )
})
