# Global Testing Data for aNCA Package
# ------------------------------------
# This file creates global testing datasets that can be used across all testthat tests.
# Developers can either:
# 1) Modify this file to add new cases as new lines in TEST_CONC_DATA & TEST_DOSE_DATA.
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

# Create Testing Concentration Data
TEST_CONC_DATA <- data.frame(
  # Columns that are mapped from the data
  AVAL = c(                 # USUBJID.DOSNO
    0:4,                    # 1.1 (Extravascular, linear & sample at dose)
    c(1, 2, 1.5, 1, 0.5),   # 2.1 (Extravascular eq, with max)
    c(1, 2, 1.5, 1, 0.5),   # 2.2 (Extravascular eq, 2nd dose)
    5:1,                    # 3.1 (IV bolus)
    5:1,                    # 3.2 (IV bolus, 2nd dose)
    rep(2, 5),              # 4.1 (IV bolus, abnormal profile)
    c(1, 2.5, 1, 0.7, 0.5), # 5.1 (Infusion)
    5:1,                    # 6.1 (IV bolus, metabolite)
    c(1:3, NA, 0.5)         # 7.1 (Extravascular, with NA)
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
    seq(0.5, 4.5, 1)       # 7.1
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
    seq(0.5, 4.5, 1)       # 7.1
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
    seq(0.5, 4.5, 1)       # 7.1
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
    seq(0.5, 4.5, 1)       # 7.1
  ),
  ROUTE = c(
    rep("extravascular", 5 * 3), # 1.1 - 2.2
    rep("intravascular", 5 * 6)  # 3.1 - 7.1
  ),
  PARAM = c(
    rep("A", 5 * 7),          # 1.1 - 5.1
    rep("B", 5 * 1),          # 6.1 (IV bolus, metabolite)
    rep("A", 5 * 1)           # 7.1 (Extravascular, with NA)
  ),
  USUBJID = c(
    rep(1, 5),
    rep(2, 5 * 2),
    rep(3, 5 * 2),
    rep(4, 5),
    rep(5, 5),
    rep(6, 5),
    rep(7, 5)
  ),
  DOSNO = c(
    rep(1, 5),
    rep(1:2, each = 5),
    rep(1:2, each = 5),
    rep(1, 5),
    rep(1, 5),
    rep(1, 5),
    rep(1, 5)
  ),
  # Included by aNCA internally
  is.excluded.hl = FALSE,
  is.included.hl = FALSE,
  exclude_half.life = FALSE,
  # Units
  AVALU = "mg/L",
  RRLTU = "h"
)

# Create Testing Dose Data
TEST_DOSE_DATA <- data.frame(
  AFRLT = c(
    0,
    c(0, 3),
    c(0, 3),
    0,
    0,
    0,
    0
  ),
  ARRLT = c(
    0,
    c(0, 0),
    c(0, 0),
    0,
    0,
    0,
    0
  ),
  NFRLT = c(
    0,
    c(0, 3),
    c(0, 3),
    0,
    0,
    0,
    0
  ),
  NRRLT = c(
    0,
    c(0, 0),
    c(0, 0),
    0,
    0,
    0,
    0
  ),
  ROUTE = c(
    rep("extravascular", 3),
    rep("intravascular", 5),
    "extravascular"
  ),
  ADOSE = 1,
  DRUG = "A",
  ADOSEDUR = c(
    rep(0, 6),
    1,            # 5.1 (Infusion)
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
    7
  ),
  DOSNO = c(
    1,
    c(1, 2),
    c(1, 2),
    1,
    1,
    1,
    1
  )
)

# Perform NCA Analysis
all_params <- setdiff(names(PKNCA::get.interval.cols()),
                      c("start", "end"))
main_intervals <- data.frame(
  start = c(0, 5),
  end = c(5, 10),
  type_interval = "main",  # Assumption 2: Include type_interval column
  DOSNO = c(1, 2)
) %>%
  left_join(
    TEST_DOSE_DATA %>%
      select(USUBJID, DOSNO) %>%
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
  start = c(0, 2),
  end = c(2, 4),
  half.life = FALSE,
  cmax = FALSE,
  aucint.last = TRUE,
  type_interval = "manual",  # Assumption 2: Include type_interval column
  DOSNO = c(1, 2)
) %>%
  left_join(
    TEST_DOSE_DATA %>%
      select(USUBJID, DOSNO) %>%
      unique()
  )
auc_intervals[, all_params] <- FALSE
auc_intervals <- auc_intervals %>%
  mutate(
    aucint.last = TRUE
  )
INTERVALS <- rbind(main_intervals, auc_intervals) %>%
  mutate(impute = case_when(
    USUBJID == 1 & DOSNO == 1 ~ NA_character_,
    USUBJID == 2 & DOSNO == 1 ~ "start_conc0",
    USUBJID == 2 & DOSNO == 2 ~ "start_predose",
    USUBJID == 3 & DOSNO == 1 ~ "start_logslope",
    USUBJID == 3 & DOSNO == 2 ~ "start_logslope",
    USUBJID == 4 & DOSNO == 1 ~ "start_c1",
    USUBJID == 4 & DOSNO == 2 ~ "start_c1",
    USUBJID == 5 & DOSNO == 1 ~ "start_conc0",
    USUBJID == 5 & DOSNO == 2 ~ "start_conc0",
    USUBJID == 6 & DOSNO == 1 ~ "start_conc0",
    USUBJID == 6 & DOSNO == 2 ~ "start_conc0",
    USUBJID == 7 & DOSNO == 1 ~ "start_conc0",
    USUBJID == 7 & DOSNO == 2 ~ "start_conc0",
    TRUE ~ NA_character_
  ))

TEST_PKNCA_DATA <- PKNCA::PKNCAdata(
  data.conc = PKNCA::PKNCAconc(TEST_CONC_DATA, AVAL ~ AFRLT | USUBJID / PARAM),
  data.dose = PKNCA::PKNCAdose(TEST_DOSE_DATA, ADOSE ~ AFRLT | USUBJID,
                               route = "ROUTE", duration = "ADOSEDUR",
                               time.nominal = "NFRLT"),
  units = PKNCA::pknca_units_table(
    concu = "ng/mL", doseu = "mg/kg", amountu = "mg", timeu = "hr"
  )
)
TEST_PKNCA_DATA$intervals <- INTERVALS
TEST_PKNCA_DATA$options <- list(keep_interval_cols = c("DOSNO", "type_interval"))

# Add start_dose and end_dose columns
TEST_PKNCA_RES <- withCallingHandlers(
  PKNCA::pk.nca(TEST_PKNCA_DATA),
  warning = function(w) {
    # Suppress warnings matching the regex "Too few points for half-life"
    if (grepl("^Too few points for half-life", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  }
)

#####################################################################
# Temporarily for some odd reason we cannot use keep_interval_cols,
# so we are manually making it
#
# TODO: Substitute this dirty hard coded trick with the proper way
TEST_PKNCA_RES$result <- TEST_PKNCA_RES$result %>%
  mutate(
    type_interval = if ("type_interval" %in% names(.)) {
      type_interval
    } else {
      ifelse(end %in% c(2, 4), "manual", "main")
    },
    DOSNO = if ("type_interval" %in% names(.)) {
      DOSNO
    } else {
      ifelse(
        end %in% c(2, 5),
        1,
        2
      )
    }
  )
#####################################################################

TEST_DOSE_DATA_TO_JOIN <- select(
  TEST_PKNCA_RES$data$dose$data,
  -exclude,
  -TEST_PKNCA_RES$data$dose$data$conc$columns$groups$group_analyte
)

TEST_PKNCA_RES$result <- TEST_PKNCA_RES$result %>%
  inner_join(
    TEST_DOSE_DATA_TO_JOIN,
    by = intersect(names(.), names(TEST_DOSE_DATA_TO_JOIN))
  ) %>%
  mutate(
    # Assumption 4: start_dose &  end_dose relative to the dose time
    start_dose = start - !!sym(TEST_PKNCA_RES$data$dose$columns$time),
    end_dose = end - !!sym(TEST_PKNCA_RES$data$dose$columns$time),
    # Assumption 3: PPSTRESU & PPSTRES are always in the results object
    PPSTRESU = ifelse(PPORRESU %in% c("fraction", "unitless"), "", PPORRESU),
    PPSTRES = PPORRES,
    # Assumption 5: PPTESTCD column folllows CDISC format
    PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD")
  )


# Dummy data
# Import dataset from testthat/data folder

DUMMY_DATA_FIXTURE <- read.csv(testthat::test_path("data", "adnca_dummy_sm_dataset.csv"))

# Create PKNCAdata object
PKNCA_DATA_FIXTURE <- PKNCA_create_data_object(DUMMY_DATA_FIXTURE %>% filter(PCSPEC == "Plasma"))
# Set intervals
PKNCA_DATA_FIXTURE$intervals <- format_pkncadata_intervals(
  PKNCA_DATA_FIXTURE$conc, PKNCA_DATA_FIXTURE$dose,
  params = c("aucinf.obs", "aucint.last", "auclast",
             "cmax", "half.life", "tmax",
             "lambda.z", "lambda.z.n.points",
             "r.squared", "adj.r.squared", "lambda.z.time.first")
)
PKNCA_DATA_FIXTURE <- create_start_impute(PKNCA_DATA_FIXTURE)

# Create NCA results
PKNCA_RESULTS_FIXTURE <- PKNCA_calculate_nca(PKNCA_DATA_FIXTURE)

