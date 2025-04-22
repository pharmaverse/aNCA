TEST_CONC_DATA <- data.frame(
  # Columns that are mapped from the data
  AVAL = c( # USUBJID/DOSNO
    0:4,                    # 1/1 (Extravascular, linear & sample at dose)
    c(1, 2, 1.5, 1, 0.5),   # 2/1 (Extravascular eq, with max)
    c(1, 2, 1.5, 1, 0.5),   # 2/2 (Extravascular eq, 2nd dose)
    5:1,                    # 3/1 (IV bolus)
    5:1,                    # 3/2 (IV bolus, 2nd dose)
    rep(2, 5),              # 4/1 (IV bolus, abnormal profile)
    c(1, 2.5, 1, 0.7, 0.5), # 5/1 (Infusion)
    5:1,                    # 6/1 (IV bolus, metabolite)
    c(1:3, NA, 0.5)         # 7/1 (Extravascular, with NA)
  ),
  AFRLT = c(
    0:4,                   # 1/1
    seq(0.5, 4.5, 1),      # 2/1
    seq(5.5, 9.5, 1),      # 2/2
    seq(0.5, 4.5, 1),      # 3/1
    seq(5.5, 9.5, 1),      # 3/2
    seq(0.5, 4.5, 1),      # 4/1
    seq(0.5, 4.5, 1),      # 5/1
    seq(0.5, 4.5, 1),      # 6/1
    seq(0.5, 4.5, 1)       # 7/1
  ),
  ARRLT = c(
    0:4,                   # 1/1
    seq(0.5, 4.5, 1),      # 2/1
    seq(0.5, 4.5, 1),      # 2/2
    seq(0.5, 4.5, 1),      # 3/1
    seq(0.5, 4.5, 1),      # 3/2
    seq(0.5, 4.5, 1),      # 4/1
    seq(0.5, 4.5, 1),      # 5/1
    seq(0.5, 4.5, 1),      # 6/1
    seq(0.5, 4.5, 1)       # 7/1
  ),
  NFRLT = c(
    0:4,                   # 1/1
    seq(0.5, 4.5, 1),      # 2/1
    seq(5.5, 9.5, 1),      # 2/2
    seq(0.5, 4.5, 1),      # 3/1
    seq(5.5, 9.5, 1),      # 3/2
    seq(0.5, 4.5, 1),      # 4/1
    seq(0.5, 4.5, 1),      # 5/1
    seq(0.5, 4.5, 1),      # 6/1
    seq(0.5, 4.5, 1)       # 7/1
  ),
  NRRLT = c(
    0:4,                   # 1/1
    seq(0.5, 4.5, 1),      # 2/1
    seq(0.5, 4.5, 1),      # 2/2
    seq(0.5, 4.5, 1),      # 3/1
    seq(0.5, 4.5, 1),      # 3/2
    seq(0.5, 4.5, 1),      # 4/1
    seq(0.5, 4.5, 1),      # 5/1
    seq(0.5, 4.5, 1),      # 6/1
    seq(0.5, 4.5, 1)       # 7/1
  ),
  ROUTE = c(
    rep("extravascular", 5*3), # 1/1 - 2/2
    rep("intravascular", 5*6) # 3/1 - 7/1
  ),
  PARAM = c(
    rep("A", 5*7),          # 1/1 - 5/1	
    rep("B", 5*1),          # 6/1 (IV bolus, metabolite)
    rep("A", 5*1)           # 7/1 (Extravascular, with NA)
  ),
  USUBJID = c(
    rep(1, 5),
    rep(2, 5*2),
    rep(3, 5*2),
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
  exclude_half.life = FALSE
)

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
    1,            # 5/1 (Infussion)
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

# Perform NCA analysis
main_intervals <- data.frame(
  start = c(0, 5),
  end = c(5, 10),
  half.life = TRUE,
  cmax = TRUE,
  aucint.last = FALSE,
  type_interval = "main",
  DOSNO = c(1, 2)
) %>%
  left_join(
    TEST_DOSE_DATA %>%
      select(USUBJID, DOSNO) %>%
      unique()
  )
auc_intervals <- data.frame(
  start = c(0, 2),
  end = c(2, 4),
  half.life = FALSE,
  cmax = FALSE,
  aucint.last = TRUE,
  type_interval = "manual",
  DOSNO = c(1, 2)
) %>%
  left_join(
    TEST_DOSE_DATA %>%
      select(USUBJID, DOSNO) %>%
      unique()
  )
options <- PKNCA::PKNCA.options()
options$keep_interval_cols <- c("DOSNO", "type_interval")

 TEST_PKNCA_DATA <- PKNCA::PKNCAdata(
    data.conc = PKNCA::PKNCAconc(TEST_CONC_DATA, AVAL ~ AFRLT | USUBJID / PARAM),
    data.dose = PKNCA::PKNCAdose(TEST_DOSE_DATA, ADOSE ~ AFRLT | USUBJID),
    intervals = rbind(
      main_intervals,
      auc_intervals
    ),
    options = options,
    units = PKNCA::pknca_units_table(
      concu = "ng/mL", doseu = "mg/kg", amountu = "mg", timeu = "hr"
    )
  )

TEST_PKNCA_RES <- PKNCA::pk.nca(TEST_PKNCA_DATA)


# Create PKNCA results object from 0 with sample concentration and dose datasets
# Additional conditions to PKNCA assumptions need to be made:
# 1) CDISC denomination of actual and nominal time variables (AFRLT, ARRLT, NFRLT, NRRLT)
# 2) For the intervals create a column (type_interval) that differentiates between
# custom AUC ranges ("manual") and main parameter calculations ("main")
# 3) There are PPSTRES and PPSTRESU variables in the PKNCA results output
# 4) start_dose & end_dose columns expressing when the actual start and actual end
# of the dose happened. The time reference is the first dose given to the subject.
# 5) CDISC denomination of PK parameters (needed temporarily to derive LAMZNPT & LAMZMTD)
TEST_DOSE_DATA_to_join <- select(
  TEST_PKNCA_RES$data$dose$data,
  -exclude,
  -TEST_PKNCA_RES$data$dose$data$conc$columns$groups$group_analyte
)
TEST_PKNCA_RES$result <- TEST_PKNCA_RES$result %>%
  # Function assumes dose time information is added to PKNCA results
  inner_join(
    TEST_DOSE_DATA_to_join,
    by = intersect(names(.), names(TEST_DOSE_DATA_to_join))
  ) %>%
  # Function assumes start_dose, end_dose, PPSTRES, PPSTRESU
  mutate(
    start_dose = start - !!sym(TEST_PKNCA_RES$data$dose$columns$time),
    end_dose = end - !!sym(TEST_PKNCA_RES$data$dose$columns$time),
    PPSTRESU = ifelse(PPORRESU %in% c("fraction", "unitless"), "", PPORRESU),
    PPSTRES = PPORRES,
    # Function assumes PPTESTCD is following CDISC standards
    PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD")
  )
