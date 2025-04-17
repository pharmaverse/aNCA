conc_data <- data.frame(
  # Columns that are mapped from the data
  AVAL = c( # USUBJID / DOSNO
    0:4,               # 1 / 1 (Extravascular, linear & sample at dose)
    c(1:4, 0.5),       # 2 / 1 (Extravascular eq, with max)
    c(1:4, 0.5),       # 2 / 2 (Extravascular eq, 2nd dose)
    5:1,               # 3 / 1 (IV bolus)
    5:1,               # 3 / 2 (IV bolus, 2nd dose)
    rep(2, 5),         # 4 / 1 (IV bolus, abnormal profile)
    c(1:3, 1, 0.5),    # 5 / 1 (Infussion)
    5:1,               # 6 / 1 (IV bolus)
    c(1:3, NA, 0.5)      # 7 / 1 (Extravascular, with NA)
  ),
  AFRLT = c(
    0:4,               # 1 / 1
    seq(0.5, 4.5, 1),               # 2 / 1
    seq(5.5, 9.5, 1),              # 2 / 2
    seq(0.5, 4.5, 1),       # 3 / 1
    seq(5.5, 9.5, 1),              # 3 / 2
    seq(0.5, 4.5, 1),      # 4 / 1
    seq(0.5, 4.5, 1),      # 5 / 1
    seq(0.5, 4.5, 1),      # 6 / 1
    seq(0.5, 4.5, 1)                # 7 / 1
  ),
  ARRLT = c(
    0:4,               # 1 / 1
    seq(1, 4.5, 0.75), # 2 / 1
    1:3,               # 2 / 2
    c(1, 2, 2.5),      # 3 / 1
    c(1, 2, 2.5),      # 3 / 2
    c(1, 2, 2.5),      # 4 / 1
    c(1, 2, 2.5),      # 5 / 1
    c(1, 2, 2.5),      # 6 / 1
    0:4                # 7 / 1
  ),
  NFRLT = c(
    0:4, c(1, 2, 2.5), # 1 / 1
    4:6, c(1, 2, 2.5), # 2 / 1
    4:6, c(1, 2, 2.5), # 2 / 2
    c(1, 2, 2.5),      # 3 / 1
    c(1, 2, 2.5),      # 3 / 2
    c(1, 2, 2.5),      # 4 / 1
    c(1, 2, 2.5),      # 5 / 1
    c(1, 2, 2.5),      # 6 / 1
    0:4                # 7 / 1
  ),
  NRRLT = c(
    0:4, # 1 / 1
    1:3, c(1, 2, 2.5), # 2 / 1
    1:3, c(1, 2, 2.5), # 2 / 2
    c(1, 2, 2.5),      # 3 / 1
    c(1, 2, 2.5),      # 3 / 2
    c(1, 2, 2.5),      # 4 / 1
    c(1, 2, 2.5),      # 5 / 1
    c(1, 2, 2.5),      # 6 / 1
    0:4                # 7 / 1
  ),
  ROUTE = c(
    rep("extravascular", 9), 
    rep("intravascular", 15)
  ),
  PARAM = c(
    rep("A", 21), 
    rep("B", 3)
  ),
  USUBJID = c(
    rep(1, 3),
    rep(2, 6),
    rep(3, 6),
    rep(4, 3),
    rep(5, 3),
    rep(6, 3)
  ),
  DOSNO = c(
    rep(1, 3), 
    rep(rep(1:2, each = 3), 2), 
    rep(1, 3), 
    rep(1, 3), 
    rep(1, 3)
  ),
  # Included by aNCA internally
  is.excluded.hl = FALSE,
  is.included.hl = FALSE,
  exclude_half.life = FALSE
)

dose_data <- data.frame(
  AFRLT = c(
    0, 0, 3, 0, 3, 0, 0, 0
  ),
  ARRLT = c(
    0, 0, 0, 0, 0, 0, 0, 0
  ),
  NFRLT = c(
    0, 0, 3, 0, 3, 0, 0, 0
  ),
  NRRLT = c(
    0, 0, 0, 0, 0, 0, 0, 0
  ),
  ROUTE = c(
    rep("extravascular", 3), 
    rep("intravascular", 5)
  ),
  ADOSE = 1,
  DRUG = "A",
  ADOSEDUR = c(
    0, 0, 0, 0, 0, 0, 1, 0
  ),
  USUBJID = c(
    1, rep(2, 2), 
    rep(3, 2), 
    4, 5, 6
  ),
  DOSNO = c(
    1, c(1, 2), 
    c(1, 2), 
    1, 1, 1
  )
)

# Perform NCA analysis
main_intervals <- data.frame(
  start = c(0, 3),
  end = c(3, 6),
  half.life = TRUE,
  cmax = TRUE,
  aucint.last = FALSE,
  type_interval = "main",
  DOSNO = c(1, 2)
) %>%
  left_join(
    dose_data %>%
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
    dose_data %>%
      select(USUBJID, DOSNO) %>%
      unique()
  )
options <- PKNCA::PKNCA.options()
options$keep_interval_cols <- c("DOSNO", "type_interval")

myres <- PKNCA::pk.nca(
  PKNCA::PKNCAdata(
    data.conc = PKNCA::PKNCAconc(conc_data, AVAL ~ AFRLT | USUBJID / PARAM),
    data.dose = PKNCA::PKNCAdose(dose_data, ADOSE ~ AFRLT | USUBJID),
    intervals = rbind(
      main_intervals,
      auc_intervals
    ),
    options = options,
    units = PKNCA::pknca_units_table(
      concu = "ng/mL", doseu = "mg/kg", amountu = "mg", timeu = "hr"
    )
  )
)
