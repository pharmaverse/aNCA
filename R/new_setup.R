conc_data <- data.frame(
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

dose_data <- data.frame(
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


# Make preparations done by PKNCA_calculate_nca
dose_data_to_join <- select(
  myres$data$dose$data,
  -exclude,
  -myres$data$dose$data$conc$columns$groups$group_analyte
)
myres$result <- myres$result %>%
  # Function assumes dose time information is added to PKNCA results
  inner_join(
    dose_data_to_join,
    by = intersect(names(.), names(dose_data_to_join))
  ) %>%
  # Function assumes start_dose, end_dose, PPSTRES, PPSTRESU
  mutate(
    start_dose = start - !!sym(myres$data$dose$columns$time),
    end_dose = end - !!sym(myres$data$dose$columns$time),
    PPSTRESU = ifelse(PPORRESU %in% c("fraction", "unitless"), "", PPORRESU),
    PPSTRES = PPORRES,
    # Function assumes PPTESTCD is following CDISC standards
    PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD")
  )

# Obtain the result
result <- pivot_wider_pknca_results(myres)