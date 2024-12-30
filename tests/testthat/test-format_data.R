library(testthat)
library(dplyr)
library(PKNCA)

test_that("create_conc generates correct dataset", {
  # Generate sample ADNCA data
  ADNCA <- data.frame(
    STUDYID = rep(1, 10),
    USUBJID = rep(1:2, each = 5),
    PCSPEC = rep("Plasma", 10),
    DRUG = rep("DrugA", 10),
    ANALYTE = rep("Analyte1", 10),
    AFRLT = seq(0, 9),
    AVAL = runif(10)
  )

  # Call create_conc
  df_conc <- create_conc(ADNCA,
                         group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "ANALYTE"),
                         time_column = "AFRLT")

  # Test if df_conc is a data frame
  expect_s3_class(df_conc, "data.frame")

  # Test if df_conc has the correct columns
  expect_true(all(c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "ANALYTE",
                    "AFRLT", "AVAL", "conc_groups", "TIME", "IX") %in% colnames(df_conc)))

  # Test if df_conc is correctly grouped and arranged
  expect_equal(df_conc$TIME, df_conc$AFRLT)
  expect_equal(df_conc$IX, rep(1:5, 2))

  # Test if df_conc can be used with PKNCAconc by testing its output
  expect_s3_class(
    PKNCAconc(
      df_conc,
      formula = AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / ANALYTE,
      exclude_half.life = "exclude_half.life",
      time.nominal = "NFRLT"
    ),
    "PKNCAconc"
  )
})

test_that("create_conc generates correct dataset with multiple doses", {
  # Generate sample ADNCA data with at least two doses per subject
  ADNCA <- data.frame(
    STUDYID = rep(1, 20),
    USUBJID = rep(1:2, each = 10),
    PCSPEC = rep("Plasma", 20),
    DRUG = rep("DrugA", 20),
    ANALYTE = rep("Analyte1", 20),
    AFRLT = rep(seq(0, 9), 2),
    ARRLT = c(rep(seq(0, 4), 2), rep(seq(0, 4), 2)),
    ROUTE = "intravascular",
    DOSNO = rep(rep(1:2, each = 5), 2),
    AVAL = runif(20)
  )

  # Call create_conc
  df_conc <- create_conc(ADNCA,
                         group_columns = c("STUDYID", "USUBJID", "PCSPEC",
                                           "DRUG", "ANALYTE", "DOSNO"),
                         time_column = "AFRLT")

  # Test if df_conc is a data frame
  expect_s3_class(df_conc, "data.frame")

  # Test if df_conc has the correct columns
  expect_true(all(c("STUDYID", "USUBJID", "PCSPEC",
                    "DRUG", "ANALYTE", "DOSNO",
                    "AFRLT", "AVAL", "conc_groups",
                    "TIME", "IX") %in% colnames(df_conc)))

  # Test if df_conc is correctly grouped and arranged
  expect_equal(df_conc$TIME, df_conc$AFRLT)
  expect_equal(df_conc %>%
                 dplyr::arrange(USUBJID, DOSNO) %>%
                 dplyr::pull(IX),
               rep(1:5, 4))

  # Test if df_conc can be used with PKNCAconc by testing its output
  expect_s3_class(
    PKNCAconc(
      df_conc,
      formula = AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / ANALYTE,
      exclude_half.life = "exclude_half.life",
      time.nominal = "NFRLT"
    ),
    "PKNCAconc"
  )
})

test_that("create_conc handles empty input", {
  ADNCA <- data.frame()
  expect_error(create_conc(ADNCA,
                           group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "ANALYTE"),
                           time_column = "AFRLT"),
               regexp = "Input dataframe is empty. Please provide a valid ADNCA dataframe.")
})

test_that("create_conc handles missing columns", {
  ADNCA <- data.frame(
    STUDYID = rep(1, 10),
    USUBJID = rep(1:2, each = 5),
    PCSPEC = rep("Plasma", 10),
    DRUG = rep("DrugA", 10),
    AFRLT = seq(0, 9),
    AVAL = runif(10)
  )
  expect_error(create_conc(ADNCA,
                           group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "ANALYTE"),
                           time_column = "AFRLT"),
               regexp = "Missing required columns: ANALYTE")
})

test_that("create_conc handles multiple analytes", {
  ADNCA <- data.frame(
    STUDYID = rep(1, 20),
    USUBJID = rep(1, each = 10),
    PCSPEC = rep("Plasma", 20),
    DRUG = rep("DrugA", 20),
    ANALYTE = rep(c("Analyte1", "Analyte2"), each = 10),
    AFRLT = rep(seq(0, 9), 2),
    ARRLT = rep(seq(0, 9), 2),
    AVAL = runif(20)
  )
  df_conc <- create_conc(ADNCA,
                         group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "ANALYTE"),
                         time_column = "AFRLT")
  expect_equal(nrow(df_conc), 20)
  expect_equal(length(unique(df_conc$ANALYTE)), 2)
})

test_that("create_dose generates when missing the dose number column", {
  # Generate sample ADNCA data with at least two doses per subject
  ADNCA <- data.frame(
    STUDYID = rep(1, 20),
    USUBJID = rep(1:2, each = 10),
    PCSPEC = rep("Plasma", 20),
    DRUG = rep("DrugA", 20),
    ANALYTE = rep("Analyte1", 20),
    AFRLT = rep(seq(0, 9), 2),
    ARRLT = rep(seq(0, 4), 4),
    NFRLT = rep(seq(0, 9), 2),
    DOSEA = rep(c(5, 10), each = 10),
    ROUTE = rep(c("intravascular", "extravascular"), each = 10),
    ADOSEDUR = rep(c(0, 0), each = 10),
    AVAL = runif(20)
  )

  # Call create_conc
  df_conc <- create_conc(ADNCA,
                         group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "ANALYTE"),
                         time_column = "AFRLT")

  # Call create_dose
  df_dose <- create_dose(df_conc,
                         group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "ANALYTE"),
                         time_column = "AFRLT", since_lastdose_time_column = "ARRLT",
                         route_column = "ROUTE")

  # Test if df_dose is a data frame
  expect_s3_class(df_dose, "data.frame")

  # Test if df_dose has the correct columns
  expect_true(all(c("STUDYID", "USUBJID", "PCSPEC",
                    "DRUG", "ANALYTE", "AFRLT",
                    "ARRLT", "AVAL", "conc_groups",
                    "TIME", "IX", "ROUTE") %in% colnames(df_dose)))

  # Test if df_dose can be used with PKNCAdose by testing its output
  expect_s3_class(
    PKNCAdose(
      data = df_dose,
      formula = DOSEA ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID,
      route = "ROUTE",
      time.nominal = "NFRLT",
      duration = "ADOSEDUR"
    ),
    "PKNCAdose"
  )

  # Test if each subject has at least two doses
  dose_counts <- df_dose %>% group_by(USUBJID) %>% summarise(n = n())
  expect_true(all(dose_counts$n >= 2))
})

test_that("create_dose handles empty input", {
  df_conc <- data.frame()
  expect_error(create_dose(df_conc,
                           group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "ANALYTE"),
                           time_column = "AFRLT",
                           since_lastdose_time_column = "ARRLT",
                           route_column = "ROUTE"),
               regexp = "Input dataframe is empty. Please provide a valid concentration dataframe.")
})

test_that("create_dose_intervals handles incorrect input type", {
  mydose <- list(data = data.frame(STUDYID = 1, USUBJID = 1, TIME = 0),
                 columns = list(groups = c("STUDYID", "USUBJID"),
                                time = "TIME"))
  expect_error(create_dose_intervals(mydose),
               regexp = "Input must be a PKNCAdose object from the PKNCA package.")
})

test_that("create_dose handles negative time values", {
  df_conc <- data.frame(
    STUDYID = rep(1, 10),
    USUBJID = rep(1:2, each = 5),
    PCSPEC = rep("Plasma", 10),
    DRUG = rep("DrugA", 10),
    ANALYTE = rep("Analyte1", 10),
    AFRLT = c(-1, 0, 1, 2, 3, -1, 0, 1, 2, 3),
    ARRLT = c(-1, 0, 1, 2, 3, -1, 0, 1, 2, 3),
    ROUTE = rep(c("intravascular", "extravascular"), each = 5),
    DOSEA = 10,
    AVAL = runif(10)
  )
  df_dose <- create_dose(df_conc,
                         group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "ANALYTE"),
                         time_column = "AFRLT",
                         since_lastdose_time_column = "ARRLT",
                         route_column = "ROUTE")
  expect_true(all(df_dose$AFRLT >= 0))
})

test_that("create_dose handles multiple analytes", {
  df_conc <- data.frame(
    STUDYID = rep(1, 20),
    USUBJID = rep(1:2, each = 10),
    PCSPEC = rep("Plasma", 20),
    DRUG = rep("DrugA", 20),
    ANALYTE = rep(rep(c("Analyte1", "Analyte2"), 2), each = 5),
    AFRLT = rep(seq(0, 9), 2),
    ARRLT = c(rep(seq(0, 4), 2), rep(seq(5, 9), 2)),
    ROUTE = rep(c("intravascular", "extravascular"), each = 10),
    DOSEA = 10,
    AVAL = runif(20)
  )
  df_dose <- create_dose(df_conc,
                         group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "ANALYTE"),
                         time_column = "AFRLT",
                         since_lastdose_time_column = "ARRLT",
                         route_column = "ROUTE")
  expect_equal(nrow(df_dose), 4)
  expect_equal(length(unique(df_dose$ANALYTE)), 2)
})

test_that("create_dose_intervals generates correct dataset", {
  # Generate sample ADNCA data with at least two doses per subject
  ADNCA <- data.frame(
    STUDYID = rep(1, 20),
    USUBJID = rep(1:2, each = 10),
    PCSPEC = rep("Plasma", 20),
    DRUG = rep("DrugA", 20),
    ANALYTE = rep("Analyte1", 20),
    AFRLT = rep(seq(0, 9), 2),
    ARRLT = c(rep(seq(0, 4), 2), rep(seq(5, 9), 2)),
    NFRLT = rep(seq(0, 9), 2),
    ROUTE = "extravascular",
    DOSEA = 10,
    ADOSEDUR = 0,
    AVAL = runif(20)
  )

  # Call create_conc
  df_conc <- create_conc(ADNCA,
                         group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "ANALYTE"),
                         time_column = "AFRLT")

  # Call create_dose
  df_dose <- create_dose(df_conc,
                         group_columns = c("STUDYID", "USUBJID", "PCSPEC", "DRUG", "ANALYTE"),
                         time_column = "AFRLT",
                         since_lastdose_time_column = "ARRLT",
                         route_column = "ROUTE")

  # Generate PKNCAconc and PKNCAdose objects
  myconc <- PKNCAconc(
    df_conc,
    formula = AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / ANALYTE,
    exclude_half.life = "exclude_half.life",
    time.nominal = "NFRLT"
  )

  mydose <- PKNCAdose(
    data = df_dose,
    formula = DOSEA ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID,
    route = "ROUTE",
    time.nominal = "NFRLT",
    duration = "ADOSEDUR"
  )

  # Define the other arguments for the dose intervals
  params <- c("cmax", "tmax", "half.life")

  # Call create_dose_intervals
  myintervals <- create_dose_intervals(mydose, params = params, start_from_last_dose = TRUE)

  # Test if myintervals is a data frame
  expect_s3_class(myintervals, "data.frame")

  # Test if myintervals has the correct columns
  expect_true(all(c("start", "end", "STUDYID",
                    "USUBJID", "type_interval") %in% colnames(myintervals)))

  # Test if interval types are classified as "main"
  expect_true(all(myintervals$type_interval == "main"))

  # Test if myintervals can be used with PKNCAdata by testing its output
  expect_s3_class(
    PKNCA::PKNCAdata(
      data.conc = myconc,
      data.dose = mydose,
      intervals = myintervals,
      units = PKNCA::pknca_units_table(
        concu = myconc$data$PCSTRESU[1],
        doseu = myconc$data$DOSEU[1],
        amountu = myconc$data$PCSTRESU[1],
        timeu = myconc$data$RRLTU[1]
      )
    ),
    "PKNCAdata"
  )
})
