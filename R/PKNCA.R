#' Creates a `PKNCA::PKNCAdata` object.
#'
#' @details
#' This function creates a standard PKNCAdata object from ADNCA data.
#' It requires the following columns in the ADNCA data:
#' - STUDYID: Study identifier.
#' - PCSPEC: Matrix.
#' - ROUTE: Route of administration.
#' - DRUG: Drug identifier.
#' - USUBJID: Unique subject identifier.
#' - DOSNO: Dose profile number.
#' - ANALYTE: Analyte.
#' - AVAL: Analysis value.
#' - AVALU: AVAL unit.
#' - DOSEA: Dose amount.
#' - DOSEU: Dose unit.
#' - AFRLT: Actual time from first dose.
#' - ARRLT: Actual time from reference dose.
#' - NFRLT: Nominal time from first dose.
#' - ADOSEDUR: Duration of dose.
#' - RRLTU: Time unit.
#'
#' 1. Creating pk concentration data using `format_pkncaconc_data()`.
#' 2. Creating dosing data using `format_pkncadose_data()`.
#' 3. Creating `PKNCAconc` object using `PKNCA::PKNCAconc()`.
#' with formula `AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / ANALYTE`.
#' 4. Creating PKNCAdose object using `PKNCA::PKNCAdose()`.
#' with formula `DOSEA ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID`.
#' 5. Creating PKNCAdata object using `PKNCA::PKNCAdata()`.
#' 6. Updating units in PKNCAdata object so each analyte has its own unit.
#'
#' @param adnca_data Data table containing ADNCA data.
#'
#' @returns `PKNCAdata` object with concentration, doses, and units based on ADNCA data.
#'
#' @examples
#' adnca_data <- data.frame(
#' STUDYID = rep("STUDY001", 6),
#' PCSPEC = rep("Plasma", 6),
#' ROUTE = rep("IV", 6),
#' DRUG = rep("DrugA", 6),
#' USUBJID = rep("SUBJ001", 6),
#' DOSNO = rep(1, 6),
#' ANALYTE = rep("AnalyteA", 6),
#' AVAL = c(0, 5, 10, 7, 3, 1),
#' AVALU = rep("ng/mL", 6),
#' DOSEA = rep(100, 6),
#' DOSEU = rep("mg", 6),
#' AFRLT = c(0, 1, 2, 3, 4, 6),
#' ARRLT = c(0, 1, 2, 3, 4, 6),
#' NFRLT = c(0, 1, 2, 3, 4, 6),
#' ADOSEDUR = rep(0.5, 6),
#' RRLTU = rep("hour", 6)
#' )
#' PKNCA_create_data_object(adnca_data)
#'
#' @export
PKNCA_create_data_object <- function(adnca_data) { # nolint: object_name_linter
  # Define column names based on ADNCA vars
  group_columns <- intersect(colnames(adnca_data), c("STUDYID", "PCSPEC", "ROUTE", "DRUG"))
  usubjid_column <- "USUBJID"
  time_column <- "AFRLT"
  dosno_column <- "DOSNO"
  route_column <- "ROUTE"
  analyte_column <- "ANALYTE"
  matrix_column <- "PCSPEC"
  std_route_column <- "std_route"

  # Create concentration data
  df_conc <- format_pkncaconc_data(
    ADNCA = adnca_data,
    group_columns = c(group_columns, usubjid_column, analyte_column),
    time_column = time_column,
    route_column = route_column,
    dosno_column = dosno_column
  ) %>%
    arrange(across(all_of(c(usubjid_column, time_column))))

  # Create dosing data
  df_dose <- format_pkncadose_data(
    pkncaconc_data = df_conc,
    group_columns = c(group_columns, usubjid_column),
    time_column = time_column,
    dosno_column = dosno_column,
    since_lastdose_time_column = "ARRLT"
  )

  # Set default settings
  df_conc$is.excluded.hl <- FALSE
  df_conc$is.included.hl <- FALSE
  df_conc$REASON <- NA
  df_conc$exclude_half.life <- FALSE

  # Create PKNCA objects
  pknca_conc <- PKNCA::PKNCAconc(
    df_conc,
    formula = AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / ANALYTE,
    exclude_half.life = "exclude_half.life",
    time.nominal = "NFRLT"
  )

  pknca_dose <- PKNCA::PKNCAdose(
    data = df_dose,
    formula = DOSEA ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID,
    route = std_route_column,
    time.nominal = "NFRLT",
    duration = "ADOSEDUR"
  )

  # create basic intervals so that PKNCAdata can be created
  # TODO: investigate if this is required
  intervals <-
    data.frame(
      start = 0, end = Inf,
      cmax = TRUE,
      tmax = TRUE,
      auclast = FALSE,
      aucinf.obs = FALSE
    )

  pknca_data_object <- PKNCA::PKNCAdata(
    data.conc = pknca_conc,
    data.dose = pknca_dose,
    intervals = intervals, #TODO: should be default
    units = PKNCA::pknca_units_table(
      concu = pknca_conc$data$AVALU[1],
      doseu = pknca_conc$data$DOSEU[1],
      amountu = pknca_conc$data$AVALU[1],
      timeu = pknca_conc$data$RRLTU[1]
    )
  )

  # Update units
  unique_analytes <- unique(
    pknca_data_object$conc$data[[pknca_data_object$conc$columns$groups$group_analyte]]
  )
  analyte_column <- pknca_data_object$conc$columns$groups$group_analyte
  pknca_data_object$units <- tidyr::crossing(
    pknca_data_object$units, !!sym(analyte_column) := unique_analytes
  ) %>%
    mutate(PPSTRESU = PPORRESU, conversion_factor = 1)

  pknca_data_object
}

#' Calculates results for PKNCA analysis.
#'
#' @details
#' This function+ calculates results for PKNCA analysis using `PKNCA::pk.nca()`.
#' It then joins the results with the dosing data, to create a full results data frame
#' with the start and end times for each dose, from first and most recent dose.
#'
#' @param pknca_data Data object created using PKNCA::PKNCAdata() function.
#'
#' @returns Results object with start and end times for each dose, from first dose
#' and from most recent dose
#'
#' @examples
#' example_data <- data.frame(
#'   STUDYID = rep("STUDY001", 6),
#'   PCSPEC = rep("Plasma", 6),
#'   ROUTE = rep("IV", 6),
#'   DRUG = rep("DrugA", 6),
#'   USUBJID = rep("SUBJ001", 6),
#'   DOSNO = rep(1, 6),
#'   ANALYTE = rep("AnalyteA", 6),
#'   AVAL = c(0, 5, 10, 7, 3, 1),
#'   AVALU = rep("ng/mL", 6),
#'   DOSEA = rep(100, 6),
#'   DOSEU = rep("mg", 6),
#'   AFRLT = c(0, 1, 2, 3, 4, 6),
#'   ARRLT = c(0, 1, 2, 3, 4, 6),
#'   NFRLT = c(0, 1, 2, 3, 4, 6),
#'   ADOSEDUR = rep(0.5, 6),
#'   RRLTU = rep("hour", 6)
#' )
#'
#' # Create a PKNCAdata object
#' pknca_data <- PKNCA_create_data_object(example_data)
#'
#' # Perform NCA calculations
#' nca_results <- PKNCA_calculate_nca(pknca_data)
#'
#' @export
PKNCA_calculate_nca <- function(pknca_data) { # nolint: object_name_linter
  results <- PKNCA::pk.nca(data = pknca_data, verbose = FALSE)

  results$result <- results$result %>%
    inner_join(
      select(pknca_data$dose$data, -exclude, -pknca_data$conc$columns$groups$group_analyte)
      # TODO: add `by = `argument to avoid warnings
    ) %>%
    mutate(
      start_dose = start - !!sym(results$data$dose$columns$time),
      end_dose = end - !!sym(results$data$dose$columns$time)
    ) %>%
    select(names(results$result), start_dose, end_dose) %>%
    # TODO: PKNCA package should offer a better solution to this at some point
    # Prevent that when t0 is used with non-imputed params to show off two result rows
    # just choose the derived ones (last row always due to interval_helper funs)
    group_by(across(-c(PPSTRES, PPORRES, exclude))) %>%
    slice_tail(n = 1) %>%
    ungroup()

  results
}

#' This function imputes the start concentration using the log slope method.
#'
#' @param conc Numeric vector of concentrations.
#' @param time Numeric vector of times corresponding to the concentrations.
#' @param start Numeric value indicating the start/dose time.
#' @param end Numeric value indicating the end time.
#' @param ... Additional arguments (currently not used).
#' @param options List of options (currently not used).
#'
#' @returns A data frame with imputed start concentration.
#' @details
#' This function adheres to the structure required by the `PKNCA` package to work with its
#' functionalities. For more information, see the
#' [PKNCA Data Imputation Vignette](https://cran.r-project.org/web/packages/PKNCA/vignettes).
#' @export
#'
#' @examples
#' conc <- c(5, 4, 3, 2, 1)
#' time <- c(1, 2, 3, 4, 5)
#' start <- 0
#' end <- 4
#' PKNCA_impute_method_start_logslope(conc, time, start, end)

PKNCA_impute_method_start_logslope <- function(conc, time, start, end, ..., options = list()) { # nolint

  d_conc_time <- data.frame(conc = conc, time = time)
  if (!any(time == start)) {
    all_concs <- conc[time >= start  &  time <= end]
    all_times <- time[time >= start  &  time <= end]
    if (!all(is.na(all_concs))) {
      c0 <- PKNCA::pk.calc.c0(all_concs, all_times, method = "logslope")
      if (!is.na(c0)) {
        d_conc_time <- rbind(d_conc_time, data.frame(time = start, conc = c0))
        d_conc_time <- d_conc_time[order(d_conc_time$time), ]
      }
    }
  }
  d_conc_time
}

#' This function imputes the start concentration using the first concentration after dose
#'
#' @param conc Numeric vector of concentrations.
#' @param time Numeric vector of times corresponding to the concentrations.
#' @param start Numeric value indicating the start/dose time.
#' @param end Numeric value indicating the end time.
#' @param ... Additional arguments (currently not used).
#' @param options List of options (currently not used).
#'
#' @returns A data frame with imputed start concentration.
#' @details
#' This function adheres to the structure required by the `PKNCA` package to work with its
#' functionalities.For more information, see the
#' [PKNCA Data Imputation Vignette](https://cran.r-project.org/web/packages/PKNCA/vignettes).
#' @export
#'
#' @examples
#' conc <- c(1, 2, 3, 4, 5)
#' time <- c(1, 2, 3, 4, 5)
#' start <- 0
#' end <- 4
#' PKNCA_impute_method_start_c1(conc, time, start, end)
PKNCA_impute_method_start_c1 <- function(conc, time, start, end, ..., options = list()) { # nolint
  d_conc_time <- data.frame(conc = conc, time = time)
  if (!any(time == start)) {
    all_concs <- conc[time >= start & time <= end]
    all_times <- time[time >= start & time <= end]
    if (!all(is.na(all_concs))) {
      c1 <- all_concs[which.min(all_times)]
      d_conc_time <- rbind(d_conc_time, data.frame(time = start, conc = c1))
      d_conc_time <- d_conc_time[order(d_conc_time$time), ]
    }
  }
  d_conc_time
}
