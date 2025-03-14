#' Creates a `PKNCA::PKNCAdata` object.
#'
#' @details
#' TODO
#'
#' @param adnca_data Data table containing ADNCA data.
#'
#' @returns `PKNCAdata` object with... TODO
#'
#' @examples
#' TODO
#'
#' @export
PKNCA_create_data_object <- function(adnca_data) { # nolint: object_name_linter
  # Define column names
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
    intervals = intervals,
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