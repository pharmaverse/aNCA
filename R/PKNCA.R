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
#' - PARAM: Analyte.
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
#' with formula `AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / PARAM`.
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
#' PARAM = rep("AnalyteA", 6),
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
  group_columns <- intersect(colnames(adnca_data), c("STUDYID", "ROUTE", "DRUG"))
  usubjid_column <- "USUBJID"
  time_column <- "AFRLT"
  dosno_column <- "NCA_PROFILE"
  route_column <- "ROUTE"
  analyte_column <- "PARAM"
  matrix_column <- "PCSPEC"
  std_route_column <- "std_route"

  #Filter out flagged duplicates if DFLAG column available
  if ("DFLAG" %in% colnames(adnca_data)) {
    adnca_data <- adnca_data %>%
      filter(!DFLAG) %>%
      select(-DFLAG)
  }

  # Create concentration data
  df_conc <- format_pkncaconc_data(
    ADNCA = adnca_data,
    group_columns = c(group_columns, usubjid_column, analyte_column, matrix_column),
    time_column = time_column,
    rrlt_column = "ARRLT",
    route_column = route_column
  ) %>%
    arrange(across(all_of(c(usubjid_column, time_column))))

  # Check for missing values in group columns
  na_columns <- group_columns[sapply(df_conc[, group_columns], function(x) any(is.na(x)))]

  if (length(na_columns) > 0) {
    stop(
      "Missing values detected in grouping columns: ",
      paste(na_columns, collapse = ", "),
      ". Please ensure all columns contain values."
    )
  }

  # Create dosing data
  df_dose <- format_pkncadose_data(
    pkncaconc_data = df_conc,
    group_columns = c(group_columns, usubjid_column)
  )

  # Set default settings
  df_conc$is.excluded.hl <- FALSE
  df_conc$is.included.hl <- FALSE
  df_conc$REASON <- NA
  df_conc$exclude_half.life <- FALSE

  # Create PKNCA objects
  pknca_conc <- PKNCA::PKNCAconc(
    df_conc,
    formula = AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / PARAM,
    exclude_half.life = "exclude_half.life",
    include_half.life = "include_half.life",
    time.nominal = "NFRLT"
  )

  pknca_dose <- PKNCA::PKNCAdose(
    data = df_dose,
    formula = DOSEA ~ TIME_DOSE | STUDYID + DRUG + USUBJID,
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



#' Create a PKNCAdata Object for NCA or Slope Analysis
#'
#' This function updates a previously prepared `PKNCAdata` object
#' based on user selections for method, analyte, dose, specimen, and parameters.
#' @details
#' Step 1: Update units in the `PKNCAdata` object
#' ensuring unique analytes have their unique units
#'
#' Step 2: Set `PKNCAoptions` for NCA calculation
#'
#' Step 3: Format intervals using `format_pkncadata_intervals()`
#'
#' Step 4: Apply filtering based on user selections and partial aucs
#'
#' Step 5: Impute start values if requested
#'
#' @param adnca_data A reactive PKNCAdata object
#' @param auc_data A data frame containing partial aucs added by user
#' @param method NCA calculation method selection
#' @param selected_analytes User selected analytes
#' @param selected_dosno User selected dose numbers
#' @param selected_pcspec User selected specimen
#' @param params A list of parameters for NCA calculation
#' @param should_impute_c0 Logical indicating if start values should be imputed
#'
#' @returns A fully configured `PKNCAdata` object.
#'
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr crossing
#' @importFrom rlang sym
#' @importFrom purrr pmap
#'
#' @export
PKNCA_update_data_object <- function( # nolint: object_name_linter
  adnca_data,
  auc_data,
  method,
  selected_analytes,
  selected_dosno,
  selected_pcspec,
  params,
  should_impute_c0 = TRUE
) {

  data <- adnca_data
  analyte_column <- data$conc$columns$groups$group_analyte
  unique_analytes <- unique(data$conc$data[[analyte_column]])

  # Add and expand units
  data$units <-  data$units %>%
    filter(!!sym(analyte_column) %in% selected_analytes) %>%
    select(-!!sym(analyte_column)) %>%
    tidyr::crossing(!!sym(analyte_column) := unique_analytes) %>%
    mutate(PPSTRESU = PPORRESU, conversion_factor = 1)

  data$options <- list(
    auc.method = method,
    progress = FALSE,
    keep_interval_cols = c("NCA_PROFILE", "DOSNOA", "type_interval"),
    min.hl.r.squared = 0.01
  )

  # Format intervals
  data$intervals <- format_pkncadata_intervals(
    pknca_conc = data$conc,
    pknca_dose = data$dose,
    params = params,
    start_from_last_dose = should_impute_c0
  )

  # Apply filtering
  data$intervals <- data$intervals %>%
    filter(
      PARAM %in% selected_analytes,
      NCA_PROFILE %in% selected_dosno,
      PCSPEC %in% selected_pcspec
    )

  # # Add partial AUCs if any

  auc_ranges <- auc_data %>%
    filter(!is.na(start_auc), !is.na(end_auc), start_auc >= 0, end_auc > start_auc)

  # Make a list of intervals from valid AUC ranges
  intervals_list <- pmap(auc_ranges, function(start_auc, end_auc) {
    data$intervals %>%
      mutate(
        start = start + start_auc,
        end = start + (end_auc - start_auc),
        across(where(is.logical), ~FALSE),
        aucint.last = TRUE,
        type_interval = "manual"
      )
  })

  data$intervals <- bind_rows(
    data$intervals,
    intervals_list
  ) %>%
    unique()

  data$impute <- NA

  # Impute start values if requested
  if (should_impute_c0) {
    data <- create_start_impute(data)

    # Don't impute parameters that are not AUC dependent
    params_auc_dep <- pknca_cdisc_terms %>%
      filter(grepl("auc|aumc", PKNCA) | grepl("auc", Depends)) %>%
      pull(PKNCA)

    params_not_to_impute <- pknca_cdisc_terms %>%
      filter(!grepl("auc|aumc", PKNCA),
             !grepl(paste0(params_auc_dep, collapse = "|"), Depends)) %>%
      pull(PKNCA) |>
      intersect(names(PKNCA::get.interval.cols()))

    all_impute_methods <- na.omit(unique(data$intervals$impute))

    data$intervals <- Reduce(function(d, ti_arg) {
      interval_remove_impute(
        d,
        target_impute = ti_arg,
        target_params = params_not_to_impute
      )
    }, all_impute_methods, init = data$intervals)
  }

  data
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
#'   PARAM = rep("AnalyteA", 6),
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

  # Calculate results using PKNCA
  results <- PKNCA::pk.nca(data = pknca_data, verbose = FALSE)

  dose_data_to_join <- select(
    pknca_data$dose$data,
    unlist(unname(pknca_data$dose$columns$groups)),
    pknca_data$dose$columns$time,
    pknca_data$dose$columns$dose,
    DOSNOA
  )

  results$result <- results$result %>%
    inner_join(
      dose_data_to_join,
      by = intersect(names(.), names(dose_data_to_join))
    ) %>%
    mutate(
      start_dose = start - !!sym(results$data$dose$columns$time),
      end_dose = end - !!sym(results$data$dose$columns$time)
    ) %>%
    select(names(results$result), start_dose, end_dose) %>%
    # Make empty strings for units that have no metric (unitless, fraction...)
    mutate(PPSTRESU = ifelse(PPSTRESU %in% c("unitless", "fraction"), "", PPSTRESU)) %>%
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
#' [PKNCA Data Imputation Vignette](https://CRAN.R-project.org/package=PKNCA).
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
#' [PKNCA Data Imputation Vignette](https://CRAN.R-project.org/package=PKNCA).
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
