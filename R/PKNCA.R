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
  dosno_column <- "DOSNO"
  route_column <- "ROUTE"
  analyte_column <- "PARAM"
  matrix_column <- "PCSPEC"
  std_route_column <- "std_route"
  all_group_columns <- c(group_columns, usubjid_column, analyte_column, matrix_column)
  

  #Filter out flagged duplicates if DFLAG column available
  if ("DFLAG" %in% colnames(adnca_data)) {
    adnca_data <- adnca_data %>%
      filter(!DFLAG) %>%
      select(-DFLAG)
  }

  # Create concentration data
  df_conc <- format_pkncaconc_data(
    ADNCA = adnca_data,
    group_columns = all_group_columns,
    time_column = time_column,
    rrlt_column = "ARRLT",
    route_column = route_column
  ) %>%
    arrange(across(all_of(c(usubjid_column, time_column))))

  # Check for missing values in group columns
  na_columns <- all_group_columns[sapply(df_conc[, all_group_columns], function(x) any(is.na(x)))]

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
    group_columns = c(group_columns, usubjid_column),
    time_column = time_column
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
    time.nominal = "NFRLT",
    concu = "AVALU",
    timeu = "RRLTU"
  )

  pknca_dose <- PKNCA::PKNCAdose(
    data = df_dose,
    formula = DOSEA ~ TIME_DOSE | STUDYID + DRUG + USUBJID,
    route = std_route_column,
    time.nominal = "NFRLT",
    duration = "ADOSEDUR",
    doseu = "DOSEU"
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
    units = PKNCA_build_units_table(pknca_conc, pknca_dose)
  )
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

  data$options <- list(
    auc.method = method,
    progress = FALSE,
    keep_interval_cols = c("DOSNO", "DOSNOA", "type_interval"),
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
      DOSNO %in% selected_dosno,
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

PKNCA_build_units_table <- function(o_conc, o_dose) {
  
  o_conc <- ensure_column_unit_exists(o_conc, "concu")
  o_conc <- ensure_column_unit_exists(o_conc, "amountu")
  o_conc <- ensure_column_unit_exists(o_conc, "timeu")
  o_dose <- ensure_column_unit_exists(o_dose, "doseu")
  
  # Extract relevant columns from o_conc and o_dose
  group_dose_cols <- names(PKNCA::getGroups(o_dose))
  group_conc_cols <- names(PKNCA::getGroups(o_conc))
  concu_col <- o_conc$columns$concu
  timeu_col <- o_conc$columns$timeu
  doseu_col <- o_dose$columns$doseu
  
  # Join concentration and dose data (groups and units)
  groups_units_tbl <- pknca_full_join_conc_dose(o_conc, o_dose) %>%
    select(any_of(c(group_conc_cols, group_dose_cols,
                    concu_col, timeu_col, doseu_col))
    ) %>%
    # Derive the minimum expression of groups for the units
    mutate(across(everything(), ~ as.character(.))) %>%
    # Derive the minimum expression of groups for the units
    select(-any_of(c(o_conc$columns$subject, o_dose$columns$subject))) %>%
    unique() %>%
    select_relevant_columns(c(concu_col, timeu_col, doseu_col))
  PKNCA:::full_join_PKNCAconc_PKNCAdose(o_conc, o_dose)
  # TODO: Add some checkings for the groups_units_tbl
  conc_time_col <- o_conc$columns$time
  dose_time_col <- o_dose$columns$time
  bind_rows(
    o_conc$data %>% select(any_of(c(group_conc_cols, concu_col, timeu_col, conc_time_col))) %>%
      mutate(is.dose.col = FALSE),
    o_dose$data %>% select(any_of(c(group_dose_cols, doseu_col, dose_time_col))) %>%
      rename(!!conc_time_col := !!sym(dose_time_col)) %>%
      mutate(is.dose.col = TRUE)
  ) %>%
    arrange(across(any_of(c(group_dose_cols, conc_time_col)))) %>%
    # fill all missing values until the next is.dose.col == TRUE
    fill(everything(), .direction = "down")  %>%
    filter(!is.dose.col) %>%
    select(-is.dose.col)
  
  # Generate a PKNCA units table for each group
  groups_units_tbl %>%
    rowwise() %>%
    mutate(
      pknca_units_tbl = list(
        PKNCA::pknca_units_table(
          concu = !!sym(concu_col),
          doseu = !!sym(doseu_col),
          amountu = !!sym(concu_col),
          timeu = !!sym(timeu_col)
        )
      )
    ) %>%
    
    # Combine all PKNCA units tables into one
    unnest(cols = c(pknca_units_tbl)) %>%
    select(-any_of(c(concu_col, timeu_col, doseu_col))) %>%
    mutate(
      PPSTRESU = PPORRESU,
      conversion_factor = 1
    ) %>%
    select(any_of(c(group_conc_cols, group_dose_cols)),
           PPTESTCD, PPORRESU, PPSTRESU, conversion_factor) %>%
    unique()
}

select_relevant_columns <- function(data, target_col) {
  
  # If there is no target_col specified, simply return the original data
  if (is.null(target_col)) return(data)
  
  # Convert all columns to numeric factors for comparison
  data_binary <- data %>%
    mutate(target_col_derived = paste(!!!syms(target_col), sep = "_")) %>%
    select(-all_of(target_col)) %>%
    mutate(across(everything(), ~ as.numeric(as.factor(.))))
  
  # Extract the binary representation of the target column
  target_binary <- data_binary[["target_col_derived"]]
  
  # Check which columns have at least one change in value when the target column changes
  relevant_cols <- sapply(data_binary, function(col) any(diff(col) != 0 & diff(target_binary) != 0))
  relevant_cols <- names(relevant_cols)[relevant_cols] |>
    setdiff("target_col_derived")
  
  # Check if there is any duplicated column in data_binary
  non_unit_cols <- as.list(data_binary  %>% select(any_of(relevant_cols)))
  relevant_non_dup_cols <- names(non_unit_cols)[!duplicated(non_unit_cols, fromLast = TRUE)]
  
  # Select and return only the relevant columns from the original data
  
  data %>% select(any_of(c(relevant_non_dup_cols, target_col)))
}

# Force the creation of unit columns if they were not defined like that
# Note: (in the App units columns will always be defined)
ensure_column_unit_exists <- function(pknca_obj, unit_name) {
  if (is.null(pknca_obj$columns[[unit_name]])) {
    unit_colname <- make.unique(c(names(pknca_obj$data), unit_name))[ncol(pknca_obj$data) + 1]
    pknca_obj$columns[[unit_name]] <- unit_colname
    if (!is.null(pknca_obj$units[[unit_name]])) {
      pknca_obj$data[[unit_colname]] <- pknca_obj$units[[unit_name]]
    } else {
      pknca_obj$data[[unit_colname]] <- NA_character_
    }
  }
  pknca_obj
}

pknca_full_join_conc_dose <- function(o_conc, o_dose) {
  # Extract necessary columns from o_conc and o_dose
  group_conc_cols <- names(PKNCA::getGroups(o_conc))
  group_dose_cols <- names(PKNCA::getGroups(o_dose))
  conc_time_col <- o_conc$columns$time
  dose_time_col <- o_dose$columns$time
  
  # Combine concentration and dose data
  combined_data <- bind_rows(
    o_conc$data %>%
      mutate(is.dose.col = FALSE),
    o_dose$data %>%
      mutate(
        !!conc_time_col := !!sym(dose_time_col),
        is.dose.col = TRUE
      )
  ) %>%
    group_by(!!!syms(group_dose_cols)) %>%
    arrange(across(any_of(c(group_dose_cols, conc_time_col))), !is.dose.col) %>%
    fill(everything(), .direction = "downup") %>%
    ungroup() %>%
    filter(!is.dose.col) %>%
    select(-is.dose.col)

  combined_data
}

