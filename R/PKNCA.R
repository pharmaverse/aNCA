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
#' - NCA_PROFILE: (Non- standard column). Can be any column, used for filtering the data for NCA
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
#' with formula `DOSEA ~ TIME | STUDYID + DRUG + USUBJID`.
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
#' NCA_PROFILE = rep(1, 6),
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
#' Note*: The function assumes that the `adnca_data` object has been
#' created using the `PKNCA_create_data_object()` function.
#'
#' @param adnca_data A reactive PKNCAdata object
#' @param auc_data A data frame containing partial aucs added by user
#' @param method NCA calculation method selection
#' @param selected_analytes User selected analytes
#' @param selected_profile User selected dose numbers/profiles
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
  selected_profile,
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
      NCA_PROFILE %in% selected_profile,
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
#'   NCA_PROFILE = rep(1, 6),
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
    # Use standard CDISC unit names
    # TODO (Gerardo): When PKNCA changes these unit names, remove this part
    mutate(
      PPORRESU = case_when(
        PPORRESU == "unitless" ~ "",
        PPORRESU == "fraction" ~ "fraction of 1",
        TRUE ~ PPORRESU
      ),
      PPSTRESU = case_when(
        PPSTRESU == "unitless" ~ "",
        PPSTRESU == "fraction" ~ "fraction of 1",
        TRUE ~ PPSTRESU
      )
    ) %>%
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
      c0 <- PKNCA::pk.calc.c0(all_concs, all_times, time.dose = start, method = "logslope")
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

#' Build Units Table for PKNCA
#'
#' This function generates a PKNCA units table including the potential unit segregating columns
#' among the dose and/or concentration groups.
#'
#' @param o_conc A PKNCA concentration object (PKNCAconc).
#' @param o_dose A PKNCA dose object (PKNCAdose).
#'
#' @returns A data frame containing the PKNCA formatted units table.
#'
#' @details
#' The function performs the following steps:
#' 1. Ensures the unit columns (e.g., `concu`, `timeu`, `doseu`, `amountu`) exist in the inputs.
#' 2. Joins the concentration and dose data based on their grouping columns.
#' 3. Generates a PKNCA units table for each group, including conversion factors and custom units.
#' 4. Returns a unique table with relevant columns for PKNCA analysis.
#'
#' @examples
#' # Assuming `o_conc` and `o_dose` are valid PKNCA objects:
#' # 1) Sharing group variables in their formulas
#' # 2) Time units are the same within dose groups
#' # 3) Units are the same for subjects within the same concentration group
#'
#' d_conc <- data.frame(
#'   subj = 1,
#'   analyte = rep(c("A", "B"), each = 2),
#'   concu = rep(c("ng/mL", "ug/mL"), each = 2),
#'   conc = c(0, 2, 0, 5),
#'   time = rep(0:1, 2),
#'   timeu = "h"
#' )
#' d_dose <- data.frame(
#'   subj = 1,
#'   dose = 100,
#'   doseu = "mg",
#'   time = 0,
#'   timeu = "h"
#' )
#' o_conc <- PKNCA::PKNCAconc(d_conc, conc ~ time | subj / analyte, concu = "concu")
#' o_dose <- PKNCA::PKNCAdose(d_dose, dose ~ time | subj, doseu = "doseu")
#' units_table <- PKNCA_build_units_table(o_conc, o_dose)
#'
#' @importFrom dplyr select mutate rowwise any_of across everything %>% add_count inner_join
#' @importFrom tidyr unnest
#' @importFrom rlang sym syms
#' @importFrom utils capture.output
#' @export
PKNCA_build_units_table <- function(o_conc, o_dose) { # nolint

  o_conc <- ensure_column_unit_exists(o_conc, c("concu", "timeu", "amountu"))
  o_dose <- ensure_column_unit_exists(o_dose, c("doseu"))

  # Extract relevant columns from o_conc and o_dose
  group_dose_cols <- group_vars(o_dose)
  group_conc_cols <- group_vars(o_conc)
  concu_col <- o_conc$columns$concu
  amountu_col <- o_conc$columns$amountu
  timeu_col <- o_conc$columns$timeu
  doseu_col <- o_dose$columns$doseu
  all_unit_cols <- c(concu_col, amountu_col, timeu_col, doseu_col)

  # Join dose units with concentration group columns and units
  groups_units_tbl <- left_join(
    o_conc$data %>%
      select(any_of(c(group_conc_cols, concu_col, amountu_col, timeu_col))) %>%
      unique(),
    o_dose$data %>%
      select(any_of(c(group_dose_cols, doseu_col))) %>%
      unique(),
    by = intersect(group_conc_cols, group_dose_cols)
  ) %>%
    # Prevent any issue with NAs in the group(s) or unit columns
    mutate(across(everything(), ~ as.character(.))) %>%
    unique()

  # Check that at least for each concentration group units are uniform
  mismatching_units_groups <- groups_units_tbl %>%
    add_count(!!!syms(group_conc_cols), name = "n") %>%
    filter(n > 1) %>%
    select(-n)
  if (nrow(mismatching_units_groups) > 0) {
    stop(
      "Units should be uniform at least across concentration groups.",
      "Review the units for the next group(s):\n",
      paste(utils::capture.output(print(mismatching_units_groups)), collapse = "\n")
    )
  }

  # Generate the PKNCA units table
  groups_units_tbl %>%
    # Pick only the group columns that are relevant in stratifying the units
    select_minimal_grouping_cols(all_unit_cols) %>%
    unique() %>%
    # Create a PKNCA units table for each group
    rowwise() %>%
    mutate(
      pknca_units_tbl = list(
        PKNCA::pknca_units_table(
          concu = !!sym(concu_col),
          doseu = !!sym(doseu_col),
          amountu = !!sym(amountu_col),
          timeu = !!sym(timeu_col)
        )
      )
    ) %>%
    # Combine all PKNCA units tables into one
    unnest(cols = c(pknca_units_tbl)) %>%
    mutate(
      PPSTRESU = PPORRESU,
      conversion_factor = 1
    ) %>%
    # Order the columns to have them in a clean display
    select(any_of(c(group_conc_cols, group_dose_cols)),
           PPTESTCD, PPORRESU, PPSTRESU, conversion_factor)
}

#' Ensure Unit Columns Exist in PKNCA Object
#'
#' Checks if specified unit columns exist in a PKNCA object (either PKNCAconc or PKNCAdose).
#' If the columns do not exist, it creates them and assigns default values (NA or existing units).
#'
#' @param pknca_obj A PKNCA object (either PKNCAconc or PKNCAdose).
#' @param unit_name A character vector of unit column names to ensure (concu, amountu, timeu...).
#' @returns The updated PKNCA object with ensured unit columns.
#'
#' @details
#' The function performs the following steps:
#' 1. Checks if the specified unit columns exist in the PKNCA object.
#' 2. If a column does not exist, it creates the column and assigns default values.
#' 3. If not default values are provided, it assigns NA to the new column.
ensure_column_unit_exists <- function(pknca_obj, unit_name) {
  for (unit in unit_name) {
    if (is.null(pknca_obj$columns[[unit]])) {
      unit_colname <- make.unique(c(names(pknca_obj$data), unit))[ncol(pknca_obj$data) + 1]
      pknca_obj$columns[[unit]] <- unit_colname
      if (!is.null(pknca_obj$units[[unit]])) {
        pknca_obj$data[[unit_colname]] <- pknca_obj$units[[unit]]
      } else {
        pknca_obj$data[[unit_colname]] <- NA_character_
      }
    }
  }
  pknca_obj
}

#' Find Minimal Grouping Columns for Strata Reconstruction
#'
#' This function identifies the smallest set of columns in a data frame whose unique combinations
#' can reconstruct the grouping structure defined by the specified strata columns.
#' It removes duplicate, constant, and redundant columns, then searches for the minimal combination
#' that uniquely identifies each stratum.
#'
#' @param df A data frame.
#' @param strata_cols Column names in df whose unique combination defines the strata.
#' @returns A data frame containing the strata columns and their minimal set of grouping columns.
select_minimal_grouping_cols <- function(df, strata_cols) {
  # If there is no strata_cols specified, simply return the original df
  if (length(strata_cols) == 0) return(df)

  # Obtain the comb_vals values of the target column(s)
  strata_vals <- df %>%
    mutate(strata_cols_comb = paste(!!!syms(strata_cols), sep = "_")) %>%
    pull(strata_cols_comb)

  # If the target column(s) only has one level, there are no relevant columns
  if (length(unique(strata_vals)) == 1) {
    return(df[strata_cols])
  }

  candidate_cols <- setdiff(names(df), strata_cols)
  # 1. Remove columns that are duplicates in levels terms
  candidate_levels <- lapply(
    df[candidate_cols], function(x) as.numeric(factor(x, levels = unique(x)))
  )
  candidate_cols <- candidate_cols[!duplicated(candidate_levels)]

  # 2. Remove columns with only 1 level
  candidate_n_levels <- sapply(df[candidate_cols], function(x) length(unique(x)))
  candidate_cols <- candidate_cols[candidate_n_levels > 1]

  # 3. Check combinations of columns to find minimal key combination to level group strata_cols
  for (n in seq_len(length(candidate_cols))) {
    all_candidate_combs <- combn(candidate_cols, n, simplify = FALSE)
    for (comb in all_candidate_combs) {
      comb_vals <- apply(df[, comb, drop = FALSE], 1, paste, collapse = "_")
      if (all(tapply(strata_vals, comb_vals, FUN = \(x) length(unique(x)) == 1))) {
        return(df[c(comb, strata_cols)])
      }
    }
  }
  df[strata_cols]
}

#' Exclude NCA results based on user-defined rules over the half-life related parameters
#' This function applies exclusion rules to the NCA results based on user-defined parameters.
#' @param res A PKNCAresults object containing the NCA results.
#' @param rules A list of exclusion rules where each rule is a named vector.
#' @returns A PKNCAresults object with the exclusions applied.
#' @details
#' The function iterates over the rules and applies the exclusion criteria to the NCA results.
#' For any parameter that is not aucpext.obs or aucpext.pred it applies a minimum threshold,
#' and for aucpext.obs and aucpext.pred it applies a maximum threshold.
#' @importFrom PKNCA exclude
#' @export
PKNCA_hl_rules_exclusion <- function(res, rules) { # nolint

  for (param in names(rules)) {
    if (startsWith(param, "aucpext")) {
      exc_fun <- exclude_nca_by_param(
        param,
        max_thr = rules[[param]],
        affected_parameters = PKNCA::get.parameter.deps("half.life")
      )
    } else {
      exc_fun <- exclude_nca_by_param(
        param,
        min_thr = rules[[param]],
        affected_parameters = PKNCA::get.parameter.deps("half.life")
      )
    }
    res <- PKNCA::exclude(res, FUN = exc_fun)
  }
  res
}
