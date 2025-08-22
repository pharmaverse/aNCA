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
PKNCA_create_slopes_object <- function( # nolint: object_name_linter
  adnca_data,
  method,
  selected_analytes,
  selected_profile,
  selected_pcspec,
  params = NULL,
  should_impute_c0 = TRUE
) {

  data <- adnca_data
  analyte_column <- data$conc$columns$groups$group_analyte
  unique_analytes <- unique(data$conc$data[[analyte_column]])
  
  data$options <- list(
    auc.method = method,
    progress = FALSE,
    keep_interval_cols = c(
      "NCA_PROFILE", "DOSNOA", "type_interval",
      adnca_data$dose$columns$route, "ROUTE"
    ),
    min.hl.r.squared = 0.01
  )
  
  # Format intervals
  data$intervals <- format_pkncadata_intervals(
    pknca_conc = data$conc,
    pknca_dose = data$dose,
    start_from_last_dose = should_impute_c0
  ) %>%
    mutate(across(any_of(params), ~ TRUE, .names = "{.col}")) %>% 
    # Join route information
    left_join(
      select(
        adnca_data$dose$data,
        any_of(c(group_vars(adnca_data$dose), adnca_data$dose$columns$route, "ROUTE"))
      ) %>% unique(),
      by = group_vars(adnca_data$dose)
    )
  
  # Apply filtering
  data$intervals <- data$intervals %>%
    filter(
      PARAM %in% selected_analytes,
      NCA_PROFILE %in% selected_profile,
      PCSPEC %in% selected_pcspec
    )
  
  data$impute <- NA
  
  # Impute start values if requested
  if (should_impute_c0) {
    data <- create_start_impute(data)
    
    # Don't impute parameters that are not AUC dependent
    params_auc_dep <- metadata_nca_parameters %>%
      filter(grepl("auc|aumc", PKNCA) | grepl("auc", Depends)) %>%
      pull(PKNCA)
    
    params_not_to_impute <- metadata_nca_parameters %>%
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