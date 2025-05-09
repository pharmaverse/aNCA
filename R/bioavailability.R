#' Calculate bioavailability for intravascular vs extravascular aucs
#'
#' This function calculates bioavailability (F) based on AUC (Area Under Curve) data
#' extracted from `res_nca`. It computes individual bioavailability
#'  where IV and EX data are available
#' for a subject. If IV data is missing, it estimates bioavailability
#'  using the mean IV values for that grouping.
#'
#' @details
#' - The function extracts AUC data from `res_nca$data$conc$data` and
#'  filters for selected AUC types.
#' - It separates data into intravascular (IV) and extravascular (EX) groups.
#' - Individual bioavailability is calculated for subjects with both
#'  IV and EX data using PKNCA function `pk.calc.f`.
#' - If IV data is missing for a subject, the function estimates
#'  bioavailability using mean IV values for that grouping.
#' - The final output includes bioavailability estimates
#'  for individual subjects and mean-based estimates.
#' @param res_nca A list containing non-compartmental analysis (NCA) results,
#'  including concentration and dose data.
#' @param selected_aucs A character vector of selected
#'  AUC variables (e.g., `c("f_aucinf.obs", "f_auclast")`).
#'
#' @returns A data frame with calculated bioavailability values (`f_aucinf`, `f_auclast`, etc.)
#'   for individual subjects where IV data is available. If IV data is missing,
#'  it estimates bioavailability using the mean IV AUC for that grouping.
#'
#' @importFrom dplyr bind_rows filter full_join group_by left_join mutate select summarize
#' @importFrom purrr reduce
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
#'
#' @export
calculate_F <- function(res_nca, selected_aucs) { # nolint: object_name_linter

  # Extract and clean AUC selection
  auc_vars <- gsub("^f_", "", selected_aucs)

  #check if selected_aucs are available
  if (length(selected_aucs) == 0) {
    return(NULL)
  }

  if (!any(res_nca$result$PPTESTCD %in% auc_vars)) {
    stop(paste0("No AUC data available for: ", paste(auc_vars, collapse = ", ")))
  }

  # Extract required columns
  route_col <- res_nca$data$dose$columns$route
  dose_col <- res_nca$data$dose$columns$dose
  group_cols <- PKNCA::getGroups(res_nca$data$conc) %>%
    names() %>%
    append("DOSNO")

  # Extract dose information (route and dose)
  dose_info <- res_nca$data$dose$data %>%
    select(any_of(c(group_cols, route_col, dose_col)), USUBJID) %>%
    distinct()

  # Filter AUC data requested for bioavailiability calculations
  auc_data <- res_nca$result %>%
    filter(PPTESTCD %in% auc_vars) %>%
    select(any_of(group_cols), PPTESTCD, PPORRES, start, end) %>%
    pivot_wider(names_from = PPTESTCD, values_from = PPORRES)

  auc_data %>%
    # Merge dose information (route and dose)
    left_join(
      dose_info,
      by = intersect(names(auc_data), names(dose_info))
    ) %>%
    # Calculate the mean Dose & AUC values for IV & EX routes
    rename(Route = all_of(route_col), Dose = all_of(dose_col)) %>%
    pivot_longer(
      cols = any_of(auc_vars),
      names_to = "auc_type",
      values_to = "vals"
    ) %>%
    pivot_wider(
      names_from = Route,
      values_from = c(vals, Dose)
    ) %>%
    group_by(auc_type, !!!syms(setdiff(group_cols, "USUBJID")), start, end) %>%
    mutate(
      Mean_AUC_IV = mean(vals_intravascular, na.rm = TRUE),
      Mean_Dose_IV = mean(Dose_intravascular, na.rm = TRUE),
    ) %>%
    # Calculate F using group mean values when individual is not already present
    ungroup() %>%
    mutate(
      vals_intravascular = ifelse(
        is.na(vals_intravascular) | is.na(Dose_intravascular),
        Mean_AUC_IV,
        vals_intravascular
      ),
      dose_iv = ifelse(
        is.na(vals_intravascular) | is.na(Dose_intravascular),
        Mean_Dose_IV,
        Dose_intravascular
      ),
      PPORRES = pk.calc.f(
        Dose_intravascular, vals_intravascular,
        Dose_extravascular, vals_extravascular
      ) * 100,
      PPORRESU = "%",
      PPSTRES = PPORRES,
      PPSTRESU = "%"
    ) %>%
    # Arrange the data to keep 1 row per group
    mutate(
      PPTESTCD = paste0("f_", auc_type),
      type_interval = "main"
    ) %>%
    select(any_of(c(names(res_nca$result))))
}

#' Add bioavailability to PKNCAresults object
#' This helper function adds bioavailability (F) data to a PKNCAresults object.
#' The bioavailability is calculated with the calculate_bioavailability function.
#'
#' @param res_nca A list containing non-compartmental analysis (NCA) results,
#' including concentration and dose data.
#' @param bioavailability A data frame with calculated bioavailability values
#'
#' @returns A PKNCAresults object with bioavailability data added to the result slot.
#'
#' @importFrom dplyr bind_rows distinct left_join mutate select
#' @importFrom purrr list_c keep
#' @importFrom tidyr pivot_longer
#' @importFrom PKNCA pk.calc.f
#'
#' @export
PKNCA_add_F <- function(res_nca, bioavailability) { # nolint: object_name_linter

  if (is.null(bioavailability)) {
    return(res_nca)
  }
  # Extract ID groups
  group_cols <- PKNCA::getGroups(res_nca$data$conc) %>%
    names() %>%
    append("DOSNO")

  # Pivot results data
  subj_data <- res_nca$result %>%
    select(-starts_with("PP"), -exclude) %>%
    distinct()

  # Create bioavailability data in resnca format
  f_results <- subj_data %>%
    left_join(bioavailability, by = group_cols) %>%
    pivot_longer(
      cols = starts_with("f_"),  # Select columns with calculated bioavailability
      names_to = "PPTESTCD",
      values_to = "PPSTRES"
    ) %>%
    mutate(PPSTRESU = "%",
           PPORRESU = "%",
           PPORRES = PPSTRES)

  res_nca$result <- bind_rows(res_nca$result, f_results)

  res_nca
}