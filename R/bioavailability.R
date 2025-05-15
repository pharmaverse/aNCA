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
#' @param f_aucs A character vector of the comparing AUC parameter/s including
#' the prefix f_ (e.g., `c("f_aucinf.obs", "f_auclast")`).
#'
#' @returns A data frame with calculated absolute bioavailability values (`FABS_`)
#'   for individual subjects where IV data is available. If IV data is missing,
#'  it estimates bioavailability using the mean IV AUC for that grouping.
#'
#' @importFrom dplyr bind_rows filter full_join group_by left_join mutate select summarize
#' @importFrom purrr reduce
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
#'
#' @export
pknca_calculate_f <- function(res_nca, f_aucs) {

  # Extract and clean AUC selection
  auc_vars <- gsub("^f_", "", f_aucs)

  # Check if the comparing AUC parameters are available in the PKNCA results
  available_pptestcd <- unique(res_nca$result$PPTESTCD)
  if (!any(available_pptestcd %in% auc_vars)) {
    if (length(f_aucs) > 0) {
      warning(
        paste0(
          "No AUC extracted from f_aucs available in res_nca (PPTESTCD): ",
          paste(auc_vars, collapse = ", ")
        )
      )
    }
    return(NULL)
  }

  # Extract required columns
  route_col <- res_nca$data$dose$columns$route
  dose_col <- res_nca$data$dose$columns$dose
  conc_group_cols <- PKNCA::getGroups(res_nca$data$conc) %>%
    names()
  dose_group_cols <- PKNCA::getGroups(res_nca$data$dose) %>%
    names()

  # Extract dose information (route and dose)
  dose_info <- res_nca$data$dose$data

  res_nca$result %>%

    # Filter AUC parameters requested for bioavailability calculations
    filter(PPTESTCD %in% auc_vars) %>%
    left_join(
      dose_info,
      by = intersect(names(.), names(dose_info))
    ) %>%

    # Force all AUC values to be in the same unit if possible by get_conversion_factor
    group_by(PPTESTCD, !!!syms(setdiff(conc_group_cols, "USUBJID"))) %>%
    mutate(
      conv_factor = get_conversion_factor(PPORRESU, PPORRESU[1]),
      PPORRES = ifelse(!is.na(conv_factor), PPORRES * conv_factor, PPORRES),
      PPORRESU = ifelse(!is.na(conv_factor), PPORRESU[1], PPORRESU)
    ) %>%
    ungroup() %>%

    # Pivot and calculate by group mean AUC and dose values
    rename(
      vals = PPORRES,
      Dose = any_of(dose_col)
    ) %>%
    pivot_wider(
      names_from = any_of(route_col),
      values_from = c(vals, Dose)
    ) %>%

    # Calculate AUC dose normalized values for each profile (intravascular)
    mutate(
      AUCdn_IV_prof = PKNCA::pk.calc.dn(vals_intravascular, Dose_intravascular)
    ) %>%

    # Mean AUC dose normalized values by subject (intravascular)
    group_by(PPTESTCD, !!!syms(conc_group_cols), PPORRESU) %>%
    mutate(
      Mean_AUCdn_IV_subj = mean(AUCdn_IV_prof, na.rm = TRUE)
    ) %>%

    # Mean AUC dose normalized values by cohort (intravascular)
    group_by(PPTESTCD, !!!syms(setdiff(conc_group_cols, "USUBJID")), start, end, PPORRESU) %>%
    mutate(
      Mean_AUCdn_IV_coh = mean(AUCdn_IV_prof, na.rm = TRUE)
    ) %>%

    # Calculate F using group mean values when individual is not present for both routes
    ungroup() %>%
    mutate(
      AUCdn_IV = case_when(
        !is.na(AUCdn_IV_prof) ~ AUCdn_IV_prof,
        !is.na(Mean_AUCdn_IV_subj) ~ Mean_AUCdn_IV_subj,
        !is.na(Mean_AUCdn_IV_coh) ~ Mean_AUCdn_IV_coh,
        TRUE ~ NA
      ),
      PPORRES = PKNCA::pk.calc.f(
        1, AUCdn_IV, # The AUC is already dose normalized for IV
        Dose_extravascular, vals_extravascular
      ) * 100,
      # Maintain the PKNCA results format
      PPTESTCD = paste0("f_", PPTESTCD),
      PPTEST = paste0("Absolute Bioavailability (", PPTESTCD, ")"),
      exclude = case_when(
        is.na(vals_extravascular) & !is.na(vals_intravascular) ~
          "", # Not calculated because it is an IV record
        is.na(vals_extravascular) ~
          paste0(gsub("f_", "", PPTESTCD), " not available"),
        !is.na(AUCdn_IV_prof) ~ "",
        !is.na(Mean_AUCdn_IV_subj) ~ "Mean AUC.dn IV for the subject was used",
        !is.na(Mean_AUCdn_IV_coh) ~ "Mean AUC.dn IV for the cohort was used",
        TRUE ~ "Bioavailability: No individual, subject or cohort IV records to compare with"
      ),
      PPORRESU = "%",
      PPSTRES = PPORRES,
      PPSTRESU = "%",
      type_interval = "main"
    ) %>%
    select(any_of(c(names(res_nca$result))))
}

#' Calculate bioavailability with pivoted output
#'
#' This function calculates bioavailability (F) based on AUC (Area Under Curve) data
#' extracted from `res_nca`. It computes individual bioavailability
#' where IV and EX data are available for a subject. If IV data is missing, it estimates
#' bioavailability using the mean IV values for that grouping. The output is pivoted
#' such that each row represents all main results summarized for each profile in each
#' subject. Columns are assumed to be in `%` units even if not explicitly stated.
#'
#' @inheritParams pknca_calculate_f
#'
#' @returns A pivoted data frame with bioavailability calculations (`f_aucinf`, `f_auclast`, etc.)
#'   for individual subjects where IV data is available. If IV data is missing for the subject,
#'   the mean IV AUC for that group is used instead. Variables are assumed to be in `%` units.
#'
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#'
#' @export
calculate_f <- function(res_nca, f_aucs) {
  pknca_result <- pknca_calculate_f(res_nca, f_aucs)
  res_nca$result <- pknca_result %>%
    mutate(PPSTRESU = "")
  pivot_wider_pknca_results(res_nca) %>%
    select(any_of(c(
      names(PKNCA::getGroups(res_nca)),
      "end",
      paste0(f_aucs),
      "Exclude"
    )))
}

#' Add bioavailability results to PKNCA results
#'
#' @inheritParams pknca_calculate_f
#' @returns A PKNCA result object which results data frame contains added the
#' bioavailability calculations requested based on the AUCs provided in `f_aucs`.
#'
#' @importFrom dplyr left_join
add_f_to_pknca_results <- function(res_nca, f_aucs) {
  f_results <- pknca_calculate_f(res_nca, f_aucs)
  res_nca$result <- bind_rows(res_nca$result, f_results)
  res_nca
}
