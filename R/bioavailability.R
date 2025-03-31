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

  #check if selected_aucs are available
  if (is.null(selected_aucs) || length(selected_aucs) == 0) {
    return(NULL)
  }

  # Extract and clean AUC selection
  auc_vars <- gsub("^f_", "", selected_aucs)

  # Extract required columns
  route_col <- res_nca$data$dose$columns$route
  dose_col <- res_nca$data$dose$columns$dose

  # Extract ID groups
  id_groups <- res_nca$data$conc$columns$groups %>%
    purrr::list_c() %>%
    append("DOSNO") %>%
    purrr::keep(~ !is.null(.) && . != "DRUG" && length(unique(res_nca$data$conc$data[[.]])) > 1)

  # Filter and transform AUC data
  auc_data <- res_nca$result %>%
    filter(PPTESTCD %in% auc_vars) %>%
    select(USUBJID, PPTESTCD, PPORRES, all_of(id_groups)) %>%
    pivot_wider(names_from = PPTESTCD, values_from = PPORRES)

  # Extract dose information
  dose_info <- res_nca$data$dose$data %>%
    select(all_of(c(id_groups, route_col, dose_col)), USUBJID) %>%
    distinct()

  # Merge dose information with AUC data
  auc_data <- auc_data %>%
    inner_join(dose_info, by = id_groups) %>%
    rename(Route = all_of(route_col), Dose = all_of(dose_col))

  results_list <- list()

  for (auc_type in auc_vars) {

    data <- auc_data %>%
      mutate(grouping = apply(select(., all_of(id_groups), -USUBJID), 1, paste, collapse = " "))

    # Separate IV and EX data
    iv_data <- data %>%
      filter(tolower(Route) == "intravascular") %>%
      rename(AUC_IV = !!sym(auc_type), Dose_IV = Dose, Grouping_IV = grouping) %>%
      select(USUBJID, Grouping_IV, AUC_IV, Dose_IV)

    ex_data <- data %>%
      filter(tolower(Route) == "extravascular") %>%
      rename(AUC_EX = !!sym(auc_type), Dose_EX = Dose, Grouping_EX = grouping) %>%
      select(USUBJID, Grouping_EX, AUC_EX, Dose_EX)

    # Merge IV and EX by USUBJID
    merged_data <- left_join(ex_data, iv_data, by = "USUBJID")

    # Compute bioavailability for individuals (F)
    individual_data <- merged_data %>%
      filter(!is.na(AUC_IV) & !is.na(Dose_IV)) %>%
      mutate(!!paste0("f_", auc_type) := (pk.calc.f(Dose_IV, AUC_IV, Dose_EX, AUC_EX)) * 100)

    # Compute mean IV AUC for missing IV subjects
    mean_iv <- iv_data %>%
      group_by(Grouping_IV) %>%
      summarize(Mean_AUC_IV = mean(AUC_IV, na.rm = TRUE),
                Mean_Dose_IV = mean(Dose_IV, na.rm = TRUE),
                .groups = "drop")

    ex_without_match <- merged_data %>%
      filter(is.na(AUC_IV) | is.na(Dose_IV)) %>%
      mutate(!!paste0("f_", auc_type)
             := (pk.calc.f(mean_iv$Mean_Dose_IV, mean_iv$Mean_AUC_IV, Dose_EX, AUC_EX)) * 100)

    # Combine results
    auc_results <- bind_rows(
      individual_data %>% select(USUBJID, Grouping_EX, Grouping_IV,
                                 !!paste0("f_", auc_type)),
      ex_without_match %>% select(USUBJID, Grouping_EX, Grouping_IV,
                                  !!paste0("f_", auc_type))
    )

    results_list[[auc_type]] <- auc_results
  }

  purrr::reduce(results_list, full_join, by = c("USUBJID", "Grouping_EX", "Grouping_IV")) %>%
    select(-Grouping_IV)
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
#'
#' @export
PKNCA_add_F <- function(res_nca, bioavailability) { # nolint: object_name_linter

  if (is.null(bioavailability)) {
    return(res_nca)
  }
  # Extract ID groups
  id_groups <- res_nca$data$conc$columns$groups %>%
    purrr::list_c() %>%
    append("DOSNO") %>%
    purrr::keep(~ !is.null(.) && . != "DRUG" &&
                  length(unique(res_nca$data$conc$data[[.]])) > 1)

  # Pivot results data
  subj_data <- res_nca$result %>%
    select(-starts_with("PP"), -exclude) %>%
    distinct()

  # Create bioavailability data in resnca format
  f_results <- subj_data %>%
    mutate(Grouping_EX = apply(select(., all_of(id_groups), -USUBJID),
                               1, paste, collapse = " ")) %>%
    left_join(bioavailability, by = c("USUBJID", "Grouping_EX")) %>%
    select(-Grouping_EX) %>%
    pivot_longer(
      cols = starts_with("f_"),  # Select columns with calculated bioavailability
      names_to = "PPTESTCD",
      values_to = "PPSTRES"
    ) %>%
    mutate(PPSTRESU = "%")

  res_nca$result <- bind_rows(res_nca$result, f_results)

  res_nca
}
