#' Detect study types in data
#' This function detects the type of study based on the provided data.
#' @param data The dataset containing the study types to be identified.
#' Assumed to be the output from aNCA formatted concentration data.
#' Must contain the columns `STUDYID`, `DRUG`, `USUBJID`, `PCSPEC`, `DOSNOA` and the specified route column.
#' @param route_column A character string specifying the column name for the route of administration.
#' @param volume_column A character string specifying the column name for the volume of a sample.
#' 
#' @details
#' The function identifies a possible five different types of studies
#' based on grouping by `STUDYID`, `DRUG`, `USUBJID`, `PCSPEC`, and the specified route column.
#' The study types are determined as follows:
#'  - "Excretion Data": If the volume column for a group is greater than 0.
#'  - "Single Extravascular Dose": If there is only one dose and TAU is NA, and the route is extravascular.
#'  - "Single IV Dose": If there is only one dose and TAU is NA, and the route is not extravascular.
#'  - "Multiple Extravascular Doses": If there are multiple doses (or TAU is available) and the route is extravascular.
#'  - "Multiple IV Doses": If there are multiple doses (or TAU is available) and the route is not extravascular.
#' If none of these conditions are met, the type is marker as "Unknown".
#' 
#' 
#' @returns A data frame summarizing the detected study types,
#' including the number of unique subjects (USUBJID) for each type.
#' 
#' @importFrom dplyr group_by mutate case_when ungroup
#' @importFrom rlang sym syms
#' 
#' @example
#' # \dontrun{
#' 
#' 
#' 
#' @export
detect_study_types <- function(data, route_column, volume_column) {
  browser()
  full_grouping = c("STUDYID", "DRUG", "USUBJID", "PCSPEC", route_column)
  summary_grouping = c("DRUG", "STUDYID", "PCSPEC")
  
  has_tau <- "TAU" %in% names(data)
  
  study_data <- data %>%
    #group by grouping and route columne
    group_by(!!!syms(full_grouping)) %>%
    # Only one dosnoa and has_tau is FALSE or TAU column is NA
    mutate(is_one_dose = length(unique(DOSNOA)) == 1 && (!has_tau || all(is.na(get("TAU")))),
           is_extravascular = !!sym(route_column) == "extravascular",
           is_excretion = (!is.na(!!sym(volume_column)) & !!sym(volume_column) > 0)) %>%
    ungroup()
  
  # Identify unique combinations of study types
  study_types <- study_data %>%
    mutate(Type = case_when(
      is_excretion ~ "Excretion Data",
      is_one_dose & is_extravascular ~ "Single Extravascular Dose",
      is_one_dose & !is_extravascular ~ "Single IV Dose",
      !is_one_dose & is_extravascular ~ "Multiple Extravascular Doses",
      !is_one_dose & !is_extravascular ~ "Multiple IV Doses",
      TRUE ~ "Unknown"
    )) %>%
    #summarise each unique Type and group with number of USUBJID
    group_by(across(all_of(summary_grouping)), Type) %>%
    summarise(USUBJID_Count = n_distinct(USUBJID), .groups = "drop")
}