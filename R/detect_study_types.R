#' Detect study types in data
#' This function detects the type of study based on the provided data.
#' @param data The dataset containing the study types to be identified.
#' @param route_column A character string specifying the column name for the route of administration.
#' @param volume_column A character string specifying the column name for the volume of distribution.
#' @param grouping A character vector of column names to group by in the data.
#'      Must include a variable to separate the specimen types, eg `PCSPEC`.
#'      Default will be `c("DRUG", "USUBJID", "PCSPEC")`.
#' 
#' @returns A list of the unique study types identified.
#' 
#' @importFrom dplyr group_by mutate case_when ungroup
#' @importFrom rlang sym syms
#' 
#' @example
#' # \dontrun{
#' study_types <- detect_study_types(data)
#' 
#' 
#' @export
detect_study_types <- function(data, route_column, volume_column,
                               grouping = c("DRUG", "USUBJID", "PCSPEC")) {

  has_tau <- "TAU" %in% names(data)
  
  study_data <- data %>%
    #group by grouping and route columne
    group_by(!!!syms(grouping), !!sym(route_column)) %>%
    # Only one dosnoa and has_tau is FALSE or TAU column is NA
    mutate(is_one_dose = length(unique(DOSNOA)) == 1 && (!has_tau || all(is.na(get("TAU")))),
           is_extravascular = !!sym(route_column) == "extravascular",
           is_excretion = (!is.na(!!sym(volume_column)) & !!sym(volume_column) > 0))
  
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
    pull(Type) %>%
    unique()
}