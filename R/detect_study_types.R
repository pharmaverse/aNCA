#' Detect study types in data
#' This function detects the type of study based on the provided data.
#' @param data A PKNCA object containing the concentration and dose data.
#' @param grouping A character vector of column names to group by in the data.
#'      Must include a variable to separate the specimen types, eg `PCSPEC`.
#'      Default will be `c("DRUG", "USUBJID", "PCSPEC")`.
#' 
#' @returns A list of the unique study types identified.
#' 
#' @importFrom dplyr group_by mutate case_when ungroup
#' @importFrom rlang sym syms
#' 
#' @export
detect_study_types <- function(data, grouping = c("DRUG", "USUBJID", "PCSPEC")) {
browser()
  route_column <- data$dose$columns$route
  volume_column <- data$conc$columns$volume
  
  has_tau <- "TAU" %in% names(data$conc$data)
  
  study_data <- data$conc$data %>%
    #group by grouping and route columne
    group_by( !!!syms(grouping), !!sym(route_column)) %>%
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