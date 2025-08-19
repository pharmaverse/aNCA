#' Detect study types
#'
#' This function detects the type of study based on the provided data.
#' @param data The dataset containing the study types to be identified.
#' Assumed to be the output from aNCA formatted concentration data.
#' Must contain the columns `STUDYID`, `DRUG`, `USUBJID`, `PCSPEC`,
#'  `DOSNOA` (created in `format_pkncaconc_data`) and the specified route column.
#' @param route_column A character string specifying the
#'  column name for the route of administration.
#' @param volume_column A character string specifying the
#'  column name for the volume of a sample. Extravascular
#' samples must be written as `extravascular`.
#' Can be set to `NULL` if not applicable.
#'
#' @details
#' The function identifies a possible five different types of studies
#' based on grouping by `STUDYID`, `DRUG`, `USUBJID`, `PCSPEC`, and the route column.
#' The study types are determined as follows:
#'  - "Excretion Data": If the volume column for a group is greater than 0.
#'  - "Single Extravascular Dose": If there is only one dose and TAU is NA,
#'   and the route is extravascular.
#'  - "Single IV Dose": If there is only one dose and TAU is NA,
#'   and the route is not extravascular.
#'  - "Multiple Extravascular Doses": If there are multiple doses (or TAU is available)
#'   and the route is extravascular.
#'  - "Multiple IV Doses": If there are multiple doses (or TAU is available)
#'   and the route is not extravascular.
#'  - If none of these conditions are met, the type is marker as "Unknown".
#'
#' @returns A data frame summarizing the detected study types,
#' including the number of unique subjects (USUBJID) for each type.
#'
#' @importFrom dplyr group_by mutate case_when ungroup
#' @importFrom rlang sym syms
#'
#' @examples
#' sample_data <- data.frame(
#'   STUDYID = "STUDY001",
#'   DRUG = "Drug",
#'   USUBJID = c(
#'     # 1. Single IV Dose subject
#'     "Subj-01", "Subj-01",
#'     # 2. Multiple Extravascular Doses subject (identified by TAU)
#'     "Subj-02", "Subj-02",
#'     # 3. Excretion Data subject (identified by positive volume)
#'     "Subj-03", "Subj-03"
#'   ),
#'   PCSPEC = "PLASMA",
#'   DOSNOA = c(
#'     1, 1,        # Single dose
#'     1, 1,        # Appears as single dose...
#'     1, 1         # Single dose
#'   ),
#'   ROUTE = c(
#'     "INTRAVENOUS", "INTRAVENOUS",
#'     "extravascular", "extravascular",
#'     "INTRAVENOUS", "INTRAVENOUS"
#'   ),
#'   SAMPLE_VOLUME = c(
#'     NA, 0,
#'     NA, 0,
#'     10, 12       # Positive volume indicates excretion
#'   ),
#'   TAU = c(
#'     NA, NA,
#'     24, 24,      # ...but TAU indicates a multiple-dose regimen
#'     NA, NA
#'   )
#' )
#'
#' study_summary <- detect_study_types(
#'   data = sample_data,
#'   route_column = "ROUTE",
#'   volume_column = "SAMPLE_VOLUME"
#' )
#'
#' @export
detect_study_types <- function(data, route_column, volume_column = NULL) {
  full_grouping <- c("STUDYID", "DRUG", "USUBJID", "PCSPEC", route_column)
  summary_grouping <- c("DRUG", "STUDYID", "PCSPEC")

  has_tau <- "TAU" %in% names(data)
  
  # If volume column is not provided, create volume_column and set to NA
  if (is.null(volume_column)) {
    volume_column <- "volume"
    data[[volume_column]] <- NA
  }

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
