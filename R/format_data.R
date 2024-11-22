#' Format Data for Pharmacokinetic Analysis
#'
#' This function formats a data file for pharmacokinetic analysis by converting specific concentration values to numeric and adjusting time based on dose number.
#'
#' @param datafile A data frame containing the raw data to be formatted.
#'
#' @return A data frame with formatted concentration and time values.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Converts concentration values ('AVAL') that are 'BLQ', 'Negative', 'negative', or 'NEGATIVE' to 0.
#'   \item Adjusts the time values ('TIME') based on the dose number ('DOSNO'). If 'DOSNO' is 1, 'TIME' is set to 'AFRLT'; otherwise, it is set to 'TIME'.
#' }
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   formatted_data <- format_data(raw_data)
#' }
#'
#' @import dplyr
#' @export

#Format data for pknca
format_data <- function(datafile) {  
  datafile <- datafile %>% 
    dplyr::mutate(AVAL = as.numeric(ifelse(AVAL %in% 
                                      c("BLQ", "Negative", "negative", "NEGATIVE"),
                                    0, AVAL))
    )%>%
    dplyr::mutate(TIME = ifelse(DOSNO == 1, AFRLT, TIME))

  return(datafile)
}

#' Create PK Concentration Dataset
#'
#' This function creates a pharmacokinetic concentration dataset from the provided ADNCA data.
#'
#' @param ADNCA A data frame containing the ADNCA data.
#' @param analyte A character string specifying the analyte of interest.
#' @param proftype A character string specifying the profile type (not used in the function but kept for consistency).
#'
#' @return A data frame containing the filtered and processed concentration data.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Filters the data for the specified analyte and event ID (EVID == 0).
#'   \item Creates a 'groups' column by concatenating 'USUBJID' and 'DOSNO'.
#'   \item Filters out negative time values.
#'   \item Arranges the data by 'STUDYID', 'USUBJID', 'PCSPEC', 'DOSNO', and 'TIME'.
#'   \item Adds an index column ('IX') within each group.
#' }
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   conc_data <- create_conc(ADNCA, "analyte_name", "profile_type")
#' }
#'
#' @import dplyr
#' @export

#create pknca concentration dataset
create_conc <- function(ADNCA,
                        group_columns,
                        time_column="AFRLT") {
  data <- ADNCA %>%
    dplyr::mutate(conc_groups = interaction(!!!syms(group_columns), sep = "\n")) %>%
    dplyr::arrange(!!sym(time_column)) %>%
    dplyr::mutate(TIME = !!sym(time_column)) %>%
    dplyr::group_by(!!!syms(group_columns)) %>%
    dplyr::mutate(IX = 1:n()) %>% 
    dplyr::ungroup()
}

#' Create PK Dose Dataset
#'
#' This function creates a pharmacokinetic dose dataset from the provided concentration data, including time of dose (TIME) and first (TIME1) and last (TIMELAST) times observed. 
#'
#' @param ADNCA_conc A data frame containing the concentration data.
#'
#' @return A data frame containing the dose data.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Arranges the data by 'USUBJID' and 'DOSNO'.
#'   \item Groups the data by 'USUBJID' and 'DOSNO'.
#'   \item Selects the first row within each group.
#'   \item Converts 'DOSEA' to numeric and retains the 'ROUTE' as 'IQROUTE'.
#' }
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   dose_data <- create_dose(conc_data)
#' }
#'
#' @import dplyr
#' @export

create_dose <- function(df_conc,
                        group_columns,
                        time_column = "AFRLT",
                        since_lastdose_time_column = "ARRLT") {

  df_conc %>%
    dplyr::mutate(TIME = !!sym(time_column) - !!sym(since_lastdose_time_column)) %>% 
    dplyr::group_by(!!!syms(group_columns)) %>%
    dplyr::filter(if (any(!!sym(since_lastdose_time_column) >= 0)) !!sym(since_lastdose_time_column) >= 0 else TRUE) %>%
    dplyr::mutate(TIME1 =  min(!!sym(since_lastdose_time_column)), TIMELAST = max(!!sym(since_lastdose_time_column))) %>% 
    dplyr::slice(1) %>%
    dplyr::ungroup()
}
