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
  datafile = datafile %>% 
    mutate(AVAL = as.numeric(ifelse(AVAL %in% 
                                          c('BLQ', 'Negative', 'negative', 'NEGATIVE'),
                                        0, AVAL))
    )%>%  
    mutate(TIME = ifelse(DOSNO == 1, AFRLT, TIME))
  
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
create_conc <- function(ADNCA, analyte, proftype) {
  data <- ADNCA %>%  
    filter(ANALYTE == analyte,
           EVID == 0) %>%  
    # mutate(TIME=ifelse(TIME<0,0,TIME),
    #        AVAL=ifelse(TIME==0,0,AVAL)) %>% #make the conc at time 0, 0
    # distinct()  %>%
    mutate(groups=paste0(USUBJID, ', ', DOSNO)) %>%
    filter(TIME>=0) %>% 
    arrange(STUDYID, USUBJID, PCSPEC, DOSNO, TIME) %>%
    group_by(STUDYID, USUBJID, PCSPEC, DOSNO) %>%
    mutate(IX=1:n())
}

#' Create PK Dose Dataset
#'
#' This function creates a pharmacokinetic dose dataset from the provided concentration data.
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

create_dose <- function(ADNCA_conc) {
  
  ADNCA_conc %>%
    arrange(USUBJID, DOSNO) %>%
    group_by(USUBJID, DOSNO) %>%
    slice(1) %>%
    mutate(DOSEA=as.numeric(DOSEA),
           IQROUTE = ROUTE) 
}
