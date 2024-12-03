#' Create PK Concentration Dataset
#'
#' This function creates a pharmacokinetic concentration dataset from the provided ADNCA data.
#'
#' @param ADNCA A data frame containing the ADNCA data.
#' @param analyte A character string specifying the analyte of interest.

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
                        time_column = "AFRLT") {
  data <- ADNCA %>%
    dplyr::mutate(conc_groups = interaction(!!!syms(group_columns), sep = "\n")) %>%
    dplyr::arrange(!!sym(time_column)) %>%
    dplyr::mutate(TIME = !!sym(time_column)) %>%
    dplyr::group_by(!!!syms(group_columns)) %>%
    dplyr::mutate(IX = seq_len(n())) %>%
    dplyr::ungroup()
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

create_dose <- function(df_conc,
                        group_columns,
                        time_column = "AFRLT",
                        since_lastdose_time_column = "ARRLT") {

  df_conc %>%
    dplyr::mutate(TIME = !!sym(time_column) - !!sym(since_lastdose_time_column)) %>%
    dplyr::group_by(!!!syms(group_columns)) %>%
    dplyr::arrange(!!sym(since_lastdose_time_column) < 0,
                   !!sym(since_lastdose_time_column)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
}

#' Create Dose Intervals Dataset
#'
#' This function creates a dataset with dose intervals and specified pharmacokinetic parameters.
#'
#' @param df_dose A PKNCAdose object. Default from the app is `mydose`.
#' @param params A character vector specifying the pharmacokinetic parameters to include.
#' @param start_from_last_dose Logical defining if start is at time of last dose or C1.
#'
#' @return A data frame containing the dose intervals and specified pharmacokinetic parameters.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Creates a vector with all pharmacokinetic parameters.
#'   \item Based on dose times, creates a data frame with start and end times.
#'   \item Adds logical columns for each specified parameter.
#' }
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   dose_intervals <- create_dose_intervals(df_dose, params)
#' }
#'
#' @import dplyr
#' @export
create_dose_intervals <- function(mydose = mydose,
                                  params =  c("aucinf.obs", "aucint.last", "auclast", "cmax",
                                              "half.life", "tmax", "lambda.z", "lambda.z.n.points",
                                              "r.squared", "adj.r.squared", "lambda.z.time.first"),
                                  start_from_last_dose = TRUE) {

  # Based on dose times create a data frame with start and end times
  dose_intervals <- mydose$data %>%
    group_by(!!!syms(unname(unlist(mydose$columns$groups)))) %>%
    dplyr::arrange(!!sym(mydose$columns$time)) %>%
    mutate(start = if (start_from_last_dose) !!sym(mydose$columns$time) else !!sym("AFRLT"),
           end = lead(!!sym(mydose$columns$time),
                      default = Inf)) %>%
    ungroup() %>%
    select(start, end, unname(unlist(mydose$columns$groups)), DOSNO) %>%

    # Create logical columns with the TRUE and as names params argument
    mutate(!!!setNames(rep(TRUE, length(params)), params)) %>%

    # Identify the intervals as the base ones for the NCA analysis
    mutate(type_interval = "main")

  return(dose_intervals)
}

