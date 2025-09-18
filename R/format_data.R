#' Create PK Concentration Dataset
#'
#' This function creates a pharmacokinetic concentration dataset from the provided ADNCA data.
#'
#' @param ADNCA A data frame containing the ADNCA data.
#' @param group_columns A character vector specifying the columns to group by.
#' @param time_column A character string specifying the time column.
#' @param rrlt_column A character string specifying the time since last dose column.
#' @param route_column A character string specifying the route column.
#'
#' @returns A data frame containing the filtered and processed concentration data.
#'
#' @details
#' The function performs the following steps:
#'   - Checks for required columns and data.
#'   - Filters out rows with EVID = 0 and PARAMCD containing "DOSE"
#'   (dosing data- not CDISC standard)
#'   - Creates `DOSNOA` variable, sequential numbers based on time of dose
#'   - Adds a 'std_route' column taking values "intravascular" or "extravascular".
#'   - Arranges the data by group_columns.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   conc_data <- format_pkncaconc_data(ADNCA,
#'                                      group_columns,
#'                                      "AFRLT",
#'                                      "ROUTE")
#' }
#'
#' @import dplyr
#' @export

format_pkncaconc_data <- function(ADNCA,
                                  group_columns,
                                  time_column = "AFRLT",
                                  rrlt_column = "ARRLT",
                                  route_column = "ROUTE") {
  if (nrow(ADNCA) == 0) {
    stop("Input dataframe is empty. Please provide a valid ADNCA dataframe.")
  }

  missing_columns <- setdiff(c(group_columns, time_column,
                               rrlt_column), colnames(ADNCA))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # filter out dose data if present
  if ("EVID" %in% colnames(ADNCA)) {
    ADNCA <- ADNCA %>%
      filter(EVID == 0)
  }

  if ("PARAMCD" %in% colnames(ADNCA)) {
    ADNCA <- ADNCA %>%
      filter(!grepl("DOSE", PARAMCD, ignore.case = TRUE))
  }

  #set a tolerance for the arranging to avoid floating point precision issues
  tol <- 0.02

  ADNCA %>%
    mutate( #round to prevent floating point precision issues
      dose_time = round(!!sym(time_column) - !!sym(rrlt_column), 6),
      std_route = ifelse(
        grepl("(INFUS|DRIP|IV|INTRAVEN.*|IVADMIN|BOLUS|INTRAVASCULAR)",
              gsub("[^[:alnum:]]", "", toupper(!!sym(route_column)))),
        "intravascular",
        "extravascular"
      )
    ) %>%
    arrange(!!!syms(group_columns), dose_time) %>%
    group_by(!!!syms(group_columns)) %>%
    mutate(
      DOSNOA = cumsum(c(TRUE, diff(dose_time) > tol))
    ) %>%
    ungroup() %>%
    select(-dose_time)
}

#' Create PK Dose Dataset
#'
#' This function creates a pharmacokinetic dose dataset from the provided concentration data.
#'
#' @param pkncaconc_data A data frame containing the concentration data.
#' @param time_column A character string specifying the time from first dose column.
#' @param rrlt_column A character string specifying the time since last dose column.
#' @param group_columns A character vector specifying the columns to group by.
#'
#' @returns A data frame containing the dose data.
#'
#' @details
#' The function performs the following steps:
#'   - Arranges and groups the data by group_columns
#'   - Selects the first row within each group (arranged by DOSNOA- a variable created
#'   in `format_pkncaconc_data`)
#'
#' Note*: This function is designed to work with the output of `format_pkncaconc_data`.
#'
#' @import dplyr
#' @export

format_pkncadose_data <- function(pkncaconc_data,
                                  time_column = "AFRLT",
                                  rrlt_column = "ARRLT",
                                  group_columns) {

  # Check: Dataset is not empty
  if (nrow(pkncaconc_data) == 0) {
    stop("Input dataframe is empty. Please provide a valid concentration dataframe.")
  }

  # Check: All necessary columns are present
  required_columns <- c(group_columns, "DOSNOA")
  missing_columns <- setdiff(required_columns, colnames(pkncaconc_data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Select unique doses
  pkncaconc_data %>%
    group_by(!!!syms(group_columns), DOSNOA) %>%
    slice(1) %>%
    ungroup() %>%
    mutate( #round to prevent floating point precision issues
      !!sym(time_column) := round(!!sym(time_column) - !!sym(rrlt_column), 6)
    ) %>%
    # filter out rows with NA in DOSEA column
    filter(!is.na(DOSEA)) %>%
    arrange(across(all_of(c(group_columns, time_column))))

}
