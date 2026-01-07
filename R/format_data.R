#' Create PK Concentration Dataset
#'
#' This function creates a pharmacokinetic concentration dataset from the provided ADNCA data.
#'

#' @param ADNCA A data frame containing the ADNCA data.
#' @param group_columns A character vector specifying the columns to group by.
#' @param time_column A character string specifying the time column.
#' @param rrlt_column A character string specifying the time since last dose column.
#' @param route_column A character string specifying the route column.
#' @param time_end_column (Optional) A character string specifying the column for end time, used to
#'   calculate `CONCDUR`.
#' @param nca_exclude_reason_columns A character vector specifying the columns to indicate reasons
#'   for excluding records from NCA analysis.
#'
#' @returns A data frame containing the filtered and processed concentration data. The following new
#'   columns are created:
#'   - `DOSNOA`: Sequential dose number within each group, based on time of dose.
#'   - `std_route`: Standardized route, either "intravascular" or "extravascular", derived from the
#'     route column.
#'   - `nca_exclude`: Character column with concatenated exclusion reasons (if
#'     `nca_exclude_reason_columns` is provided), otherwise empty string.
#'   - `CONCDUR`: (Optional) Sampling duration, calculated as `time_end_column - time_column`
#'     if `time_end_column` is provided.
#'
#' @details
#' The function performs the following steps:
#'   - Checks for required columns and data.
#'   - Filters out rows with EVID = 0 and PARAMCD containing "DOSE" (removes dosing data; not CDISC
#'     standard).
#'   - Optionally creates `CONCDUR` if `time_end_column` is provided.
#'   - Optionally creates `nca_exclude` by merging specified exclusion reason columns.
#'   - Creates `DOSNOA` variable: sequential numbers based on time of dose within each group.
#'   - Creates a `std_route` column with PKNCA values "intravascular" or "extravascular" based on
#'     the route column (ROUTE, CDISC: C66729).
#'   - Arranges the data by group_columns and dose time.
#'
#' @examples
#' adnca <- read.csv(system.file("shiny/data/example-ADNCA.csv", package = "aNCA"))
#' conc_data <- format_pkncaconc_data(
#'   ADNCA = adnca,
#'   group_columns = c("STUDYID", "DOSETRT", "USUBJID", "PARAM"),
#'   time_column = "AFRLT",
#'   rrlt_column = "ARRLT",
#'   route_column = "ROUTE"
#' )
#' @import dplyr
#' @export

format_pkncaconc_data <- function(ADNCA,
                                  group_columns,
                                  time_column = "AFRLT",
                                  rrlt_column = "ARRLT",
                                  route_column = "ROUTE",
                                  time_end_column = NULL,
                                  nca_exclude_reason_columns = NULL) {
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

  if (!is.null(time_end_column)) {
    ADNCA[["CONCDUR"]] <- ADNCA[[time_end_column]] - ADNCA[[time_column]]
  }

  if (!is.null(nca_exclude_reason_columns)) {

    ADNCA <- ADNCA %>%
      # Merge all reason columns into one
      mutate(
        nca_exclude = apply(
          select(., any_of(nca_exclude_reason_columns)),
          1,
          function(row) {
            reasons <- row[!is.na(row) & row != ""]
            if (length(reasons) > 0) {
              paste(reasons, collapse = "; ")
            } else {
              ""
            }
          }
        )
      )
  } else {
    ADNCA$nca_exclude <- ""
  }

  #set a tolerance for the arranging to avoid floating point precision issues
  tol <- 0.02

  # Make a pattern to derive PKNCA route from CDISC ROUTE
  intravascular_pattern <- paste0(
    "(INFUS|DRIP|IV|INTRAVEN|IVADMIN|BOLUS|INTRAVASCULAR|INTRA-?ARTERIAL|",
    "INTRACARDIAC|INTRACORONARY)"
  )
  ADNCA %>%
    mutate(browser(), #round to prevent floating point precision issues
      dose_time = round(!!sym(time_column) - !!sym(rrlt_column), 6),
      std_route =  ifelse(
        grepl(intravascular_pattern, gsub("[^[:alnum:]]", "", toupper(!!sym(route_column)))),
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
