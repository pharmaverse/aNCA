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
#'   - Calculates `TIME_DOSE`as the time of dose reference by the PK sample
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
    mutate(conc_groups = interaction(!!!syms(group_columns), sep = "\n")) %>%
    arrange(!!sym(time_column)) %>%
    mutate(TIME = !!sym(time_column)) %>%
    mutate( #round to prevent floating point precision issues
      TIME_DOSE = round(!!sym(time_column) - !!sym(rrlt_column), 6)
    ) %>%
    mutate(std_route = ifelse(
                              grepl("(INFUS|DRIP|IV|INTRAVEN.*|IVADMIN|BOLUS|INTRAVASCULAR)",
                                    gsub("[^[:alnum:]]", "", toupper(!!sym(route_column)))),
                              "intravascular",
                              "extravascular")) %>%
    arrange(!!!syms(group_columns), TIME_DOSE) %>%
    group_by(!!!syms(group_columns)) %>%
    mutate(
      DOSNOA = cumsum(c(TRUE, diff(TIME_DOSE) > tol))
    ) %>%
    arrange(!!!syms(group_columns))
}

#' Create PK Dose Dataset
#'
#' This function creates a pharmacokinetic dose dataset from the provided concentration data.
#'
#' @param pkncaconc_data A data frame containing the concentration data.
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
                                  group_columns) {

  # Check: Dataset is not empty
  if (nrow(pkncaconc_data) == 0) {
    stop("Input dataframe is empty. Please provide a valid concentration dataframe.")
  }

  # Check: All necessary columns are present
  required_columns <- c(group_columns, "TIME_DOSE", "DOSNOA")
  missing_columns <- setdiff(required_columns, colnames(pkncaconc_data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Select unique doses
  pkncaconc_data %>%
    group_by(!!!syms(group_columns), DOSNOA) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(!!!syms(group_columns)) %>%
    # filter out rows with NA in DOSEA column
    filter(!is.na(DOSEA))

}

#' Create Dose Intervals Dataset
#'
#' This function creates a dataset with dose intervals and specified pharmacokinetic parameters.
#'
#' @param pknca_conc A PKNCAdose object containing the concentration data.
#' @param pknca_dose A PKNCAdose object containing the dose data.
#' @param params A character vector specifying the pharmacokinetic parameters to include.
#' @param start_from_last_dose Logical defining if start is at time of last dose or C1.
#'
#' @returns A data frame containing the dose intervals and specified pharmacokinetic parameters.
#'
#' @details
#' The function performs the following steps:
#'   - Creates a vector with all pharmacokinetic parameters.
#'   - Based on dose times, creates a data frame with start and end times.
#'   - If TAU column is present in data, sets last dose end time to start + TAU
#'   - If no TAU column in data, sets last dose end time to the time of last sample
#'   or Inf if single dose data.
#'   - Adds logical columns for each specified parameter.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   dose_intervals <- format_pkncadata_intervals(pknca_conc, pknca_dose, params)
#' }
#'
#' @import dplyr
#' @importFrom stats setNames
#' @export
format_pkncadata_intervals <- function(pknca_conc,
                                       pknca_dose,
                                       params =  c("aucinf.obs", "aucint.last", "auclast",
                                                   "cmax", "half.life", "tmax", "lambda.z",
                                                   "lambda.z.n.points", "r.squared",
                                                   "adj.r.squared", "lambda.z.time.first",
                                                   "aucpext.obs", "aucpext.pred", "clast.obs",
                                                   "cl.obs"),
                                       start_from_last_dose = TRUE) {
  if (!inherits(pknca_conc, "PKNCAconc")) {
    stop("Input pknca_conc must be a PKNCAconc object from the PKNCA package.")
  }

  if (!inherits(pknca_dose, "PKNCAdose")) {
    stop("Input pknca_dose must be a PKNCAdose object from the PKNCA package.")
  }

  required_columns <- c(unname(unlist(pknca_dose$columns$groups)), pknca_dose$columns$time)
  missing_columns <- setdiff(required_columns, colnames(pknca_dose$data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Select relevant group columns
  conc_groups <- unname(unlist(pknca_conc$columns$groups))
  dose_groups <- unname(unlist(pknca_dose$columns$groups))

  # Obtain all possible pknca parameters
  all_pknca_params <- setdiff(names(PKNCA::PKNCA.options()$single.dose.auc),
                              c("start", "end"))

  # Select conc data and for time column give priority to non-predose samples
  sub_pknca_conc <- pknca_conc$data %>%
    select(any_of(c(conc_groups, "AFRLT", "ARRLT", "NCA_PROFILE", "DOSNOA", "TAU"))) %>%
    arrange(!!!syms(conc_groups), ARRLT < 0, AFRLT)

  has_tau <- "TAU" %in% names(sub_pknca_conc)
  is_single_dose <- !has_tau && length(unique(sub_pknca_conc$DOSNOA)) == 1
  # Select dose data and use its time column as a time of last dose reference
  sub_pknca_dose <- pknca_dose$data %>%
    select(any_of(c(dose_groups,
                    pknca_dose$columns$time, "DOSNOA")))

  # Based on dose times create a data frame with start and end times
  dose_intervals <- left_join(sub_pknca_dose,
                              sub_pknca_conc,
                              by = intersect(names(sub_pknca_dose), c(conc_groups, "DOSNOA")),
                              relationship = "many-to-many") %>%

    # Pick 1 per concentration group and dose number
    arrange(!!!syms(conc_groups), ARRLT < 0, AFRLT) %>%
    group_by(!!!syms(c(conc_groups, "DOSNOA"))) %>%
    mutate(max_end = max(AFRLT, na.rm = TRUE)) %>%
    slice(1) %>%
    ungroup() %>%

    # Make start from last dose (pknca_dose) or first concentration (pknca_conc)
    mutate(start = if (start_from_last_dose) TIME_DOSE
           else TIME_DOSE + !!sym("ARRLT")) %>%
    group_by(!!!syms(conc_groups)) %>%
    arrange(TIME_DOSE) %>%

    # Make end based on next dose time (if no more, Tau or last NFRLT)
    mutate(end = if (has_tau) {
      case_when(
        !is.na(lead(TIME_DOSE)) ~ lead(TIME_DOSE),
        TRUE ~ start + TAU
      )
    } else {
      case_when(
        !is.na(lead(TIME_DOSE)) ~ lead(TIME_DOSE),
        is_single_dose ~ Inf,
        TRUE ~ max_end
      )
    }
    ) %>%
    ungroup() %>%
    select(any_of(c("start", "end", conc_groups, "TIME_DOSE", "NCA_PROFILE", "DOSNOA"))) %>%

    # Create logical columns with only TRUE for the NCA parameters requested by the user
    mutate(!!!setNames(rep(FALSE, length(all_pknca_params)), all_pknca_params)) %>%
    mutate(across(any_of(params), ~ TRUE, .names = "{.col}")) %>%
    # Set FALSE for aucint when end = Inf
    mutate(across(starts_with("aucint"), ~ if_else(end == Inf, FALSE, .))) %>%
    # Identify the intervals as the base ones for the NCA analysis
    mutate(type_interval = "main")

  dose_intervals
}

