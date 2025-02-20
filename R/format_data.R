#' Create PK Concentration Dataset
#'
#' This function creates a pharmacokinetic concentration dataset from the provided ADNCA data.
#'
#' @param ADNCA A data frame containing the ADNCA data.
#' @param analyte A character string specifying the analyte of interest.

#'
#' @returns A data frame containing the filtered and processed concentration data.
#'
#' @details
#' The function performs the following steps:
#'   - Creates a 'groups' column by concatenating 'USUBJID' and 'DOSNO'.
#'   - Arranges and groups the data by groups_column.
#'   - Adds an index column ('IX') 1:n within each group of length n.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   conc_data <- format_pkncaconc_data(ADNCA, "analyte_name", "profile_type")
#' }
#'
#' @import dplyr
#' @export

format_pkncaconc_data <- function(ADNCA, group_columns,
                                  time_column = "AFRLT", dosno_column = "DOSNO") {
  if (nrow(ADNCA) == 0) {
    stop("Input dataframe is empty. Please provide a valid ADNCA dataframe.")
  }

  missing_columns <- setdiff(c(group_columns, time_column, dosno_column), colnames(ADNCA))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  ADNCA %>%
    mutate(conc_groups = interaction(!!!syms(group_columns), sep = "\n")) %>%
    arrange(!!sym(time_column)) %>%
    mutate(TIME = !!sym(time_column)) %>%
    group_by(!!!syms(group_columns), !!sym(dosno_column)) %>%
    mutate(IX = seq_len(n())) %>%
    ungroup() %>%
    arrange(!!!syms(group_columns))
}

#' Create PK Dose Dataset
#'
#' This function creates a pharmacokinetic dose dataset from the provided concentration data.
#'
#' @param pkncaconc_data A data frame containing the concentration data.
#' @param group_columns A character vector specifying the columns to group by.
#' @param dosno_column A character string specifying the dose number column.
#' @param time_column A character string specifying the time column.
#' @param since_lastdose_time_column A character string specifying the time since last dose column.
#'
#' @returns A data frame containing the dose data.
#'
#' @details
#' The function performs the following steps:
#'   - Arranges and groups the data by group_columns
#'   - Derives the dose time as: time_column - since_lastdose_time_column
#'   - Selects the first row within each group (arranged by since_lastdose_time_column)
#'
#' @import dplyr
#' @export

format_pkncadose_data <- function(pkncaconc_data,
                                  group_columns,
                                  dosno_column = NULL,
                                  time_column = "AFRLT",
                                  since_lastdose_time_column = "ARRLT") {

  # Check: Dataset is not empty
  if (nrow(pkncaconc_data) == 0) {
    stop("Input dataframe is empty. Please provide a valid concentration dataframe.")
  }

  # Check: All necessary columns are present
  required_columns <- c(group_columns, time_column, since_lastdose_time_column)
  missing_columns <- setdiff(required_columns, colnames(pkncaconc_data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Make sure there is a slice of the data by dose number even if the column does not exist
  if (!is.null(dosno_column) && dosno_column %in% names(pkncaconc_data)) {
    group_columns <- c(group_columns, dosno_column)
  } else {
    group_columns <- c(group_columns, "TIME")
  }

  pkncaconc_data %>%
    mutate(TIME = !!sym(time_column) - !!sym(since_lastdose_time_column)) %>%
    group_by(!!!syms(group_columns)) %>%
    arrange(!!sym(since_lastdose_time_column) < 0,
            !!sym(since_lastdose_time_column)) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(!!!syms(group_columns))

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
#'   - Adds logical columns for each specified parameter.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   dose_intervals <- format_pkncadata_intervals(pknca_conc, pknca_dose, params)
#' }
#'
#' @import dplyr
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
  time_dose_seg <- if ("DOSNO" %in% names(pknca_dose$data)) "DOSNO" else "time_dose"

  # Select conc data and for time column give priority to non-predose samples
  sub_pknca_conc <- pknca_conc$data %>%
    select(any_of(c(conc_groups, "AFRLT", "ARRLT", "DOSNO"))) %>%
    arrange(!!!syms(conc_groups), ARRLT < 0, AFRLT) %>%
    ungroup()

  # Select dose data and use its time column as a time of last dose reference
  sub_pknca_dose <- pknca_dose$data %>%
    select(any_of(c(dose_groups,
                    pknca_dose$columns$time, "DOSNO"))) %>%
    rename_with(~ "time_dose", pknca_dose$columns$time)

  # Based on dose times create a data frame with start and end times
  dose_intervals <- left_join(sub_pknca_conc,
            sub_pknca_dose,
            relationship = "many-to-many") %>%

    # Pick 1 per concentration group and dose number
    arrange(!!!syms(conc_groups), ARRLT < 0, AFRLT) %>%
    group_by(!!!syms(c(conc_groups, time_dose_seg))) %>%
    slice(1) %>%
    ungroup() %>%

    # Make start from last dose (pknca_dose) or first concentration (pknca_conc)
    mutate(start = if (start_from_last_dose) time_dose
           else time_dose + !!sym("ARRLT")) %>%
    group_by(!!!syms(conc_groups)) %>%
    arrange(time_dose) %>%

    # Make end based on next dose time (if no more, Inf)
    mutate(end = lead(as.numeric(time_dose), default = Inf)) %>%
    ungroup() %>%
    select(any_of(c("start", "end", conc_groups, "DOSNO"))) %>%

    # Create logical columns with only TRUE for the NCA parameters requested by the user
    mutate(!!!setNames(rep(FALSE, length(all_pknca_params)), all_pknca_params)) %>%
    mutate(across(any_of(params), ~ TRUE, .names = "{.col}")) %>%

    # Prevent any potential attributes associated to the column names
    mutate(across(everything(), ~ {
      column <- .
      attributes(column) <- NULL
      column
    })) %>%

    # Identify the intervals as the base ones for the NCA analysis
    mutate(type_interval = "main")
  
  dose_intervals
}

