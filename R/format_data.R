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

format_pkncaconc_data <- function(ADNCA, group_columns, time_column = "AFRLT") {
  if (nrow(ADNCA) == 0) {
    stop("Input dataframe is empty. Please provide a valid ADNCA dataframe.")
  }

  missing_columns <- setdiff(c(group_columns, time_column), colnames(ADNCA))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  ADNCA %>%
    mutate(conc_groups = interaction(!!!syms(group_columns), sep = "\n")) %>%
    arrange(!!sym(time_column)) %>%
    mutate(TIME = !!sym(time_column)) %>%
    group_by(!!!syms(group_columns)) %>%
    mutate(IX = seq_len(n())) %>%
    ungroup() %>%
    arrange(!!!syms(group_columns))
}

#' Create PK Dose Dataset
#'
#' This function creates a pharmacokinetic dose dataset from the provided concentration data.
#'
#' @param ADNCA_conc A data frame containing the concentration data.
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
#' @param df_dose A PKNCAdose object
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
#'   dose_intervals <- format_pkncadata_intervals(df_dose, params)
#' }
#'
#' @import dplyr
#' @export
format_pkncadata_intervals <- function(pknca_dose,
                                       params =  c("aucinf.obs", "aucint.last", "auclast",
                                                   "cmax", "half.life", "tmax", "lambda.z",
                                                   "lambda.z.n.points", "r.squared",
                                                   "adj.r.squared", "lambda.z.time.first",
                                                   "aucpext.obs", "aucpext.pred", "clast.obs",
                                                   "cl.obs"),
                                       start_from_last_dose = TRUE) {
  if (!inherits(pknca_dose, "PKNCAdose")) {
    stop("Input must be a PKNCAdose object from the PKNCA package.")
  }

  required_columns <- c(unname(unlist(pknca_dose$columns$groups)), pknca_dose$columns$time)
  missing_columns <- setdiff(required_columns, colnames(pknca_dose$data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Based on dose times create a data frame with start and end times
  dose_intervals <- pknca_dose$data %>%
    mutate(start = if (start_from_last_dose) !!sym(pknca_dose$columns$time)
           else !!sym(pknca_dose$columns$time) + !!sym("ARRLT")) %>%
    group_by(!!!syms(unname(unlist(pknca_dose$columns$groups)))) %>%
    arrange(!!sym(pknca_dose$columns$time)) %>%
    mutate(end = lead(as.numeric(!!sym(pknca_dose$columns$time)), default = Inf)) %>%
    ungroup() %>%
    select(any_of(c("start", "end", unname(unlist(pknca_dose$columns$groups)), "DOSNO"))) %>%

    # Create logical columns with the TRUE and as names params argument
    mutate(!!!setNames(rep(TRUE, length(params)), params)) %>%

    # Identify the intervals as the base ones for the NCA analysis
    mutate(type_interval = "main")

  dose_intervals
}

