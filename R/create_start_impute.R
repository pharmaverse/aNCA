#' Create C0 Impute Column
#'
#' Defines an impute column in the intervals of the PKNCAdata object based on data
#'
#' @param pknca_data A PKNCAdata object containing concentration and dose data.
#' @returns A PKNCAdata object with updated intervals table including start imputation strategies.
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom PKNCA pk.calc.c0
#' @export
#'
#' @examples
#' \dontrun{
#' pknca_data <- create_start_impute(pknca_data)
#' }
create_start_impute <- function(pknca_data) {

  if (nrow(pknca_data$intervals) == 0) {
    warning("No intervals provided. No modification performed")
    return(pknca_data)
  }

  # Define column names
  conc_column <- pknca_data$conc$columns$conc
  time_column <- pknca_data$conc$columns$time
  analyte_column <- pknca_data$conc$columns$groups$group_analyte
  route_column <- pknca_data$dose$columns$route
  duration_column <- pknca_data$dose$columns$duration
  drug_column <- "DRUG"
  conc_group_columns <- unname(unlist(pknca_data$conc$columns$groups))
  dose_group_columns <- unname(unlist(pknca_data$dose$columns$groups))
  group_columns <- unique(c(conc_group_columns, dose_group_columns))

  # Define dose number (DOSNO) if not present in dose data
  if (!"DOSNOA" %in% names(pknca_data$dose$data)) {
    pknca_data$dose$data <- pknca_data$dose$data %>%
      group_by(across(any_of(dose_group_columns))) %>%
      mutate(DOSNO = row_number()) %>%
      ungroup()
  }

  mydata_with_int <- merge(
    x = pknca_data$conc$data %>%
      select(any_of(c(conc_group_columns, conc_column, time_column))),
    y = pknca_data$dose$data %>%
      select(any_of(c(dose_group_columns, route_column,
                      duration_column, "DOSNOA", "DRUG")))
  ) %>%
    merge(pknca_data$intervals) %>%
    filter(!!sym(time_column) >= start, !!sym(time_column) <= end) %>%
    unique()

  # Define dosing drug as analyte if not present
  if (!drug_column %in% colnames(mydata_with_int)) {
    if (analyte_column %in% colnames(mydata_with_int)) {
      drug_column <- analyte_column
    } else {
      mydata_with_int <- mutate(mydata_with_int,
                                PARAM = "A")
      analyte_column <- "PARAM"
      drug_column <- analyte_column
    }
  }

  # Process imputation strategy based on each interval
  pknca_data$intervals <- mydata_with_int %>%
    group_by(across(any_of(c(group_columns, "DOSNOA", "start", "end", "type_interval")))) %>%
    arrange(across(any_of(c(group_columns, time_column)))) %>%
    mutate(
      is.first.dose = DOSNOA == 1,
      is.ivbolus = tolower(!!sym(route_column)) == "intravascular" & !!sym(duration_column) == 0,
      is.analyte.drug = !!sym(analyte_column) == !!sym(drug_column),
      is.possible.c0.logslope = !is.na(pk.calc.c0(conc = !!sym(conc_column),
                                                  time = !!sym(time_column),
                                                  time.dose = start[1],
                                                  method = "logslope"))
    ) %>%
    arrange(
      (!!sym(time_column) - start) < 0,
      (!!sym(time_column) - start)
    ) %>%
    slice(1) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      impute = case_when(
        # Start concentration already present: No imputation (NA)
        !!sym(time_column) == start & !is.na(!!sym(conc_column)) ~ NA,

        # 1st dose with not IV bolus or analyte =! drug : Start concentration is 0
        is.first.dose & (!is.ivbolus | !is.analyte.drug) ~ "start_conc0",

        # Posterior doses not IV bolus or analyte =! drug : Start concentration shifts to predose
        !is.first.dose & (!is.ivbolus | !is.analyte.drug) ~ "start_predose",

        # IV bolus with analyte = drug : Start concentration is log-backextrapolated (if possible)
        is.ivbolus & is.analyte.drug & is.possible.c0.logslope ~ "start_logslope",

        # IV bolus with analyte = drug and not possible logslope: Start concentration is 1st conc
        is.ivbolus & is.analyte.drug ~ "start_c1"
      )
    ) %>%
    ungroup() %>%
    # Select only the columns of interest
    select(any_of(c(names(pknca_data$intervals), "impute")))

  pknca_data

}
