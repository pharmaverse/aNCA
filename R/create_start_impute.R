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
#' adnca <- read.csv(system.file("shiny/data/Dummy_data.csv", package = "aNCA"))
#' pknca_data <- PKNCA_create_data_object(adnca)
#' pknca_data <- create_start_impute(pknca_data)
#' 
create_start_impute <- function(pknca_data) {

  if (nrow(pknca_data$intervals) == 0) {
    warning("No intervals provided. No modification performed")
    return(pknca_data)
  }

  # Define column names
  conc_column <- pknca_data$conc$columns$concentration
  time_column <- pknca_data$conc$columns$time
  analyte_column <- pknca_data$conc$columns$groups$group_analyte
  route_column <- pknca_data$dose$columns$route
  duration_column <- pknca_data$dose$columns$duration
  metabfl_column <- "METABFL"
  conc_group_columns <- unname(unlist(pknca_data$conc$columns$groups))
  dose_group_columns <- unname(unlist(pknca_data$dose$columns$groups))
  group_columns <- unique(c(conc_group_columns, dose_group_columns))

  mydata_with_int <- merge(
    x = pknca_data$conc$data %>%
      select(any_of(c(conc_group_columns, conc_column,
                      time_column, metabfl_column))),
    y = pknca_data$dose$data %>%
      select(any_of(c(dose_group_columns, route_column,
                      duration_column, "DOSNOA")))
  ) %>%
    merge(pknca_data$intervals) %>%
    filter(!!sym(time_column) >= start, !!sym(time_column) <= end) %>%
    unique()

  # Process imputation strategy based on each interval
  pknca_data$intervals <- mydata_with_int %>%
    group_by(across(any_of(c(group_columns, "DOSNOA", "start", "end", "type_interval")))) %>%
    arrange(across(any_of(c(group_columns, time_column)))) %>%
    mutate(
      is.first.dose = DOSNOA == 1,
      is.ivbolus = tolower(!!sym(route_column)) == "intravascular" & !!sym(duration_column) == 0,
      is.metabolite = if (metabfl_column %in% names(.)) !!sym(metabfl_column) == "Y" else FALSE,
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

        # 1st dose with not IV bolus or metabolite : Start concentration is 0
        is.first.dose & (!is.ivbolus | is.metabolite) ~ "start_conc0",

        # Posterior doses not IV bolus or metabolite : Start concentration shifts to predose
        !is.first.dose & (!is.ivbolus | is.metabolite) ~ "start_predose",

        # IV bolus with analyte = drug : Start concentration is log-backextrapolated (if possible)
        is.ivbolus & !is.metabolite & is.possible.c0.logslope ~ "start_logslope",

        # IV bolus with analyte = drug and not possible logslope: Start concentration is 1st conc
        is.ivbolus & !is.metabolite ~ "start_c1"
      )
    ) %>%
    ungroup() %>%
    # Select only the columns of interest
    select(any_of(c(names(pknca_data$intervals), "impute")))

  pknca_data

}
