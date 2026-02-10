#' Create C0 Impute Column
#'
#' Defines an impute column in the intervals of the PKNCAdata object based on data
#'
#' @param pknca_data A PKNCAdata object containing concentration and dose data.
#' @returns A PKNCAdata object with updated intervals table including start imputation strategies.
#' If the intervals are ambigous and can refer to multiple concentration groups it will define them
#' to choose the proper imputation for each.
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom PKNCA pk.calc.c0
#' @export
#'
#' @examples
#' adnca <- read.csv(system.file("shiny/data/example-ADNCA.csv", package = "aNCA"))
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
  time_dose_column <- pknca_data$dose$columns$time
  metabfl_column <- "METABFL"
  conc_group_columns <- unname(unlist(pknca_data$conc$columns$groups))
  dose_group_columns <- unname(unlist(pknca_data$dose$columns$groups))
  group_columns <- unique(c(conc_group_columns, dose_group_columns))
  nca_excl_column <- pknca_data$conc$columns$exclude

  mydata_with_int <- merge(
    x = pknca_data$conc$data %>%
      filter(!!sym(nca_excl_column) %in% c("", NA_character_)) %>%
      select(any_of(c(conc_group_columns, conc_column,
                      time_column, metabfl_column))),
    y = pknca_data$dose$data %>%
      select(
        any_of(c(
          dose_group_columns, route_column,
          duration_column, "DOSNOA", time_dose_column
        ))
      ) %>%
      rename(TIME_DOSE = !!sym(time_dose_column))
      ,
  ) %>%
    merge(
      pknca_data$intervals %>%
        # Each interval operation has to be treated later independently
        # to prevent issues in is.possible.c0.logslope
        mutate(INT_ROWID = row_number())
    ) %>%
    filter(
      !!sym(time_column) >= start,
      !!sym(time_column) <= end,
      TIME_DOSE <= start
    ) %>%
    unique()

  # Process imputation strategy based on each interval
  pknca_data$intervals <- mydata_with_int %>%
    # Consider by interval (calculation) and concentration group (parameter, specimen)
    group_by(INT_ROWID, "TIME_DOSE", !!!syms(intersect(conc_group_columns, names(.)))) %>%
    arrange(across(any_of(c("INT_ROWID", group_columns, time_column, "TIME_DOSE")))) %>%
    # In case the intervals were defined ambiguously to generate multiple results
    # force them to now be specific so start_impute is also interval specific
    slice_max(TIME_DOSE) %>%
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
        !!sym(time_column) == start & !is.na(!!sym(conc_column)) ~ NA_character_,

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
    select(any_of(c("start", "end", group_vars(pknca_data), "impute", names(pknca_data$intervals))))

  pknca_data

}
