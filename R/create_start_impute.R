#' Create C0 Impute Column
#'
#' Defines an impute column in the intervals of the PKNCAdata object based on data
#'
#' @param mydata A PKNCAdata object containing concentration and dose data.
#' @return A PKNCAdata object with updated intervals table including start imputation strategies.
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom PKNCA pk.calc.c0
#' @export
#'
#' @examples
#' \dontrun{
#' mydata <- create_start_impute(mydata)
#' }
create_start_impute <- function(mydata) {

  # Define columns
  conc_column <- mydata$conc$columns$conc
  time_column <- mydata$conc$columns$time
  analyte_column <- mydata$conc$columns$groups$group_analyte
  route_column <- mydata$dose$columns$route
  duration_column <- mydata$dose$columns$duration
  drug_column <- "DRUG"  # TODO: At some point should be mapped manually in the App
  conc_group_columns <- unname(unlist(mydata$conc$columns$groups))
  dose_group_columns <- unname(unlist(mydata$dose$columns$groups))
  group_columns <- unique(c(conc_group_columns, dose_group_columns))

  # Define dose number (DOSNO) if not present in dose data
  if (!"DOSNO" %in% mydata$dose$data) {
    mydata$dose$data <- mydata$dose$data %>%
      group_by(across(all_of(dose_group_columns))) %>%
      mutate(DOSNO = row_number()) %>%
      ungroup()
  }

  mydata_with_int <- merge(
    x = mydata$conc$data %>%
      dplyr::select(any_of(c(conc_group_columns, conc_column, time_column))),
    y = mydata$dose$data %>%
      dplyr::select(any_of(c(dose_group_columns, route_column,
                             duration_column, "DOSNO", "DRUG")))
  ) %>%
    merge(mydata$intervals)

  # Define dosing drug as analyte if not present
  if (!drug_column %in% colnames(mydata_with_int)) {
    if (analyte_column %in% colnames(mydata_with_int)) {
      drug_column <- analyte_column
    } else {
      mydata_with_int <- dplyr::mutate(mydata_with_int,
                                       ANALYTE = "A")
      analyte_column <- "ANALYTE"
      drug_column <- analyte_column
    }
  }

  # Process imputation strategy based on each interval
  mydata$intervals <- mydata_with_int %>%
    group_by(across(all_of(c(group_columns, "DOSNO", "start", "end")))) %>%
    arrange(across(all_of(c(group_columns, time_column)))) %>%
    dplyr::mutate(is.possible.c0.logslope = !is.na(pk.calc.c0(conc = !!sym(conc_column),
                                                              time = !!sym(time_column),
                                                              time.dose = start[1],
                                                              method = "logslope"))
    ) %>%
    arrange((!!sym(time_column) - start) < 0,
            (!!sym(time_column) - start)) %>%
    slice(1) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      impute = case_when(
        !!sym(time_column) == start & !is.na(!!sym(conc_column)) ~ NA,
        DOSNO == 1 &
          (tolower(!!sym(route_column)) == "extravascular" |
             !!sym(duration_column) > 0 |
             !!sym(analyte_column) != !!sym(drug_column)) ~ "start_conc0",
        DOSNO > 1 &
          (tolower(!!sym(route_column)) == "extravascular" |
             !!sym(duration_column) > 0 |
             !!sym(analyte_column) != !!sym(drug_column)) ~ "start_predose",
        tolower(!!sym(route_column)) == "intravascular" &
          !!sym(duration_column) == 0 &
          !!sym(analyte_column) == !!sym(drug_column) &
          is.possible.c0.logslope ~ "start_logslope",
        tolower(!!sym(route_column)) == "intravascular" &
          !!sym(duration_column) == 0 &
          !!sym(analyte_column) == !!sym(drug_column) ~ "start_c1"
      )
    ) %>%
    # Select only the columns of interest
    select(any_of(c(names(mydata$intervals), "impute")))

  return(mydata)
}
