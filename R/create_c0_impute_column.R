create_c0_impute <- function(mydata) {

  # Define columns
  conc_column <- mydata$conc$columns$conc
  time_column <- mydata$conc$columns$time
  analyte_column <- mydata$conc$columns$groups$group_analyte
  route_column <- mydata$dose$columns$route
  duration_column <- mydata$dose$columns$duration
  drug_column <- "DRUG"  # At some point should be added at least manually in PKNCA mydata object
  group_columns <- unique(c(unname(unlist(mydata$conc$columns$groups)),
                            unname(unlist(mydata$dose$columns$groups))))

  mydata_with_int <- merge(
    x = mydata$conc$data,
    y = mydata$dose$data[, group_columns]
  ) %>%
    merge(mydata$intervals)

  # Process imputation strategy based on each interval
  new_intervals <- mydata_with_int %>%
    group_by(across(all_of(c(group_columns, "start", "end")))) %>%
    arrange(across(all_of(c(group_columns, time_column)))) %>%
    dplyr::mutate(
      vr_avals = paste0("c(", paste0(!!sym(conc_column), collapse = ","), ")"),
      vr_times = paste0("c(", paste0(!!sym(time_column), collapse = ","), ")")
    ) %>%
    arrange((!!sym(time_column) - start) < 0,
            (!!sym(time_column) - start)) %>%
    slice(1) %>%
    ungroup() %>%
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
          !is.na(PKNCA::pk.calc.c0(conc = eval(parse(text = vr_avals)),
                                   time = eval(parse(text = vr_times)),
                                   time.dose = start[1],
                                   method = "logslope")) ~ "start_logslope",
        tolower(!!sym(route_column)) == "intravascular" &
          !!sym(duration_column) == 0 &
          !!sym(analyte_column) == !!sym(drug_column) ~ "start_c1"
      )
    ) %>%
    # Select only the columns of interest
    select(any_of(c(names(mydata$intervals), "impute")))

  mydata$intervals <- new_intervals

  return(mydata)
}
