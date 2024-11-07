create_c0_impute <- function(mydata) {

  # Define group columns
  group_columns <- unname(unlist(mydata$dose$columns$groups))
  conc_column <- mydata$conc$columns$conc
  time_column <- mydata$conc$columns$time
  common_cols <- intersect(unlist(mydata$conc$columns$groups),
                           unlist(mydata$dose$columns$groups))

  mydata_conc <- merge(
    x = mydata$conc$data,
    y = mydata$dose$data[, common_cols] %>%
      dplyr::mutate(time_dose = mydata$dose$data[[time_column]])
  )

  # Define parameters that need to include c0 imputations
  all_params <- suppressWarnings(apply(mydata$intervals, MARGIN = 2, function(col) any(col)))
  all_params <- names(all_params[!is.na(all_params) & all_params])
  params_c0 <- setdiff(all_params, c("half.life", "cmax", "tmax", "cmax.dn",
                                     "ctrough", "min.hl.points", "min.hl.r.squared"))

  # Process c0_rows and assign c0 imputation strategy
  c0_rows <- mydata_conc %>%
    group_by(across(all_of(group_columns))) %>%
    arrange(across(all_of(c(group_columns, time_column)))) %>%
    dplyr::mutate(
      vr_avals = paste0("c(", paste0(!!sym(conc_column), collapse = ","), ")"),
      vr_times = paste0("c(", paste0(!!sym(time_column), collapse = ","), ")"),
      time_dose = !!sym(time_column) - ARRLT
    ) %>%
    ungroup() %>%
    group_by(across(all_of(group_columns))) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      impute = case_when(
        AFRLT == 0 & !is.na(AVAL) ~ NA,
        DOSNO == 1 &
          (tolower(ROUTE) == "extravascular" |
             ADOSEDUR > 0 |
             ANALYTE != DRUG) ~ "start_conc0",
        DOSNO > 1 &
          (tolower(ROUTE) == "extravascular" |
             ADOSEDUR > 0 |
             ANALYTE != DRUG) ~ "start_predose",
        tolower(ROUTE) == "intravascular" & ADOSEDUR == 0 & ANALYTE == DRUG &
          !is.na(PKNCA::pk.calc.c0(conc = eval(parse(text = vr_avals)),
                                   time = eval(parse(text = vr_times)),
                                   time.dose = time_dose[1],
                                   method = "logslope")) ~ "start_logslope",
        tolower(ROUTE) == "intravascular" & ADOSEDUR == 0 & ANALYTE == DRUG ~ "start_c1"
      )
    ) %>%
    select(any_of(c(group_columns, "impute")))

  # Update mydata$intervals
  mydata$intervals <- rbind(
    mydata$intervals %>%
      mutate(across(all_of(params_c0), ~ FALSE)) %>%
      mutate(impute = NA),

    mydata$intervals %>%
      left_join(c0_rows, by = group_columns) %>%
      mutate(across(all_of(params_c0), ~ TRUE))
  )

  return(mydata)
}
