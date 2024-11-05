process_c0_imputation <- function(params_with_impute_c0, mydata) {
  
  # Define group columns
  group_columns <- unname(unlist(mydata$conc$columns$groups))
  time_column <- mydata$conc$columns$time
  
  # Process c0_rows and assign c0 imputation strategy
  c0_rows <- mydata$conc$data %>%
    group_by(across(all_of(group_columns))) %>%
    arrange(across(all_of(c(group_columns, time_column)))) %>% 
    dplyr::mutate(
      vr_avals = paste0('c(', paste0(AVAL, collapse = ','), ')'),
      vr_times = paste0('c(', paste0(AFRLT, collapse = ','), ')'),
      time_dose = !!sym(time_column) - ARRLT
    ) %>%
    ungroup() %>%
    group_by(across(all_of(group_columns))) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      impute = case_when(
        AFRLT == 0 & !is.na(AVAL) ~ NA,
        DOSNO == 1 & (tolower(ROUTE) == "extravascular" | ADOSEDUR > 0 | ANALYTE != DRUG) ~ "start_conc0",
        DOSNO > 1 & (tolower(ROUTE) == "extravascular" | ADOSEDUR > 0 | ANALYTE != DRUG) ~ "start_predose",
        tolower(ROUTE) == "intravascular" & ADOSEDUR == 0 & ANALYTE == DRUG &
          !is.na(PKNCA::pk.calc.c0(conc = eval(parse(text = vr_avals)), 
                                   time = eval(parse(text = vr_times)), 
                                   time.dose = time_dose[1],
                                   method = 'logslope')) ~ "start_logslope",
        tolower(ROUTE) == "intravascular" & ADOSEDUR == 0 & ANALYTE == DRUG ~ "start_c1"
      )
    ) %>%
    select(any_of(c(group_columns, "impute")))
  
  # Update mydata$intervals
  mydata$intervals <- rbind(
    mydata$intervals %>%
      mutate(across(all_of(params_with_impute_c0), ~ FALSE)) %>%
      mutate(across(all_of(params_without_impute_c0), ~ TRUE)) %>%
      mutate(impute = NA),
    
    mydata$intervals %>%
      left_join(c0_rows, by = group_columns) %>%
      mutate(across(all_of(params_without_impute_c0), ~ FALSE))
  )
  
  return(mydata)
}