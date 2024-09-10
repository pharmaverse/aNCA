# If needed, derive concentrations at TIME=0 and add them to the dataset
impute_c0 = function(adnca_conc){
  c0_rows = adnca_conc  %>% 
    filter(TIME>=0, DOSNO==1, !is.na(AVAL))  %>% 
    group_by(USUBJID, PCSPEC, ANALYTE, IQROUTE)  %>% 
    mutate(vr_avals = paste0('c(',paste0(AVAL, collapse=','),')'),
           vr_times = paste0('c(',paste0(TIME, collapse=','),')'),
    )  %>% 
    slice(1) %>% 
    ungroup()  %>%
    
    # Delete this column after the merge with tolerate unneded columns
    mutate(ADOSEDUR= if(!'ADOSEDUR' %in% names(adnca_conc)) NDOSEDUR else ADOSEDUR) %>% 
    ######
    
    mutate(AVAL = case_when(
      TIME == 0 & !is.na(AVAL) ~ AVAL,
      IQROUTE != 'INTRAVASCULAR' | ADOSEDUR > 0 | ANALYTE!=DRUG ~ 0,   # Metabolites or non-IV bolus cases have C0=0
      all(diff(eval(parse(text=vr_avals)))>0) ~ PKNCA::pk.calc.c0(eval(parse(text=vr_avals)), eval(parse(text=vr_times)), method = 'logslope'),
      TIME <= 5/60 ~ AVAL,
      T ~ NA_real_
    ))  %>% 
    mutate(TIME=0)  %>% 
    select(any_of(c(names(adnca_conc))))
  
  View(c0_rows)
  # Add the C0 rows to the dataset (if already present eliminate duplicates)
  adnca_conc = bind_rows(adnca_conc, c0_rows)  %>%  
    unique() %>% 
    arrange(STUDYID, USUBJID, DRUG, ANALYTE, DOSNO, TIME)
  
  return(adnca_conc)
}
