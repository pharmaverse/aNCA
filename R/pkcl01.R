#' PKCL01 - l_pkconc
#' @author Legras, Valentin / Kobana, Kezia
#' @param ADPC dataset
#' @description
#' This l_pkconc function creates PK concentration listing by cohort, patient and visit
#' https://insightsengineering.github.io/tlg-catalog/listings/pharmacokinetic/pkcl01.html

# check which formats for listings + create function
# check how many signif figures for listings, formats arg does not work
datasets = list(adpc = read.csv("inst/shiny/data/DummyRO_ADNCA.csv"))
l_pkconc <- function(datasets, manual_footnote = NULL){
  assert_emptydf(datasets$adpc)
  assert_patients_exist(datasets$adpc)
  assert_unique(datasets$adpc,"AVALU")
  assert_unique(datasets$adpc,"PARAM")
  
  
  AVALC_label   = paste0(unique(datasets$adpc$ANALYTE), ' Concentration\n (', unique(datasets$adpc$AVALU), ')')
  datasets$adpc = datasets$adpc  %>% mutate(AVALC = ifelse(AVAL == 0, "BLQ", as.character(AVAL)))
  attr(datasets$adpc[["AVALC"]], "label") <- AVALC_label
  
  AVAL_label   = paste0(unique(datasets$adpc$ANALYTE), ' Concentration\n (', unique(datasets$adpc$AVALU), ')')
  attr(datasets$adpc[["AVAL"]], "label") <- AVAL_label
  
  
  lst = as_listing(datasets$adpc,
                   key_cols = c("AVALC"),
                   disp_cols = c("TRT01A", "USUBJID", "AVISIT"))
  # formats = list("xx.xx", "xx.xx", "xx.xx"))
  
  return(lst)
  
}
export_as_pdf(lst, "lst.pdf")

library(rlistings)

as_listing(df = ,
           key_cols = ,
           disp_cols = ,
           non_disp_cols = ,
           unique_rows = ,
           default_formatting = fmt_config(format = "xx.xx" "xx.xx - xx.xx" or format_fun(),
                                           na_str = "-",
                                           align = "left"),
           col_formatting = ,
           main_title = ,
           subtitles = ,
           main_footer = ,
           prov_footer = ,
           split_into_pages_by_var = )



attr(adpc[["AFRLT"]], "label") <- "Actual time from first dose"
attr(adpc[["AVAL"]], "label") <- "Analysis value" 
attr(adpc[["PARAM"]], "label") <- "Analyte"
attr(adpc[["PCSPEC"]], "label") <- "Specimen" 
attr(adpc[["ROUTE"]], "label") <- "Administration" 

l_pkconc <- function(
    adpc = read.csv("inst/shiny/data/DummyRO_ADNCA.csv"),
    listgroup_vars = c("PARAM", "PCSPEC", "ROUTE"),
    grouping_vars = c("TRT01A", "USUBJID", "AVISIT"),
    displaying_vars = c("NFRLT", "AFRLT", "AVAL"),
    formatting_vars_table = NULL,
    title_table = NULL,
    footnote_table = "*: Patients excluded from the summary table and mean plots"
) {
  
  # If the title was not user defined use the listgroup_vars argument to do it
  if (is.null(title_table)) {
    title_table <- paste(paste0("!", listgroup_vars),
                         paste0("$",listgroup_vars),
                         sep = ": ",
                         collapse = "\n")
  }
  
  # If the formatting table was not user defined make one standard
  if (is.null(formatting_vars_table)) {
    # Create a dataframe with rows c(grouping_Vars, displaying_vars) and columns c("Variable name", "Label", "Digits")
    formatting_vars_table <-  data.frame(
      Variable_name = c(grouping_vars, displaying_vars)
    ) %>% 
      rowwise()  %>% 
      # Create a label column
      mutate(Label = parse_annotation(adpc, paste0("!", Variable_name)),
             na_str = "NA",
             cero_str = "BLQ",
             align = "center",
             format = ifelse(is.numeric(adpc[[Variable_name]]), "xx.xx", NA)
      ) %>%
      ungroup()
    
    # Create a digits column
    
  }
  
  # Split the data based on the listgroup_vars variables
  adpc_grouped <- adpc %>%
    mutate(across(all_of(listgroup_vars), as.character)) %>%
    rowwise() %>%
    dplyr::mutate(id_list = interaction(!!!syms(listgroup_vars)))
  
  
  lapply(unique(adpc_grouped[["id_list"]]), \(id_val) {
    
    list_data <- adpc_grouped %>% dplyr::filter(id_plot ==  id_val)
    
    as_listing(
      df = list_data,
      key_cols = grouping_vars,
      disp_cols = displaying_vars,
      main_title = parse_annotation(data = list_data,
                                    text = title_table),
      main_footer = parse_annotation(footnote_table)
    )  
  })  %>%
    setNames(unique(adpc_grouped[["id_list"]]))
} 