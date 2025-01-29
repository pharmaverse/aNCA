#' PKCL01 - l_pkconc
#' @author Legras, Valentin / Kobana, Kezia
#' @param ADPC dataset
#' @description
#' This l_pkconc function creates PK concentration listing by cohort, patient and visit
#' https://insightsengineering.github.io/tlg-catalog/listings/pharmacokinetic/pkcl01.html

# check which formats for listings + create function
# check how many signif figures for listings, formats arg does not work

# Define variables
adpc = read.csv("inst/shiny/data/DummyRO_ADNCA.csv")
listgroup_vars = c("PARAM", "PCSPEC", "ROUTE")
grouping_vars = c("TRT01A", "USUBJID", "AVISIT")
displaying_vars = c("NFRLT", "AFRLT", "AVAL")
formatting_vars_table = NULL
title_table = NULL
footnote_table = "*: Patients excluded from the summary table and mean plots"

# Define labels
attr(adpc[["AFRLT"]], "label") <- "Actual time from first dose"
attr(adpc[["NFRLT"]], "label") <- "Planned time from first dose"
attr(adpc[["AVAL"]], "label") <- "Analysis value" 
attr(adpc[["PARAM"]], "label") <- "Analyte"
attr(adpc[["TRT01A"]], "label") <- "Actual treatment"
attr(adpc[["PCSPEC"]], "label") <- "Specimen" 
attr(adpc[["ROUTE"]], "label") <- "Administration" 
attr(adpc[["USUBJID"]], "label") <- "Unique Subject ID" 
attr(adpc[["AVISIT"]], "label") <- "Actual visit" 


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
             cero_str = ifelse(Variable_name == "AVAL", "BLQ", "0"),
             align = "center",
             # ToDo: Mateusz // Check formatters::list_valid_format_labels() for input list
             format = "NA",
             unit = case_when(Variable_name == "AVAL" ~ "AVALU",
                              Variable_name %in% c("NFRLT", "AFRLT") ~ "RRLTU",
                              TRUE ~ NA
             )
             # unit = displaying_vars we need unit, else NA
      ) %>%
      ungroup()
  }
  
  # Create the proper object from the UI table for col_formatting
  formatting_vars_list <- lapply(1:nrow(formatting_vars_table), function(i){
    row <- formatting_vars_table[i,]
    fmt_config(format = if (row$format == "NA") NULL else row$format,
               na_str = row$na_str,
               align = row$align
    )
  }) %>%
    setNames(nm = formatting_vars_table$Variable_name)
  
  # Create a special object to map 0 string values and use it to format values
  formatting_vars_cero <- formatting_vars_table %>%
    rowwise() %>%
    select(Variable_name, cero_str) %>%
    deframe()
  
  # Group the data based on the listgroup_vars
  adpc_grouped <- adpc %>%
    mutate(across(all_of(listgroup_vars), as.character)) %>%
    rowwise() %>%
    dplyr::mutate(id_list = interaction(!!!syms(listgroup_vars))) %>% 
    
    # Format 0 values as defined by formatting_vars_cero
    mutate(across(all_of(names(formatting_vars_cero)), ~
                    ifelse(. == 0, formatting_vars_cero[cur_column()], as.character(.))
    ))
  
  var_labels(adpc_grouped) = c(var_labels(adpc), id_list = "id")
  
  # Split the lists based on the listgroup_vars
  lapply(unique(adpc_grouped[["id_list"]]), \(id_val) {
    
    data <- adpc_grouped %>% dplyr::filter(id_list ==  id_val)
    list_data = data %>% select(all_of(c(grouping_vars, displaying_vars)))
    
    for(label_unit in unlist(formatting_vars_table[!is.na(formatting_vars_table$unit),"Variable_name"])){
      # TODO: print error if print(length(unique(data[[unlist(formatting_vars_table[formatting_vars_table$Variable_name == label_unit, "unit"])]])) != 1)
      print(length(unique(data[[unlist(formatting_vars_table[formatting_vars_table$Variable_name == label_unit, "unit"])]])) != 1)
      
      
      attr(list_data[[label_unit]], "label") <- paste0(attr(list_data[[label_unit]], "label"), "\n(", unique(data[[unlist(formatting_vars_table[formatting_vars_table$Variable_name == label_unit, "unit"])]]),")")
    }
    
    as_listing(
      df = list_data,
      key_cols = grouping_vars,
      disp_cols = displaying_vars,
      main_title = "Listing of PK Concentration by Treatment Group, Subject and Nominal Time, PK Population",
      subtitles = gsub("<br>", "\n", parse_annotation(data = data,
                                                      text = title_table)), 
      main_footer = parse_annotation(data = data,
                                     text = footnote_table),
      col_formatting = formatting_vars_list
    )
  })  %>%
    setNames(unique(adpc_grouped[["id_list"]]))
}