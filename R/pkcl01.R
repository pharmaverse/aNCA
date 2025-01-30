# Define variables
adpc = read.csv("inst/shiny/data/DummyRO_ADNCA.csv")
listgroup_vars = c("PARAM", "PCSPEC", "ROUTE")
grouping_vars = c("TRT01A", "USUBJID", "AVISIT")
displaying_vars = c("NFRLT", "AFRLT", "AVAL")
formatting_vars_table = NULL
subtitle_lists = NULL
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

#' Create PK Concentration Listing
#'
#' This function creates a listing of pharmacokinetic (PK) concentration data by treatment group,
#' subject, and nominal time.
#'
#' @param adpc A data frame containing the PK concentration data.
#' @param listgroup_vars A character vector specifying the variables to group by in the listing.
#' @param grouping_vars A character vector specifying the grouping variables for the listing.
#' @param displaying_vars A character vector specifying the variables to display in the listing.
#' @param formatting_vars_table A data frame specifying the formatting options for the variables.
#' @param subtitle_lists A character string specifying the title of the listing table.
#' @param footnote_table A character string specifying the footnote of the listing table.
#'
#' @return A list of listings, each corresponding to a unique combination of the grouping variables.
#'
#' @details
#' The function performs the following steps:
#'   - Groups the data based on the specified grouping variables.
#'   - Formats the 0 values as defined by the formatting table.
#'   - Creates a listing for each unique combination of the grouping variables.
#'   - Adds units to the variable labels in the listing.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   listings <- l_pkconc(adpc, listgroup_vars, grouping_vars, displaying_vars)
#' }
#'
#' @import dplyr formatters rlistings
#' @export
#' @author Gerardo Rodriguez

l_pkconc <- function(
    adpc = read.csv("inst/shiny/data/DummyRO_ADNCA.csv"),
    listgroup_vars = c("PARAM", "PCSPEC", "ROUTE"),
    grouping_vars = c("TRT01A", "USUBJID", "AVISIT"),
    displaying_vars = c("NFRLT", "AFRLT", "AVAL"),
    formatting_vars_table = NULL,
    subtitle_lists = NULL,
    footnote_table = "*: Patients excluded from the summary table and mean plots"
) {
  
  # If there are columns defined in the function that are not in the data, throw an error
  missing_cols <- setdiff(c(grouping_vars, displaying_vars), colnames(adpc))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ",")))
  }
  
  # If the title was not user defined use the listgroup_vars argument to do it
  if (is.null(subtitle_lists)) {
    subtitle_lists <- paste(paste0("!", listgroup_vars),
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
             unit = case_when(Variable_name == "AVAL" & "AVALU" %in% names(adpc) ~ "AVALU",
                              Variable_name %in% c("NFRLT", "AFRLT") & "RRLTU" %in% names(adpc) ~ "RRLTU",
                              TRUE ~ NA
             )
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
  
  # Create a special object to map 0 string values to each variable
  format_cero <- formatting_vars_table %>% 
    select(Variable_name, cero_str) %>% 
    pull(cero_str, Variable_name)
  
  # Create a special object to map unit columns to each variable
  format_unit <- formatting_vars_table %>% 
    select(Variable_name, unit) %>% 
    filter(!is.na(unit)) %>% 
    pull(unit, Variable_name)
  
  # Group the data based on the listgroup_vars
  adpc_grouped <- adpc %>% 
    mutate(across(all_of(listgroup_vars), as.character)) %>% 
    rowwise() %>% 
    dplyr::mutate(id_list = interaction(!!!syms(listgroup_vars))) %>% 
    
    # Format 0 values as defined by format_cero
    mutate(across(all_of(names(format_cero)), ~
                    ifelse(. == 0, format_cero[cur_column()], as.character(.))
    ))
  
  # Make sure the data stays labelled
  var_labels(adpc_grouped) = c(var_labels(adpc), id_list = "id")
  
  # Split the lists based on the listgroup_vars
  lapply(unique(adpc_grouped[["id_list"]]), \(id_val) {

    data <- adpc_grouped %>% dplyr::filter(id_list ==  id_val)
    list_data = data %>% select(any_of(c(grouping_vars, displaying_vars, unname(format_unit))))

    # For those variables with units specified include them in their labels for the table
    for (var_with_unit in names(format_unit)) {
      unit_val <- unique(data[[format_unit[var_with_unit]]])
      if (length(unit_val) != 1) {
        warning(paste0("pkcl01, but not unique unit in ", id_val, " for ",
                        var_with_unit, " :", paste(unit_val, collapse = ", ")))
      }
      attr(list_data[[var_with_unit]], "label") <- paste0(attr(list_data[[var_with_unit]], "label"),
                                                          " (",
                                                          paste0(unit_val, collapse = ","), 
                                                          ")")
    }

    rl <- rlistings::as_listing(
      df = list_data,
      key_cols = grouping_vars,
      disp_cols = displaying_vars,
      main_title = "Listing of PK Concentration by Treatment Group, Subject and Nominal Time, PK Population",
      subtitles = gsub("<br>", "\n", parse_annotation(data = data,
                                                      text = subtitle_lists)), 
      main_footer = parse_annotation(data = data,
                                     text = footnote_table),
      col_formatting = formatting_vars_list
    )

  })  %>% 
    
    setNames(unique(adpc_grouped[["id_list"]]))
}

l_pkconc(adpc = adpc)
