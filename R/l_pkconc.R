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
#' @param main_title A character string specifying the main title of the listing table.
#' @param subtitle_lists A character string specifying the subtitle of the listing table.
#' @param footnote_table A character string specifying the footnote of the listing table.
#'
#' @return A list of listings, each corresponding to a unique combination of the grouping variables.
#'
#' @details
#' The function performs the following steps:
#'   - Groups the data based on the specified grouping variables.
#'   - Formats the 0 and NA values as defined by the formatting table.
#'   - Creates a listing for each unique combination of the grouping variables.
#'
#' The `formatting_vars_table` should be a data frame with the following columns:
#'   - `var_name`: The name of the variable.
#'   - `Label`: The label for the variable.
#'   - `na_str`: The string to use for NA values.
#'   - `cero_str`: The string to use for 0 values.
#'   - `align`: The alignment for the variable (e.g., "center").
#'   - `format_fun`: The formatting function to use ("round" or "signif").
#'   - `digits`: The number of digits to use for numeric formatting.
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
  main_title = paste0("Listing of PK Concentration by Treatment Group,",
                      "Subject and Nominal Time, PK Population"),
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
                            paste0("$", listgroup_vars),
                            sep = ": ",
                            collapse = "\n")
  }

  # If the formatting table was not user defined make one standard
  if (is.null(formatting_vars_table)) {
    formatting_vars_table <-  data.frame(
      var_name = c(grouping_vars, displaying_vars)
    ) %>%
      rowwise() %>%
      # Create a label column
      mutate(
        Label = parse_annotation(adpc, paste0("!", var_name)),
        na_str = "NA",
        cero_str = ifelse(var_name == "AVAL", "BLQ", "0"),
        align = "center",
        format_fun =  ifelse(is.numeric(adpc[[var_name]]) &&
                               !is.integer(adpc[[var_name]]), "round", NA),
        digits = ifelse(is.numeric(adpc[[var_name]]) &&
                          !is.integer(adpc[[var_name]]), 3, NA)
      ) %>%
      # For the default case (when possible) label includes unit
      mutate(
        Label = if ("AVALU" %in% names(adpc)) ifelse(var_name == "AVAL",
                                                     paste0(Label, " ($AVALU)"), Label) else Label,
        Label = if ("RRLTU" %in% names(adpc)) ifelse(var_name == "AFRLT",
                                                     paste0(Label, " ($RRLTU)"), Label) else Label,
        Label = if ("RRLTU" %in% names(adpc)) ifelse(var_name == "NFRLT",
                                                     paste0(Label, " ($RRLTU)"), Label) else Label
      ) %>%
      ungroup()
  }

  # Create the proper object from the UI table for col_formatting
  formatting_vars_list <- lapply(seq_len(nrow(formatting_vars_table)), function(i) {
    row <- formatting_vars_table[i, ]
    fmt_config(na_str = row$na_str,
               format = NULL,
               align = row$align)
  }) %>%
    setNames(nm = formatting_vars_table$var_name)

  # Create a special object to map labels to each variable
  format_labs <- formatting_vars_table %>%
    filter(!is.na(Label)) %>%
    select(var_name, Label) %>%
    pull(Label, var_name)

  # Create a special object to map 0 string values to each variable
  format_cero <- formatting_vars_table %>%
    select(var_name, cero_str) %>%
    pull(cero_str, var_name)

  # Create a special pair of objects to map number of digits to each variable
  format_digits <- formatting_vars_table %>%
    filter(!is.na(format_fun) & !is.na(digits)) %>%
    select(var_name, digits) %>%
    pull(digits, var_name)

  format_funs <- formatting_vars_table %>%
    filter(!is.na(format_fun) & !is.na(digits)) %>%
    select(var_name, format_fun) %>%
    pull(format_fun, var_name)

  # Group the data based on the listgroup_vars
  adpc_grouped <- adpc %>%
    mutate(across(all_of(listgroup_vars), as.character)) %>%
    rowwise() %>%
    dplyr::mutate(id_list = interaction(!!!syms(listgroup_vars))) %>%

    # Format number of digits
    mutate(across(
      all_of(names(format_digits)), ~
        do.call(format_funs[cur_column()],
                list(x = ., digits = format_digits[cur_column()]))
    )) %>%

    # Format 0 values as defined by format_cero
    mutate(across(
      all_of(names(format_cero)), ~
        ifelse(. == 0, format_cero[cur_column()], as.character(.))
    ))

  # Make sure the data stays labelled
  var_labels(adpc_grouped) <- c(var_labels(adpc), id_list = "id")
  var_labels(adpc_grouped) <- ifelse(is.na(var_labels(adpc_grouped)),
                                     names(adpc_grouped),
                                     var_labels(adpc_grouped))

  # Split the lists based on the listgroup_vars
  lapply(unique(adpc_grouped[["id_list"]]), \(id_val) {

    list_data <- adpc_grouped %>% dplyr::filter(id_list ==  id_val)

    # Assign the labels requested by the user
    for (var_with_lab in names(format_labs)) {

      # Do the label
      label <- parse_annotation(list_data, format_labs[var_with_lab])
      attr(list_data[[var_with_lab]], "label") <- paste0(label, collapse = " \nor ")

      # Check label is unique for the list
      if (length(label) != 1) {
        # ToDo: Warning to display as notification in the App
        warning(paste0("pkcl01, but not unique label in ", id_val, " for ",
                       var_with_lab, ". Make sure when using $var that for each",
                       " list group only 1 expression applies. Here there are many: ",
                       paste(label, collapse = ", ")))
      }
    }

    # Make all text definitions and filter columns
    main_title <- parse_annotation(data = list_data, text = main_title)
    list_titles <- gsub("<br>", "\n", parse_annotation(data = list_data,
                                                       text = subtitle_lists))
    footnote_table <- parse_annotation(data = list_data, text = footnote_table)


    # Build the listing object
    rl <- rlistings::as_listing(
      df = list_data,
      key_cols = grouping_vars,
      disp_cols = displaying_vars,
      main_title = main_title,
      subtitles = list_titles,
      main_footer = footnote_table,
      col_formatting = formatting_vars_list
    )

  })  %>%

    setNames(unique(adpc_grouped[["id_list"]]))
}
