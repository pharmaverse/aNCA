#' Create PK Concentration Listing
#'
#' This function creates a listing of pharmacokinetic (PK) concentration data segregating a dataset
#' in lists that are customizable in title, footnotes, grouping/displayed variables, missing/zero
#' values and/or number of digits displayed.
#'
#' @param data A data frame containing the PK concentration data.
#' @param listgroup_vars A character vector specifying the variables to separate lists.
#' @param grouping_vars A character vector specifying the grouping variables within each list.
#' @param displaying_vars A character vector specifying the variables to display in the listing.
#' @param formatting_vars_table A data frame with the formatting of each variable. See details.
#' @param title A character string to parse specifying the main title for the entire listing.
#' @param subtitle A character string to parse specifying the subtitle to use for each list.
#' @param footnote A character string to parse specifying the footnote of the listing table.
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
#'   - `zero_str`: The string to use for 0 values.
#'   - `align`: The alignment for the variable (e.g., "center").
#'   - `format_fun`: The formatting function to use ("round" or "signif").
#'   - `digits`: The number of digits to use for numeric formatting.
#'
#' @examples
#'   # Create a sample dataframe 'data' with the required variables
#'   set.seed(123)
#'   data <- data.frame(
#'     PARAM = rep(c("Param1", "Param2"), each = 6),
#'     PCSPEC = rep(c("Blood", "Urine"), each = 6),
#'     TRT01A = rep(c("Treatment1", "Treatment2"), each = 6),
#'     USUBJID = rep(c(rep(1, 3), rep(2, 3)), 2),
#'     NFRLT = rep(1:3, 4),
#'     AFRLT = rep(1:3, 4) + runif(12, 0, 0.5),
#'     TIMEU = "hours",
#'     AVAL = rep(0:2, 4) + runif(12, 0, 0.5),
#'     AVALU = "mg/L"
#'   )
#'
#'   # Define the formatting table
#'   formatting_vars_table <- data.frame(var_name = names(data),
#'                                       Label = c("Parameter", "Specimen", "Treatment Arm",
#'                                                 "Unique Subject ID", "Norminal Time ($TIMEU)",
#'                                                 "Actual Time ($TIMEU)", "Time Unit",
#'                                                 "Analyte Value ($AVALU)", "Analyte Unit"),
#'                                       na_str = "Missing",
#'                                       zero_str = c(rep("0", 7), "BLQ", "0"),
#'                                       align = "center",
#'                                       format_fun = c(NA, NA, NA, NA,
#'                                                      "round", "round", NA, "round", NA),
#'                                       digits = c(NA, NA, NA, NA, 2, 2, NA, 3, NA))
#'
#'   # Call the l_pkcl01 function with the sample data
#'   listing_ex <- l_pkcl01(data = data,
#'                          listgroup_vars = c("PARAM", "PCSPEC"),
#'                          grouping_vars = c("TRT01A", "USUBJID"),
#'                          displaying_vars = c("NFRLT", "AFRLT", "AVAL"),
#'                          formatting_vars_table = formatting_vars_table,
#'                          title = "Listing of PK Concentration",
#'                          subtitle = "Subjects with !PARAM: $PARAM (!PCSPEC: $PCSPEC)"
#'                          )
#'   print(listing_ex)
#'
#' @import dplyr formatters rlistings
#' @importFrom stats setNames
#' @export
#' @author Gerardo Rodriguez
l_pkcl01 <- function(
  data,
  listgroup_vars = c("PARAM", "PCSPEC", "ROUTE"),
  grouping_vars = c("TRT01A", "USUBJID", "ATPTREF"),
  displaying_vars = c("NFRLT", "AFRLT", "AVAL"),
  formatting_vars_table = NULL,
  title = paste0("Listing of PK Concentration by Treatment Group,",
                 "Subject and Nominal Time, PK Population"),
  subtitle = NULL,
  footnote = "*: Subjects excluded from the summary table and mean plots"
) {

  # If there are columns defined in the function that are not in the data, throw an error
  missing_cols <- setdiff(c(grouping_vars, displaying_vars), colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ",")))
  }

  # If the title was not user defined use the listgroup_vars argument to do it
  if (is.null(subtitle)) {
    subtitle <- paste(paste0("!", listgroup_vars),
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
        Label = parse_annotation(data, paste0("!", var_name)),
        na_str = "NA",
        zero_str = ifelse(var_name == "AVAL", "BLQ", "0"),
        align = "center",
        format_fun =  ifelse(is.double(data[[var_name]]), "round", NA),
        digits = ifelse(is.double(data[[var_name]]), 3, NA)
      ) %>%
      # For the default case (when possible) label includes unit
      mutate(
        Label = case_when(
          "AVALU" %in% names(data) & var_name == "AVAL" ~ paste0(Label, " ($AVALU)"),
          "RRLTU" %in% names(data) & var_name == "AFRLT" ~ paste0(Label, " ($RRLTU)"),
          "RRLTU" %in% names(data) & var_name == "NFRLT" ~ paste0(Label, " ($RRLTU)"),
          .default = Label
        )
      ) %>%
      ungroup()
  } else {
    formatting_vars_table <- formatting_vars_table %>%
      rowwise() %>%
      mutate(
        Label = parse_annotation(data, Label),
        digits = as.numeric(digits)
      )
  }

  # Create the proper object from the UI table for col_formatting
  formatting_vars_list <- formatting_vars_table %>%
    dplyr::rowwise() %>%
    group_map(~ {
      fmt_config(na_str = .x$na_str, format = NULL, align = .x$align)
    }) %>%
    setNames(nm = formatting_vars_table$var_name)

  # Create a special object to map labels to each variable
  format_labs <- formatting_vars_table %>%
    filter(!is.na(Label)) %>%
    select(var_name, Label) %>%
    pull(Label, var_name)

  # Create a special object to map 0 string values to each variable
  format_zero <- formatting_vars_table %>%
    select(var_name, zero_str) %>%
    pull(zero_str, var_name)

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
  data_grouped <- data %>%
    mutate(across(all_of(listgroup_vars), as.character)) %>%
    rowwise() %>%
    dplyr::mutate(id_list = interaction(!!!syms(listgroup_vars))) %>%

    # Format number of digits
    mutate(across(
      all_of(names(format_digits)), ~
        do.call(format_funs[cur_column()],
                list(x = ., digits = format_digits[cur_column()]))
    )) %>%

    # Format 0 values as defined by format_zero
    mutate(across(
      all_of(names(format_zero)), ~
        ifelse(. == 0, format_zero[cur_column()], as.character(.))
    ))

  # Make sure the data stays labelled
  var_labels(data_grouped) <- c(var_labels(data), id_list = "id")
  var_labels(data_grouped) <- ifelse(is.na(var_labels(data_grouped)),
                                     names(data_grouped),
                                     var_labels(data_grouped))

  # Split the lists based on the listgroup_vars
  lapply(unique(data_grouped[["id_list"]]), \(id_val) {

    list_data <- data_grouped %>% dplyr::filter(id_list ==  id_val)

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
    title <- parse_annotation(data = list_data, text = title)
    list_titles <- gsub("<br>", "\n", parse_annotation(data = list_data,
                                                       text = subtitle))
    footnote <- parse_annotation(data = list_data, text = footnote)


    # Build the listing object
    rl <- rlistings::as_listing(
      df = list_data,
      key_cols = grouping_vars,
      disp_cols = displaying_vars,
      main_title = title,
      subtitles = list_titles,
      main_footer = footnote,
      col_formatting = formatting_vars_list
    )

  })  %>%

    setNames(unique(data_grouped[["id_list"]]))
}
