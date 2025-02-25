#' Reshape PKNCA Results
#'
#' This function reshapes the structure of the results produced by the main function
#' of the PKNCA package (pk.nca) in a way that each row represents all the main results
#' summarized for each profile in each individual/patient. Excluding the ID variables,
#' each column name corresponds with a calculated parameter and between brackets its
#' corresponding units. AUC intervals, if present, are be added as additional columns.
#'
#' @param myres The output of PKNCA::pk.nca
#'
#' @returns A data frame which provides an easy overview on the results from the NCA
#'          in each profile/subject and how it was computed lambda (half life) and the results
#'          of the NCA parameters (cmax, AUC, AUClast)
#'
#' @importFrom dplyr select left_join rename mutate distinct group_by arrange ungroup
#' @importFrom dplyr filter slice across where
#' @importFrom tidyr pivot_wider pivot_longer
#' @export
#'
pivot_wider_pknca_results <- function(myres) {

  ############################################################################################
  # Derive lambda.z.n.points & lambda.z.method
  # ToDo: At some point this will be integrated in PKNCA and will need to be removed//modified
  added_params <- NULL
  if (all(c("lambda.z",
            "lambda.z.n.points",
            "lambda.z.time.first") %in% unique(myres$result$PPTESTCD))) {
    added_params <- myres$result %>%
      filter(PPTESTCD %in% c("lambda.z.n.points", "lambda.z.time.first", "lambda.z"),
             type_interval == "main") %>%
      select(unname(unlist(myres$data$conc$columns$groups)),
             PPTESTCD, PPSTRES, DOSNO, start, end) %>%
      unique() %>%
      pivot_wider(names_from = PPTESTCD, values_from = PPSTRES) %>%
      left_join(myres$data$conc$data) %>%
      # Derive lambda.z.method: was lambda.z manually customized?
      mutate(lambda.z.method = ifelse(
        any(is.excluded.hl) | any(is.included.hl), "Manual", "Best slope"
      )) %>%
      # Derive lambda.z.ix: If present consider inclusions and disconsider exclusions
      group_by(!!!syms(unname(unlist(myres$data$conc$columns$groups))), DOSNO) %>%
      filter(!exclude_half.life | is.na(lambda.z.time.first) | is.na(lambda.z.n.points)) %>%
      filter(TIME >= (lambda.z.time.first + start) | is.na(lambda.z.time.first)) %>%
      filter(row_number() <= lambda.z.n.points | is.na(lambda.z.n.points)) %>%
      mutate(lambda.z.ix = paste0(IX, collapse = ",")) %>%
      mutate(lambda.z.ix = ifelse(is.na(lambda.z), NA, lambda.z.ix)) %>%
      select(unname(unlist(myres$data$conc$columns$groups)),
             DOSNO, start, end, lambda.z.ix, lambda.z.method) %>%
      unique()
  }
  ############################################################################################

  # Filter out infinite AUCs and pivot the data to incorporate
  # the parameters into columns with their units
  main_intervals_vals <- myres$result %>%
    distinct() %>%
    filter(type_interval == "main")  %>%
    mutate(PPTESTCD = paste0(PPTESTCD, "[", PPSTRESU, "]")) %>%
    select(-PPSTRESU, -PPORRES, -PPORRESU, -exclude, -type_interval) %>%
    pivot_wider(names_from = PPTESTCD, values_from = PPSTRES)

  main_intervals_exclude <- myres$result %>%
    distinct() %>%
    filter(type_interval == "main") %>%
    select(-PPSTRES, -PPSTRESU, -PPORRES, -PPORRESU, -type_interval)  %>%
    pivot_wider(names_from = PPTESTCD, values_from = exclude, names_prefix = "exclude.")

  main_intervals <- left_join(main_intervals_vals, main_intervals_exclude)

  # If there were intervals defined, make independent columns for each
  if (any(myres$result$type_interval == "manual")) {

    manual_aucs_vals <- myres$result %>%
      filter(type_interval == "manual", startsWith(PPTESTCD, "aucint")) %>%
      mutate(
        interval_name = paste0(signif(start), "-", signif(end)),
        interval_name_col = paste0(PPTESTCD, "_", interval_name)
      ) %>%
      select(-exclude, -PPSTRESU, -PPORRES, -PPORRESU, -start, -end,
             -PPTESTCD, -interval_name, -type_interval) %>%
      pivot_wider(names_from = interval_name_col,
                  values_from = PPSTRES)

    manual_aucs_exclude <- myres$result %>%
      filter(type_interval == "manual", startsWith(PPTESTCD, "aucint")) %>%
      mutate(
        interval_name = paste0(signif(start), "-", signif(end)),
        interval_name_col = paste0("exclude.", PPTESTCD, "_", interval_name)
      ) %>%
      select(-PPSTRES, -PPSTRESU, -PPORRES, -PPORRESU, -start, -end,
             -PPTESTCD, -interval_name, -type_interval) %>%
      pivot_wider(names_from = interval_name_col, values_from = exclude)

    manual_aucs <- inner_join(manual_aucs_vals, manual_aucs_exclude)

    all_aucs <- left_join(main_intervals, manual_aucs)
  } else {
    all_aucs <- main_intervals
  }

  # If derived, merge lambda.z.ix & lambda.z.method
  if (!is.null(added_params)) all_aucs <- left_join(all_aucs, added_params)

  # Do a final standardization of the results reshaped
  all_aucs  %>%
    mutate(Exclude = pmap_chr(across(starts_with("exclude.")), .extract_exclude_values)) %>%
    select(-starts_with("exclude.")) %>%
    # Define the number of decimals to round the results
    mutate(across(where(is.numeric), ~ round(.x, 3)))  %>%
    ungroup()
}

#' Helper function to extract exclude values
.extract_exclude_values <- function(...) {
  raw_values <- unique(c(...))  # Get unique exclude values from different columns
  raw_values <- raw_values[!is.na(raw_values)]  # Remove NAs

  # Split each entry into individual phrases using "; " as a separator
  split_values <- unlist(strsplit(raw_values, "; "))

  # Remove duplicate messages
  unique_values <- unique(trimws(split_values))

  if (length(unique_values) == 0) NA_character_ else paste(unique_values, collapse = ", ")
}
