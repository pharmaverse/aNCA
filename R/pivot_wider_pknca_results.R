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

  # Filter out infinite AUCs and pivot the data to incorporate
  # the parameters into columns with their units
  infinite_aucs_vals <- myres$result %>%
    distinct() %>%
    filter(type_interval == "main")  %>%
    mutate(PPTESTCD = paste0(PPTESTCD, "[", PPSTRESU, "]")) %>%
    select(-PPSTRESU, -PPORRES, -PPORRESU, -exclude, -type_interval) %>%
    pivot_wider(names_from = PPTESTCD, values_from = PPSTRES)

  infinite_aucs_exclude <- myres$result %>%
    distinct() %>%
    filter(type_interval == "main") %>%
    select(-PPSTRES, -PPSTRESU, -PPORRES, -PPORRESU, -type_interval)  %>%
    pivot_wider(names_from = PPTESTCD, values_from = exclude, names_prefix = "exclude.")

  infinite_aucs <- inner_join(infinite_aucs_vals, infinite_aucs_exclude)

  
  # # ToDo: At some point this code inside the conditional would be integrated in PKNCA
  # if (all(c("lambda.z.time.first", "lambda.z.n.points", )))
  # infinite_aucs_with_lambda <- inner_join(myres$data$conc$data, infinite_aucs) %>%
  #   group_by(STUDYID, PCSPEC, ANALYTE, USUBJID, DOSNO) %>%
  #   arrange(STUDYID, PCSPEC, ANALYTE, USUBJID, DOSNO, IX) %>%
  #   # Deduce if the user perform an exclusion/selection to indicate if the slope
  #   # is manually selected
  #   mutate(lambda.z.method = ifelse(
  #     any(is.excluded.hl) | any(is.included.hl), "Manual", "Best slope"
  #   )) %>%
  #   # filter out the rows that do not have relation with lambda calculation (when calculated)
  #   # and derive the IX
  #   filter(!exclude_half.life | is.na(lambda.z.time.first) | is.na(lambda.z.n.points)) %>%
  #   filter(TIME >= (lambda.z.time.first + start) | is.na(lambda.z.time.first)) %>%
  #   filter(row_number() <= lambda.z.n.points | is.na(lambda.z.n.points)) %>%
  #   mutate(lambda.z.ix = paste0(IX, collapse = ",")) %>%
  #   mutate(lambda.z.ix = ifelse(is.na(lambda.z), NA, lambda.z.ix)) %>%
  #   slice(1) %>%
  #   select(any_of(c(names(infinite_aucs), "lambda.z.method", "lambda.z.ix")))

  # If there were intervals defined, make independent columns for each
  if (any(myres$result$type_interval == "manual")) {

    interval_aucs_vals <- myres$result %>%
      filter(type_interval == "manual", startsWith(PPTESTCD, "aucint")) %>%
      mutate(
        interval_name = paste0(signif(start), "-", signif(end)),
        interval_name_col = paste0(PPTESTCD, "_", interval_name)
      ) %>%
      select(-exclude, -PPSTRESU, -PPORRES, -PPORRESU, -start, -end,
             -PPTESTCD, -interval_name, -type_interval) %>%
      pivot_wider(names_from = interval_name_col,
                  values_from = PPSTRES)

    interval_aucs_exclude <- myres$result %>%
      filter(type_interval == "manual", startsWith(PPTESTCD, "aucint")) %>%
      mutate(
        interval_name = paste0(signif(start), "-", signif(end)),
        interval_name_col = paste0("exclude.", PPTESTCD, "_", interval_name)
      ) %>%
      select(-PPSTRES, -PPSTRESU, -PPORRES, -PPORRESU, -start, -end,
             -PPTESTCD, -interval_name, -type_interval) %>%
      pivot_wider(names_from = interval_name_col, values_from = exclude)

    interval_aucs <- inner_join(interval_aucs_vals, interval_aucs_exclude)

    all_aucs <- inner_join(infinite_aucs, interval_aucs)
  } else {
    all_aucs <- infinite_aucs_with_lambda
  }

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
