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

  # Get all names with units and make a dictionary structure
  dict_pttestcd_with_units <- myres$result %>%
    dplyr::select(PPTESTCD, PPORRESU) %>%
    dplyr::distinct() %>%
    pull(PPORRESU, PPTESTCD)

  # Filter out infinite AUCs and pivot the data to incorporate
  # the parameters into columns with their units
  infinite_aucs_vals <- myres$result %>%
    dplyr::distinct() %>%
    dplyr::filter(type_interval == "main")  %>%
    dplyr::select(-PPORRESU, -exclude, -type_interval) %>%
    tidyr::pivot_wider(names_from = PPTESTCD, values_from = PPORRES)

  infinite_aucs_exclude <- myres$result %>%
    dplyr::distinct() %>%
    dplyr::filter(type_interval == "main") %>%
    dplyr::select(-PPORRES, -PPORRESU, -type_interval)  %>%
    dplyr::mutate(PPTESTCD = paste0("exclude.", PPTESTCD)) %>%
    tidyr::pivot_wider(names_from = PPTESTCD, values_from = exclude)

  infinite_aucs <- merge(infinite_aucs_vals, infinite_aucs_exclude)

  infinite_aucs_with_lambda <- dplyr::inner_join(myres$data$conc$data, infinite_aucs) %>%
    group_by(STUDYID, PCSPEC, ANALYTE, USUBJID, DOSNO) %>%
    arrange(STUDYID, PCSPEC, ANALYTE, USUBJID, DOSNO, IX) %>%
    # Deduce if the user perform an exclusion/dplyr::selection to indicate if the slope
    # is manually dplyr::selected
    dplyr::mutate(lambda.z.method = ifelse(
      any(is.excluded.hl) | any(is.included.hl), "Manual", "Best slope"
    )) %>%
    # dplyr::filter out the rows that do not have relation with lambda calculation (when calculated)
    # and derive the IX
    dplyr::filter(!exclude_half.life | is.na(lambda.z.time.first) | is.na(lambda.z.n.points)) %>%
    dplyr::filter(TIME >= (lambda.z.time.first + start) | is.na(lambda.z.time.first)) %>%
    dplyr::filter(row_number() <= lambda.z.n.points | is.na(lambda.z.n.points)) %>%
    dplyr::mutate(lambda.z.ix = paste0(IX, collapse = ",")) %>%
    dplyr::mutate(lambda.z.ix = ifelse(is.na(lambda.z), NA, lambda.z.ix)) %>%
    dplyr::slice(1) %>%
    dplyr::select(any_of(c(names(infinite_aucs), "lambda.z.method", "lambda.z.ix")))


  # If there were intervals defined, make independent columns for each
  if (any(myres$result$type_interval == "manual")) {

    interval_aucs_vals <- myres$result %>%
      dplyr::filter(type_interval == "manual", startsWith(PPTESTCD, "aucint")) %>%
      dplyr::mutate(
        interval_name = paste0(signif(start), "-", signif(end)),
        interval_name_col = paste0(PPTESTCD, "_", interval_name)
      ) %>%
      dplyr::select(-exclude, -PPORRESU, -start, -end,
                    -PPTESTCD, -interval_name, -type_interval) %>%
      tidyr::pivot_wider(names_from = interval_name_col,
                         values_from = PPORRES)

    interval_aucs_exclude <- myres$result %>%
      dplyr::filter(type_interval == "manual", startsWith(PPTESTCD, "aucint")) %>%
      dplyr::mutate(
        interval_name = paste0(signif(start), "-", signif(end)),
        interval_name_col = paste0("exclude.", PPTESTCD, "_", interval_name)
      )  %>%
      dplyr::select(-PPORRES, -PPORRESU, -start, -end,
                    -PPTESTCD, -interval_name, -type_interval) %>%
      tidyr::pivot_wider(names_from = interval_name_col, values_from = exclude)

    interval_aucs <- merge(interval_aucs_vals, interval_aucs_exclude) %>%
      # Rename column names to include the units in parenthesis
      rename_with(~ifelse(
        .x %in% names(dict_pttestcd_with_units),
        paste0(.x, "_", "[", dict_pttestcd_with_units[.x], "]"),
        .x
      ))

    all_aucs <- merge(infinite_aucs_with_lambda, interval_aucs, all = TRUE)
  } else {
    all_aucs <- infinite_aucs_with_lambda
  }

  # Do a final standardization of the results reshaped
  all_aucs  %>%
    # Define the number of decimals to round the results
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))  %>%
    dplyr::ungroup()

}
