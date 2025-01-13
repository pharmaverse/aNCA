#' Reshape PKNCA Results
#'
#' This function reshapes the structure of the results produced by the main function
#' of the PKNCA package (pk.nca) in a way that each row represents all the main results
#' summarized for each profile in each individual/patient.
#'
#' @param res_nca The output of PKNCA::pk.nca
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
reshape_pknca_results <- function(res_nca) {

  # Keep only custom units and custom values in the data (discard original ones)
  res_nca$result <- dplyr::select(res_nca$result, -PPORRES, -PPORRESU)

  # Get all names with units and make a dictionary structure
  dict_pttestcd_with_units <- res_nca$result %>%
    select(PPTESTCD, PPSTRESU) %>%
    unique() %>%
    pull(PPSTRESU, PPTESTCD)

  # Filter out infinite AUCs and pivot the data to incorporate
  # the parameters into columns with their units
  infinite_aucs_vals <- res_nca$result %>%
    filter(end == Inf)  %>%
    select(-PPSTRESU, -exclude) %>%
    pivot_wider(names_from = PPTESTCD, values_from = PPSTRES)

  infinite_aucs_exclude <- res_nca$result %>%
    filter(end == Inf) %>%
    select(STUDYID, PCSPEC, ANALYTE, USUBJID, DOSNO, PPTESTCD, exclude)  %>%
    mutate(PPTESTCD = paste0("exclude.", PPTESTCD)) %>%
    pivot_wider(names_from = PPTESTCD, values_from = exclude)

  infinite_aucs <- merge(infinite_aucs_vals, infinite_aucs_exclude)

  infinite_aucs_with_lambda <- res_nca$data$conc$data %>%
    merge(infinite_aucs) %>%
    group_by(STUDYID, PCSPEC, ANALYTE, USUBJID, DOSNO) %>%
    arrange(STUDYID, PCSPEC, ANALYTE, USUBJID, DOSNO, IX) %>%
    # Deduce if the user perform an exclusion/selection to indicate if the slope
    # is manually selected
    mutate(lambda.z.method = ifelse(
      any(is.excluded.hl) | any(is.included.hl), "Manual", "Best slope"
    )) %>%
    # Filter out the rows that do not have relation with lambda calculation (when calculated)
    # and derive the IX
    filter(!exclude_half.life | is.na(lambda.z.time.first) | is.na(lambda.z.n.points)) %>%
    filter(TIME >= (lambda.z.time.first + start) | is.na(lambda.z.time.first)) %>%
    filter(row_number() <= lambda.z.n.points | is.na(lambda.z.n.points)) %>%
    mutate(lambda.z.ix = paste0(IX, collapse = ",")) %>%
    mutate(lambda.z.ix = ifelse(is.na(lambda.z), NA, lambda.z.ix)) %>%
    slice(1) %>%
    select(any_of(c(names(infinite_aucs), "lambda.z.method", "lambda.z.ix")))


  # If there were intervals defined by the user,
  # filter out the AUCs corresponding to those intervals
  if (any(res_nca$result$PPTESTCD == "aucint.last")) {
    interval_aucs_vals <- res_nca$result %>%
      filter(PPTESTCD == "aucint.last") %>%
      mutate(
        interval_name = paste0(start, "-", end),
        interval_name_col = paste0(PPTESTCD, "_", interval_name)
      ) %>%
      select(-exclude, -PPSTRESU, -start, -end, -PPTESTCD, -interval_name) %>%
      pivot_wider(names_from = interval_name_col, values_from = PPSTRES)

    interval_aucs_exclude <- res_nca$result %>%
      filter(PPTESTCD == "aucint.last") %>%
      mutate(
        interval_name = paste0(start, "-", end),
        interval_name_col = paste0("exclude.", PPTESTCD, "_", interval_name)
      )  %>%
      select(-PPSTRES, -PPSTRESU, -start, -end, -PPTESTCD, -interval_name) %>%
      pivot_wider(names_from = interval_name_col, values_from = exclude)

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
  reshaped_results <- all_aucs  %>%
    # Define the number of decimals to round the results
    mutate(across(where(is.numeric), ~ round(.x, 3)))  %>%
    ungroup()

  return(reshaped_results)
}




add_lambda_details <- function(conc_data, infinite_aucs) {
  result <- conc_data %>%
    merge(infinite_aucs) %>%
    group_by(STUDYID, PCSPEC, ANALYTE, USUBJID, DOSNO) %>%
    arrange(STUDYID, PCSPEC, ANALYTE, USUBJID, DOSNO, IX) %>%
    # Deduce if the user performed an exclusion/selection
    # to indicate if the slope is manually selected
    mutate(
      lambda_z_method = ifelse(any(is.excluded.hl) | any(is.included.hl), "Manual", "Best slope")
    ) %>%
    # Filter out the rows that do not have relation with
    # lambda calculation (when calculated) and derive the IX
    filter(!exclude_half.life | is.na(lambda.z.time.first) | is.na(lambda.z.n.points)) %>%
    filter(TIME >= (lambda.z.time.first + start) | is.na(lambda.z.time.first)) %>%
    filter(n() <= lambda.z.n.points | is.na(lambda.z.n.points)) %>%
    mutate(lambda_z_ix = paste0(IX, collapse = ",")) %>%
    mutate(lambda_z_ix = ifelse(is.na(lambda.z), NA, lambda_z_ix)) %>%
    slice(1) %>%
    select(any_of(c(names(infinite_aucs), "lambda_z_method", "lambda_z_ix")))

  return(result)
}

add_lambda_details <- function(conc_data = res_nca$data$conc$data, infinite_aucs) {
  # Merge the datasets
  merged_data <- conc_data %>%
    merge(infinite_aucs)

  # Identify unique intervals from the column names
  intervals <- unique(gsub(
    ".*_(\\d+-\\d+)$",
    "\\1",
    grep("_\\d+-\\d+$", names(infinite_aucs), value = TRUE)
  ))

  if (length(intervals) == 0) intervals <- c("")

  # Initialize an empty list to store results for each interval
  results_list <- list()

  for (interval in intervals) {
    interval_suffix <- if (interval == "") "" else paste0("_", interval)

    result <- merged_data %>%
      group_by(STUDYID, PCSPEC, ANALYTE, USUBJID, DOSNO) %>%
      arrange(STUDYID, PCSPEC, ANALYTE, USUBJID, DOSNO, IX) %>%
      # Deduce if the user performed an exclusion/selection to
      # indicate if the slope is manually selected
      mutate(
        !!paste0("lambda_z_method", interval_suffix) := ifelse(
          any(get(paste0("is.excluded.hl", interval_suffix))) |
            any(get(paste0("is.included.hl", interval_suffix))),
          "Manual",
          "Best slope"
        )
      ) %>%
      # Filter out the rows that do not have relation with
      # lambda calculation (when calculated) and derive the IX
      filter(
        !get(paste0("exclude_half.life", interval_suffix)) |
          is.na(get(paste0("lambda.z.time.first", interval_suffix))) |
          is.na(get(paste0("lambda.z.n.points", interval_suffix)))
      ) %>%
      filter(
        TIME >= (get(paste0("lambda.z.time.first", interval_suffix)) + start) |
          is.na(get(paste0("lambda.z.time.first", interval_suffix)))
      ) %>%
      filter(
        n() <= get(paste0("lambda.z.n.points", interval_suffix)) |
          is.na(get(paste0("lambda.z.n.points", interval_suffix)))
      ) %>%
      mutate(!!paste0("lambda_z_ix", interval_suffix) := paste0(IX, collapse = ",")) %>%
      mutate(
        !!paste0("lambda_z_ix", interval_suffix) := ifelse(
          is.na(get(paste0("lambda.z", interval_suffix))),
          NA,
          get(paste0("lambda_z_ix", interval_suffix))
        )
      ) %>%
      slice(1) %>%
      select(any_of(c(
        names(infinite_aucs),
        paste0("lambda_z_method", interval_suffix),
        paste0("lambda_z_ix", interval_suffix)
      )))

    results_list[[interval]] <- result
  }

  # Combine results for all intervals
  combined_result <- reduce(
    results_list,
    full_join,
    by = c("STUDYID", "PCSPEC", "ANALYTE", "USUBJID", "DOSNO")
  )

  return(combined_result)
}
