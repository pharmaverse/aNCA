#' Reshape PKNCA Results
#'
#' This function reshapes the structure of the results produced by the main function
#' of the PKNCA package (pk.nca) in a way that each row represents all the main results
#' summarized for each profile in each individual/subject. Excluding the ID variables,
#' each column name corresponds with a calculated parameter and between brackets its
#' corresponding units. AUC intervals, if present, are be added as additional columns.
#'
#' @param myres The output of PKNCA::pk.nca. It makes some additional assumptions:
#'   1) CDISC denomination of actual and nominal time variables (AFRLT, ARRLT, NFRLT, NRRLT).
#'   2) Intervals must include a column (`type_interval`) to differentiate between the
#'   custom AUC ranges ("manual") and main parameter calculations ("main").
#'   3) Includes `PPSTRES` and `PPSTRESU` variables in results dataset.
#'   4) Columns `start_dose` and `end_dose` must express the actual start and end times
#'   of the dose, relative to the last reference dose.
#'   5) Temporarily: CDISC denomination of PK parameters related to half-life: "LAMZNPT",
#'   "LAMZLL", "LAMZ" Used to derive `LAMZNPT` and `LAMZMTD`.
#'
#' @returns A data frame which provides an easy overview on the results from the NCA
#'          in each profile/subject and how it was computed lambda (half life) and the results
#'          of the NCA parameters (cmax, AUC, AUClast)
#'
#' @importFrom dplyr select left_join rename mutate distinct group_by arrange ungroup
#' @importFrom dplyr filter slice across where
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom purrr pmap_chr
#' @export
#'
pivot_wider_pknca_results <- function(myres) {

  ############################################################################################
  # Derive LAMZNPT & LAMZMTD
  # ToDo: At some point this will be integrated in PKNCA and will need to be removed//modified
  conc_groups <- unname(unlist(myres$data$conc$columns$groups))
  time_col <- myres$data$conc$columns$time
  data_with_duplicates <- dose_profile_duplicates(
    myres$data$conc$data,
    c(unlist(unname(myres$data$conc$columns$groups)),
      "DOSNO")
  )

  added_params <- NULL
  if (all(c("LAMZ",
            "LAMZNPT",
            "LAMZLL") %in% unique(myres$result$PPTESTCD))) {
    added_params <- myres$result %>%
      filter(PPTESTCD %in% c("LAMZNPT", "LAMZLL", "LAMZ"),
             type_interval == "main") %>%
      select(conc_groups, PPTESTCD, PPSTRES, DOSNO, start, end) %>%
      unique() %>%
      pivot_wider(names_from = PPTESTCD, values_from = PPSTRES) %>%
      left_join(data_with_duplicates, by = intersect(names(.), names(data_with_duplicates))) %>%
      # Derive LAMZIX: If present consider inclusions and disconsider exclusions
      group_by(!!!syms(conc_groups), DOSNO) %>%
      # Derive LAMZMTD: was lambda.z manually customized?
      mutate(LAMZMTD = ifelse(
        any(is.excluded.hl) | any(is.included.hl), "Manual", "Best slope"
      )) %>%
      filter(!exclude_half.life | is.na(LAMZLL) | is.na(LAMZNPT)) %>%
      filter(!!sym(time_col) >= (LAMZLL + start) | is.na(LAMZLL)) %>%
      filter(row_number() <= LAMZNPT | is.na(LAMZNPT)) %>%
      mutate(LAMZIX = paste0(IX, collapse = ",")) %>%
      mutate(LAMZIX = ifelse(is.na(LAMZ), NA, LAMZIX)) %>%
      select(conc_groups, DOSNO, start, end, LAMZIX, LAMZMTD) %>%
      unique()
  }
  ############################################################################################

  # Pivot main interval columns by Parameter and consider each exclude column separately
  main_intervals_vals <- myres$result %>%
    distinct() %>%
    filter(type_interval == "main")  %>%
    mutate(PPTESTCD = ifelse(PPSTRESU != "",
                             paste0(PPTESTCD, "[", PPSTRESU, "]"),
                             PPTESTCD)) %>%
    select(-PPSTRESU, -PPORRES, -PPORRESU, -exclude, -type_interval) %>%
    pivot_wider(names_from = PPTESTCD, values_from = PPSTRES)

  main_intervals_exclude <- myres$result %>%
    distinct() %>%
    filter(type_interval == "main") %>%
    select(-PPSTRES, -PPSTRESU, -PPORRES, -PPORRESU, -type_interval)  %>%
    pivot_wider(names_from = PPTESTCD, values_from = exclude, names_prefix = "exclude.")

  main_intervals <- left_join(
    main_intervals_vals,
    main_intervals_exclude,
    by = intersect(names(main_intervals_vals), names(main_intervals_exclude))
  )

  # If present: Pivot manual AUC interval columns and their respective exclude column
  if (any(myres$result$type_interval == "manual")) {

    manual_aucs_vals <- myres$result %>%
      filter(type_interval == "manual", startsWith(PPTESTCD, "AUCINT")) %>%
      mutate(
        interval_name = paste0(signif(start), "-", signif(end)),
        interval_name_col = paste0(PPTESTCD, "_", interval_name)
      ) %>%
      select(-exclude, -PPSTRESU, -PPORRES, -PPORRESU, -start, -end, -start_dose, -end_dose,
             -PPTESTCD, -interval_name, -type_interval) %>%
      pivot_wider(names_from = interval_name_col,
                  values_from = PPSTRES)

    manual_aucs_exclude <- myres$result %>%
      filter(type_interval == "manual", startsWith(PPTESTCD, "AUCINT")) %>%
      mutate(
        interval_name = paste0(signif(start_dose), "-", signif(end_dose)),
        interval_name_col = paste0("exclude.", PPTESTCD, "_", interval_name)
      ) %>%
      select(-PPSTRES, -PPSTRESU, -PPORRES, -PPORRESU, -start, -end, -start_dose, -end_dose,
             -PPTESTCD, -interval_name, -type_interval) %>%
      pivot_wider(names_from = interval_name_col, values_from = exclude)

    manual_aucs <- inner_join(
      manual_aucs_vals,
      manual_aucs_exclude,
      by = intersect(names(manual_aucs_vals), names(manual_aucs_exclude))
    )

    # If present: Merge main and manual intervals together
    all_aucs <- left_join(
      main_intervals,
      manual_aucs,
      by = intersect(names(main_intervals), names(manual_aucs))
    )
  } else {
    all_aucs <- main_intervals
  }

  # If derived: Merge lambda.z.ix & lambda.z.method
  if (!is.null(added_params))
    all_aucs <- left_join(
      all_aucs,
      added_params,
      by = intersect(names(all_aucs), names(added_params))
    )

  # Do a final standardization of the results reshaped
  pivoted_res <- all_aucs  %>%
    mutate(Exclude = pmap_chr(across(starts_with("exclude.")), .extract_exclude_values)) %>%
    select(-starts_with("exclude."), -start_dose, -end_dose) %>%
    # Define the number of decimals to round the results
    mutate(across(where(is.numeric), ~ round(.x, 3)))  %>%
    ungroup()

  # Add "label" attribute to columns
  .add_label_attribute(pivoted_res, myres)
}

#' Helper function to extract exclude values
#' @noRd
.extract_exclude_values <- function(...) {
  raw_values <- unique(c(...))  # Get unique exclude values from different columns
  raw_values <- raw_values[!is.na(raw_values)]  # Remove NAs

  # Split each entry into individual phrases using "; " as a separator
  split_values <- unlist(strsplit(raw_values, "; "))

  # Remove duplicate messages
  unique_values <- unique(trimws(split_values))

  if (length(unique_values) == 0) NA_character_ else paste(unique_values, collapse = ", ")
}

#' Helper function to add "label" attribute to columns based on parameter names
#' @noRd
.add_label_attribute <- function(df, myres) {

  mapping_vr <- myres$result %>%
    mutate(PPTESTCD_unit = ifelse(PPSTRESU != "", paste0(PPTESTCD, "[", PPSTRESU, "]"), PPTESTCD),
           PPTESTCD_cdisc = gsub("\\$", "", translate_terms(PPTESTCD,
                                                            mapping_col = "PPTESTCD",
                                                            target_col = "PPTEST"))) %>%
    select(PPTESTCD_cdisc, PPTESTCD_unit) %>%
    distinct() %>%
    pull(PPTESTCD_cdisc, PPTESTCD_unit)

  mapping_cols <- intersect(names(df), names(mapping_vr))
  attrs <- unname(mapping_vr[mapping_cols])

  df[, mapping_cols] <- as.data.frame(mapply(function(col, bw) {
    attr(col, "label") <- bw
    col
  }, df[, mapping_cols], attrs, SIMPLIFY = FALSE))
  df
}
