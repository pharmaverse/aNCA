#' Calculate Matrix Ratios
#' This function calculates the  ratios for a given data set,
#' based on the shared time points for each matrix concentration sample.
#' The user can input multiple tissues for which ratios should be calculated.
#'
#' The ratios are calculated as specimen1 / specimen 2.
#'
#' @param data A data frame containing the concentration data.
#' @param matrix_col A character string specifying the column name for the matrix type.
#' @param conc_col A character string specifying the column name for the concentration data.
#' @param units_col A character string specifying the column name for the units.
#' @param groups A character vector of grouping variables to use for the analysis.
#'                  Must include time column, USUBJID, and optionally, other grouping variables.
#' @param spec1 A character string specifying the value for
#'               the first specimen type(s) in the matrix_col.
#' @param spec2 A character string specifying the value for
#'               the second specimen type(s) in the matrix_col.
#'
#' @returns A data frame containing the ratios.
#' @examples
#' data <- data.frame(
#'   USUBJID = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A"),
#'   NFRLT = c(0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2),
#'   MATRIX = c(
#'     "BLOOD", "BLOOD", "BLOOD", "PLASMA", "PLASMA", "PLASMA",
#'     "BRAIN", "BRAIN", "BRAIN", "LIVER", "LIVER", "LIVER"
#'   ),
#'   CONC = c(10, 20, 15, 25, 30, 40, 5, 10, 8, 12, 18, 16),
#'   UNITS = rep("ng/mL", 12)
#' )
#' multiple_matrix_ratios(data, "MATRIX", "CONC", "UNITS", c("NFRLT", "USUBJID"), "BLOOD", "PLASMA")
#'
#' @import dplyr
#' @export
multiple_matrix_ratios <- function(data, matrix_col, conc_col, units_col,
                                   groups = c("NFRLT", "USUBJID"),
                                   spec1, spec2) {
  # Separate Samples
  df_spec1 <- data %>%
    filter(!!sym(matrix_col) %in% spec1) %>%
    rename(
      Spec1_Value = !!sym(conc_col),
      Spec1_Label = !!sym(matrix_col),
      Spec1_Units = !!sym(units_col)
    ) %>%
    select(all_of(groups), Spec1_Value, Spec1_Label, Spec1_Units)

  df_spec2 <- data %>%
    filter(!!sym(matrix_col) %in% spec2) %>%
    rename(
      Spec2_Value = !!sym(conc_col),
      Spec2_Label = !!sym(matrix_col),
      Spec2_Units = !!sym(units_col)
    ) %>%
    select(all_of(groups), Spec2_Value, Spec2_Label, Spec2_Units)

  # Merge Data
  left_join(df_spec1, df_spec2, by = groups, relationship = "many-to-many") %>%
    filter(!is.na(Spec1_Value) & !is.na(Spec2_Value)) %>%
    rowwise() %>%
    mutate(
      Ratio_Type = paste0(Spec1_Label, "/", Spec2_Label),
      Ratio = Spec1_Value / Spec2_Value,
      Ratio = signif(Ratio, 3)
    ) %>%
    filter(Spec1_Label != Spec2_Label) %>%
    select(all_of(groups), Ratio_Type, Spec1_Value, Spec1_Units, Spec2_Value, Spec2_Units, Ratio)
}

#' Calculate Ratios from PKNCA Results
#'
#' @param data A PKNCAresults object or its result data.frame.
#' @param test_parameter Character. The PPTESTCD value to use as test (numerator).
#' @param ref_parameter Character. The PPTESTCD value to use as reference (denominator).
#' Defaults to test_parameter.
#' @param match_cols Character vector of column names to match between test and reference groups
#'  or a data.frame specifying columns and values.
#' @param ref_groups A data.frame specifying reference groups.
#' At its minimum, contains the contrast variable value(s) for the reference.
#' @param test_groups A data.frame specifying test groups. Optional.
#' By default is NULL, allowing rows not in ref_groups be used as test.
#' @param adjusting_factor Numeric to multiply the ratio. Default is 1.
#' @param custom_pptestcd Optional character. If provided, will be used as the PPTESTCD value.
#' @returns A data.frame result object with the calculated ratios.
#' @export
calculate_ratios <- function(
    data,
    test_parameter,
    ref_parameter = test_parameter,
    match_cols,
    ref_groups,
    test_groups = NULL,
    adjusting_factor = 1,
    custom_pptestcd = NULL) {
  UseMethod("calculate_ratios", data)
}

#' @export
calculate_ratios.data.frame <- function(
    data,
    test_parameter,
    ref_parameter = test_parameter,
    match_cols,
    ref_groups,
    test_groups = NULL,
    adjusting_factor = 1,
    custom_pptestcd = NULL) {
  if (!any(data$PPTESTCD == test_parameter)) {
    warning(
      paste0(
        "No test_parameter with PPTESTCD: '",
        paste(test_parameter, collapse = ","),
        "' found in the PKNCA results."
      )
    )
  }
  if (!any(data$PPTESTCD == ref_parameter)) {
    warning(
      paste0(
        "No ref_parameter with PPTESTCD: '",
        paste(ref_parameter, collapse = ","),
        "' found in the PKNCA results."
      )
    )
  }

  # Deduce what are all the group columns in the result data.frame
  extra_res_cols <- c(
    "PPTEST", "PPORRES", "PPORRESU", "PPSTRES", "PPSTRESU", "type_interval", "exclude"
  )
  group_cols <- setdiff(colnames(data), extra_res_cols)

  # Define the reference and test data based on the parameters and groups
  df_ref <- as.data.frame(data)[data$PPTESTCD == ref_parameter, , drop = FALSE]
  df_ref <- merge(df_ref, ref_groups)

  df_test <- as.data.frame(data)[data$PPTESTCD == test_parameter, , drop = FALSE]
  df_test <- {
    if (!is.null(test_groups)) {
      merge(df_test, test_groups)
    } else {
      anti_join(df_test, ref_groups)
    }
  }

  # Define the key column(s) to use for group reference in PPANMETH
  ref_cols <- colnames(ref_groups[, !names(ref_groups) %in% match_cols, drop = FALSE])

  # Join test and denominator by their matching columns
  merge(df_test, df_ref, by = c(match_cols), suffixes = c("", "_ref")) %>%
    # If possible compute conversion factors for the units of test and denominator
    mutate(
      PPORRESU_factor = get_conversion_factor(PPORRESU_ref, PPORRESU),
      PPSTRESU_factor = if ("PPSTRESU" %in% names(.)) {
        get_conversion_factor(PPSTRESU_ref, PPSTRESU)
      } else {
        NULL
      },
      PPORRES_ref = ifelse(!is.na(PPORRESU_factor), PPORRES_ref * PPORRESU_factor, PPORRES_ref),
      PPSTRES_ref = if ("PPSTRESU" %in% names(.)) {
        ifelse(!is.na(PPSTRESU_factor), PPSTRESU_factor * PPSTRES_ref, PPSTRES_ref)
      } else {
        NULL
      }
    ) %>%
    group_by(across(any_of(c(match_cols, group_cols, "PPTESTCD")))) %>%
    unique() %>%
    # Use mean values in case of multiple denominator rows per test
    mutate(
      PPORRES_ref = mean(PPORRES_ref, na.rm = TRUE),
      PPSTRES_ref = if ("PPSTRESU" %in% names(.)) {
        mean(PPSTRES_ref, na.rm = TRUE)
      } else {
        NULL
      },
      n = n()
    ) %>%
    ungroup() %>%
    mutate(
      PPORRES = (PPORRES / PPORRES_ref) * adjusting_factor,
      PPSTRES = if ("PPSTRES" %in% names(.) & "PPSTRES_ref" %in% names(.)) {
        (PPSTRES / PPSTRES_ref) * adjusting_factor
      } else {
        NULL
      },
      PPORRESU = ifelse(!is.na(PPORRESU_factor), "fraction", paste0(PPORRESU, "/", PPORRESU_ref)),
      PPSTRESU = if ("PPSTRESU" %in% names(.)) {
        ifelse(!is.na(PPORRESU_factor), "fraction", paste0(PPSTRESU, "/", PPSTRESU_ref))
      } else {
        NULL
      }
    ) %>%
    rowwise() %>%
    mutate(
      ppanmeth_test_groups = paste0(
        paste(paste(ref_cols, c_across(all_of(ref_cols)), sep = ": "), collapse = ", ")
      ),
      ppanmeth_ref_groups = paste0(
        paste(
          paste(paste0(ref_cols), c_across(all_of(paste0(ref_cols, "_ref"))), sep = ": "),
          collapse = ", "
        )
      )
    ) %>%
    ungroup() %>%
    mutate(
      PPANMETH = ifelse(
        ppanmeth_test_groups == ppanmeth_ref_groups,
        paste0(PPTESTCD, " TO ", PPTESTCD_ref),
        paste0(PPTESTCD, " TO ", PPTESTCD_ref, " [", ppanmeth_ref_groups, "]")
      ),
      PPTESTCD = if (!is.null(custom_pptestcd)) {
        custom_pptestcd
      } else {
        ifelse(n > 1, paste0("RA", test_parameter, " (mean)"), paste0("RA", test_parameter))
      }
    ) %>%
    # Make sure all basic columns are still character (even when empty #730)
    mutate(
      PPORRESU = as.character(PPORRESU),
      PPSTRESU = if ("PPSTRESU" %in% names(.)) {
        as.character(PPSTRESU)
      } else {
        NULL
      },
      PPANMETH = as.character(PPANMETH)
    ) %>%
    # Keep same format as the input (PKNCAresults)
    select(any_of(c(names(df_test), "PPANMETH"))) %>%
    unique()
}

#' @export
calculate_ratios.PKNCAresults <- function(
    data,
    test_parameter,
    ref_parameter = test_parameter,
    match_cols,
    ref_groups,
    test_groups = NULL,
    adjusting_factor = 1,
    custom_pptestcd = NULL) {
  # Check if match_cols and ref_groups are valid group columns
  # Make checks on the input formats
  cols_used_for_ratios <- c(match_cols, names(ref_groups), names(test_groups))
  if (!all(cols_used_for_ratios %in% c(names(PKNCA::getGroups(data)), "start", "end"))) {
    stop(paste0(
      "match_cols and ref_groups must contain valid group column names in PKNCAres: ",
      paste(names(PKNCA::getGroups(data)), collapse = ", ")
    ))
  }
  if (!"PPANMETH" %in% names(data$result)) {
    data$result$PPANMETH <- ""
  }

  # Calculate ratios using the data.frame method
  ratios_result <- calculate_ratios.data.frame(
    data = data$result,
    test_parameter = test_parameter,
    ref_parameter = ref_parameter,
    match_cols = match_cols,
    ref_groups = ref_groups,
    test_groups = test_groups,
    adjusting_factor = adjusting_factor,
    custom_pptestcd = custom_pptestcd
  )

  # Update the PKNCA results with the new ratios
  data$result <- bind_rows(data$result, ratios_result)
  data
}

#' Apply Ratio Calculations to PKNCAresult Object
#'
#' This function takes a PKNCAresult object and a data.frame specifying ratio calculations.
#'
#' @param res A PKNCAresult object.
#' @param ratio_table Data.frame with columns:
#' TestParameter, RefParameter, Reference, Test, AggregateSubject,
#' AdjustingFactor, TestGroups, RefGroups, PPTESTCD.
#' @returns The updated PKNCAresult object with added rows in the `result` data.frame.
#' @export
calculate_table_ratios <- function(res, ratio_table) {
  # Use lapply to process each row of ratio_table
  ratio_results <- lapply(seq_len(nrow(ratio_table)), function(i) {
    result <- calculate_ratio_app(
      res = res,
      test_parameter = ratio_table$TestParameter[i],
      ref_parameter = ratio_table$RefParameter[i],
      test_group = ratio_table$TestGroups[i],
      ref_group = ratio_table$RefGroups[i],
      aggregate_subject = ratio_table$AggregateSubject[i],
      adjusting_factor = as.numeric(ratio_table$AdjustingFactor[i]),
      custom_pptestcd = if (ratio_table$PPTESTCD[i] == "") NULL else ratio_table$PPTESTCD[i]
    )
    if (nrow(result) == 0) {
      warning(
        "Ratio ", ratio_table$PPTESTCD[i], " not computed.",
        "No comparable groups found between RefGroups",
        " (", ratio_table$RefGroups[i], ")",
        "and TestGroups",
        " (", ratio_table$TestGroups[i], ")"
      )
    }
    result
  })
  if (!"PPANMETH" %in% names(res$result)) {
    res$result$PPANMETH <- ""
  }

  # Combine all results into the original PKNCAresult object
  res$result <- do.call(bind_rows, c(list(res$result), ratio_results))
  res
}

#' Parse an interval parameter name with range suffix
#'
#' Detects whether a parameter string contains a range suffix (e.g. `AUCINT_0-20`)
#' and extracts the base PPTESTCD, start, and end values.
#'
#' @param param Character. Parameter name, possibly with range suffix.
#' @returns A list with `base` (character), `start` (numeric), `end` (numeric),
#'   and `is_interval` (logical). When `is_interval` is FALSE, `start` and `end`
#'   are NULL.
#' @noRd
parse_interval_parameter <- function(param) {
  m <- regmatches(param, regexec("^(.+)_(\\d+\\.?\\d*)-(\\d+\\.?\\d*)$", param))[[1]]
  if (length(m) == 4) {
    list(
      base = m[2],
      start = as.numeric(m[3]),
      end = as.numeric(m[4]),
      is_interval = TRUE
    )
  } else {
    list(base = param, start = NULL, end = NULL, is_interval = FALSE)
  }
}

#' Links the table ratio of the App with the ratio calculations via PKNCA results
#'
#' @param res A PKNCAresult object.
#' @param test_parameter Character. The PPTESTCD value to use as test (numerator).
#'   May include a range suffix (e.g. `AUCINT_0-20`) or a custom interval label.
#' @param ref_parameter Character. The PPTESTCD value to use as reference (denominator).
#'   Defaults to test_parameter. May include a range suffix or custom interval label.
#' @param test_group Character. The test group (numerator). Default is "(all other levels)".
#' @param ref_group Character. The reference group (denominator).
#' @param aggregate_subject Character. Aggregation mode: "yes", "no", or "if-needed".
#' @param adjusting_factor Numeric that multiplies the calculated ratio. Default is 1.
#' @param custom_pptestcd Optional character. If provided, will be used as the PPTESTCD value.
#' @returns A data.frame with the calculated ratios for the specified settings.
calculate_ratio_app <- function(
    res,
    test_parameter,
    ref_parameter = test_parameter,
    test_group = "(all other levels)",
    ref_group = "PARAM: Analyte01",
    aggregate_subject = "no",
    adjusting_factor = 1,
    custom_pptestcd = NULL) {
  test <- .resolve_ratio_parameter(res$result, test_parameter)
  ref <- .resolve_ratio_parameter(res$result, ref_parameter)
  test_parsed <- test$parsed
  ref_parsed <- ref$parsed
  result_data <- bind_rows(test$data, ref$data) %>% distinct()

  match_cols <- .build_ratio_match_cols(
    res, ref_group, aggregate_subject, test_parsed, ref_parsed
  )

  parsed_groups <- .parse_ratio_groups(ref_group, test_group)
  test_groups <- parsed_groups$test_groups
  ref_groups <- parsed_groups$ref_groups

  ratio_list <- lapply(seq_along(match_cols), function(ix) {
    calculate_ratios(
      data = result_data,
      test_parameter = test_parameter,
      ref_parameter = ref_parameter,
      match_cols = match_cols[[ix]],
      ref_groups = ref_groups,
      test_groups = test_groups,
      adjusting_factor = adjusting_factor,
      custom_pptestcd = custom_pptestcd
    )
  })
  ratio_list <- Filter(function(x) nrow(x) > 0, ratio_list)
  all_ratios <- bind_rows(ratio_list)

  if (nrow(all_ratios) == 0) {
    return(all_ratios)
  }

  # A ratio of manual intervals has its own label, not its numerator's label.
  if ("interval_name" %in% names(all_ratios)) {
    manual <- which(all_ratios$type_interval == "manual")
    all_ratios$interval_name[manual] <- all_ratios$PPTESTCD[manual]
  }

  # For if-needed: individual results (first in ratio_list) are preferred over
  # aggregated ones. Dedup by subject + group without PPTESTCD so that individual
  # (RACMAX) and aggregated (RACMAX (mean)) rows are treated as duplicates.
  all_ratios %>%
    distinct(
      across(all_of(c(group_vars(res$data), "end"))),
      .keep_all = TRUE
    )
}

#' Build match_cols for ratio calculation based on grouping, aggregation, and interval settings
#' @importFrom utils head
#' @noRd
.build_ratio_match_cols <- function(res, ref_group, aggregate_subject, test_parsed, ref_parsed) {
  reference_colname <- gsub("(.*): (.*)", "\\1", ref_group)
  match_cols <- setdiff(unique(c(dplyr::group_vars(res), "start", "end")), reference_colname)

  # Remove start/end when they cannot serve as reliable join keys
  atptref_exists <- "ATPTREF" %in% reference_colname
  is_interval <- test_parsed$is_interval || ref_parsed$is_interval
  if (atptref_exists || aggregate_subject != "no" || is_interval) {
    match_cols <- setdiff(match_cols, c("start", "end"))
  }
  # Interval bounds differ between numerator and denominator, but their dose
  # profiles must still match unless the ratio explicitly compares profiles.
  if (is_interval) {
    profile_col <- head(intersect(c("ATPTREF", "DOSNOA"), names(res$result)), 1)
    match_cols <- union(match_cols, setdiff(profile_col, reference_colname))
  }

  switch(aggregate_subject,
    "yes" = list(setdiff(match_cols, "USUBJID")),
    "no" = {
      if (!"USUBJID" %in% match_cols) {
        stop("USUBJID must be included in match_cols when aggregate_subject is 'never'.")
      }
      list(match_cols)
    },
    "if-needed" = {
      has_usubjid <- "USUBJID" %in% match_cols
      match_cols <- list(match_cols)
      if (has_usubjid) {
        match_cols <- c(match_cols, list(setdiff(match_cols[[1]], "USUBJID")))
      }
      match_cols
    }
  )
}

#' Parse ref_group and test_group strings into data frames for ratio merging
#' @noRd
.parse_ratio_groups <- function(ref_group, test_group) {
  ref_colname <- gsub("(.*): (.*)", "\\1", ref_group)
  ref_value <- gsub("(.*): (.*)", "\\2", ref_group)
  ref_groups <- data.frame(
    matrix(ref_value, nrow = 1, ncol = length(ref_colname),
           dimnames = list(NULL, ref_colname))
  )

  if (test_group == "(all other levels)") {
    test_groups <- NULL
  } else {
    test_colname <- gsub("(.*): (.*)", "\\1", test_group)
    test_value <- gsub("(.*): (.*)", "\\2", test_group)
    test_groups <- data.frame(
      matrix(test_value, nrow = 1, ncol = length(test_colname),
             dimnames = list(NULL, test_colname))
    )
  }

  list(test_groups = test_groups, ref_groups = ref_groups)
}

# Resolve each side independently so different ranges of the same parameter
# cannot be pooled into the denominator. Keep the selected label as the match
# key, while the source rows supply the original interval and profile metadata.
.resolve_ratio_parameter <- function(data, parameter) {
  parsed <- parse_interval_parameter(parameter)
  manual <- if ("type_interval" %in% names(data)) {
    !is.na(data$type_interval) & data$type_interval == "manual"
  } else {
    rep(FALSE, nrow(data))
  }
  labels <- rename_interval_params(data)$PPTESTCD
  start <- if ("start_dose" %in% names(data)) data$start_dose else data$start
  end <- if ("end_dose" %in% names(data)) data$end_dose else data$end
  standard <- .interval_names(data$PPTESTCD, start, end)
  matches <- manual & (labels == parameter | standard == parameter)
  # A qualified option may come from settings for several study types even
  # when the current result contains only one of them.
  if ("interval_name" %in% names(data)) {
    qualified <- paste0(trimws(data$interval_name), " (", standard, ")")
    matches <- matches | (manual & qualified == parameter)
  }
  if (!any(matches) && "interval_name" %in% names(data)) {
    matches <- manual & !is.na(data$interval_name) & trimws(data$interval_name) == parameter
    if (length(unique(labels[matches])) > 1) {
      stop("Interval name '", parameter, "' matches different intervals. Select one of: ",
           paste(unique(labels[matches]), collapse = ", "), call. = FALSE)
    }
  }
  if (any(matches)) {
    parsed$is_interval <- TRUE
  } else if (parsed$is_interval) {
    matches <- data$PPTESTCD == parsed$base & start == parsed$start & end == parsed$end
  } else {
    matches <- data$PPTESTCD == parameter
  }
  selected <- data[which(matches), , drop = FALSE]
  selected$PPTESTCD <- rep(parameter, nrow(selected))
  list(data = selected, parsed = parsed)
}
