#' Shared table builder for ADPP summary tables (pkpt03 / pkpt08 pattern).
#'
#' Deduplicates to one row per subject x stratum, then applies `summary_fn` to
#' the numeric values for each stratum.  A "stratum" is the combination of every
#' column in `strat_vars` -- the parameter column (`PARAM`) is just one more
#' stratification variable, so the row layout is fully controlled by the caller's
#' `strat_var` selection (see `t_pkpt03_col`).  Rows are grouped by the
#' `interaction()` of the (present) `strat_vars`, mirroring `t_pkct01()`.
#'
#' @param df Data frame (one split from `split_and_apply`).
#' @param strat_vars Character vector of columns whose combination defines the
#'   table rows.  Absent columns are silently dropped; an empty set yields a
#'   single summary row over all rows.
#' @param value_var Column name string of the numeric analysis value.
#' @param summary_fn Function that takes a numeric vector and returns a
#'   one-row `data.frame` of summary statistics.
#' @param col_group_var Optional column whose levels become side-by-side column
#'   groups: each statistic block is repeated once per level. `NULL` (default)
#'   produces the flat single-block table.
#' @param group_levels Ordered group levels (from [.resolve_col_group()]),
#'   shared across splits so every table has the same columns. Ignored when
#'   `col_group_var` is `NULL`.
#' @param stats Optional character vector of terse statistic names to keep
#'   (see [.select_stats()]). `NULL` (default) keeps every statistic.
#' @return A labeled `data.frame`. When `col_group_var` is set it also carries a
#'   `col_groups` attribute (level -> leaf column names) for the render layer.
#' @noRd
.build_pkpp_table <- function(df, strat_vars, value_var, summary_fn,
                              col_group_var = NULL, group_levels = NULL,
                              stats = NULL) {
  strat_vars <- intersect(strat_vars, names(df))

  if ("USUBJID" %in% names(df)) {
    # Include AVISIT in the dedup key when present so that rows from different
    # visits (genuinely different AVAL values) are kept.  AVISIT is absent from
    # single-interval ADPP; including it only when present is safe because
    # !duplicated() still collapses true within-visit duplicates (same
    # USUBJID × stratum × AVISIT repeated per dose event).  col_group_var is
    # added so a subject that is constant within the group is never split across
    # two group columns by the dedup.
    # The analyte and parameter columns are always part of the key, even when they are not
    # stratification variables: rows that differ in PPCAT or PARAM are genuinely different
    # measurements, and collapsing them silently discarded whole analytes (a parent/metabolite
    # pair reduced to the parent alone). Duplicate rows from repeated dose events share both,
    # so they still collapse as intended.
    dedup_cols <- intersect(
      c("USUBJID", strat_vars, "AVISIT", col_group_var, "PPCAT", "PARAM"),
      names(df)
    )
    df <- df[!duplicated(df[dedup_cols]), , drop = FALSE]
  }

  stat_block <- function(sub) {
    if (is.null(col_group_var)) {
      summary_fn(sub[[value_var]])
    } else {
      .pivot_group_blocks(
        sub, col_group_var, group_levels,
        block_fn = function(sd) summary_fn(sd[[value_var]])
      )
    }
  }

  if (length(strat_vars) == 0) {
    # No row-grouping variables: a single summary row over all rows.
    result <- stat_block(df)
  } else {
    # Coerce each grouping column to character (NA -> "NA") so interaction()
    # keeps rows with missing group values visible, mirroring t_pkct01().
    group_cols <- lapply(strat_vars, function(v) {
      x <- as.character(df[[v]])
      x[is.na(x)] <- "NA"
      x
    })
    groups <- do.call(interaction, c(group_cols, list(sep = " | ", drop = TRUE)))
    rows <- lapply(levels(groups), function(grp) {
      sub <- df[groups == grp, , drop = FALSE]
      if (nrow(sub) == 0) return(NULL)
      key <- sub[1, strat_vars, drop = FALSE]
      cbind(key, stat_block(sub), stringsAsFactors = FALSE)
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) return(data.frame())
    result <- do.call(rbind, rows)
    # Natural-aware ordering (embedded numbers sort numerically) so each stratum
    # is contiguous and "10 mg" sorts after "2 mg".
    order_keys <- lapply(strat_vars, function(v) .natural_sort_key(result[[v]]))
    result <- result[do.call(order, order_keys), , drop = FALSE]
    rownames(result) <- NULL
  }

  result <- .apply_stat_labels(apply_labels(result, type = "ADPP"))
  if (!is.null(col_group_var)) {
    attr(result, "col_groups") <-
      .make_col_groups(group_levels, names(summary_fn(numeric(0))))
  }
  .select_stats(result, stats)
}

#' Summary PK Parameters Table -- statistics in columns (pkpt03)
#'
#' Summarizes pharmacokinetic parameters from ADPP data. Returns one data frame
#' per analyte (PPCAT) combination with PK parameters as rows and descriptive
#' statistics as columns.
#'
#' @param data A CDISC ADPP data frame (from `export_cdisc()$adpp`).
#' @param list_vars Character vector of columns used to split output into
#'   separate tables. Default: `c("PPCAT")`.  `AVISIT` is a conditional ADPP
#'   column that is typically absent from `export_cdisc()$adpp`; it is silently
#'   skipped when not present so there is no need to remove it manually, but
#'   adding it only helps when your ADPP actually contains visit information.
#' @param strat_var One or more columns whose combination defines the table rows
#'   (stratification). Default: `c("TRT01A", "PARAM")` -- statistics are
#'   separated by treatment arm and parameter.  Add e.g. `"PCSPEC"` to also split
#'   by specimen.  Any variable that is also a `list_vars` (table-split) column is
#'   dropped from the rows, since it is constant within each split.
#' @param value_var Column containing the numeric analysis value. Default: `"AVAL"`.
#' @param param_filter Optional character vector of `PARAM` values to keep.
#'   `NULL` (default) keeps every parameter.
#' @param col_group_var Optional subject-level column (e.g. `"SEX"`, `"RACE"`)
#'   whose values become side-by-side comparison column groups: the full
#'   statistic block is repeated once per level, nested under a group header.
#'   `NULL` (default) produces the standard flat table. Must differ from
#'   `strat_var` and the `list_vars`.
#' @param stats Optional character vector of statistics to display, chosen from
#'   `c("n", "Mean", "SD", "CV_pct", "GeoMean", "GeoCV_pct", "Median", "Min",
#'   "Max")`. `NULL` (default) shows all of them. Names not produced by this
#'   table are ignored.
#'
#' @return A named list of data frames, one per combination of `list_vars`.
#'   Each data frame has one column per `strat_var` followed by the statistics:
#'   `n`, `Mean`, `SD`, `CV_pct`, `GeoMean`, `GeoCV_pct`, `Median`, `Min`, `Max`.
#'   When `col_group_var` is set, the statistic columns are prefixed per group
#'   level and a `col_groups` attribute drives the rendered two-level header.
#'
#' @examples
#' \dontrun{
#' adpp <- export_cdisc(res_nca)$adpp
#' tables <- t_pkpt03_col(adpp)
#' tables[[1]]
#' # Separate statistics by specimen too:
#' t_pkpt03_col(adpp, strat_var = c("TRT01A", "PARAM", "PCSPEC"))[[1]]
#' # Compare sexes side by side:
#' t_pkpt03_col(adpp, col_group_var = "SEX")[[1]]
#' }
#'
#' @importFrom stats sd median
#' @export
t_pkpt03_col <- function(
  data,
  list_vars  = c("PPCAT"),
  strat_var  = c("TRT01A", "PARAM"),
  value_var  = "AVAL",
  param_filter = NULL,
  col_group_var = NULL,
  stats = NULL
) {
  if (!value_var %in% names(data)) {
    stop("t_pkpt03_col: missing required column: ", value_var)
  }

  data <- filter_summary_excluded(data)

  if (!is.null(param_filter) && length(param_filter) > 0 && "PARAM" %in% names(data)) {
    data <- data[data$PARAM %in% param_filter, , drop = FALSE]
  }

  if (nrow(data) == 0) return(list(data.frame()))

  # A table-split (list_vars) column is constant within each split, so keeping it
  # on the rows only adds a redundant constant column.
  split_strat <- intersect(strat_var, list_vars)
  if (length(split_strat) > 0) {
    .tlg_warn(
      "t_pkpt03_col: stratification variable(s) also used to split tables and dropped from the ",
      "rows: ", paste(split_strat, collapse = ", "),
      ". Within a split these are constant; the value is shown in the group header instead."
    )
  }
  strat_var <- setdiff(strat_var, list_vars)
  missing_strat <- setdiff(strat_var, names(data))
  if (length(missing_strat) > 0) {
    .tlg_warn(
      "t_pkpt03_col: stratification variable(s) not found in the data and skipped: ",
      paste(missing_strat, collapse = ", "),
      ". The table is grouped by the remaining variable(s) only."
    )
  }

  group_levels <- NULL
  if (!is.null(col_group_var)) {
    group_levels <- .resolve_col_group(
      col_group_var, data, reserved = c(strat_var, list_vars)
    )
  }

  split_and_apply(
    data, list_vars,
    function(df) {
      .build_pkpp_table(
        df, strat_var, value_var, .summarise_adpp,
        col_group_var = col_group_var, group_levels = group_levels,
        stats = stats
      )
    }
  )
}

#' @describeIn t_pkpt03_col Summary of metabolite-to-parent ratios (stats in columns).
#'   Filters to metabolite rows using `METABFL` (preferred) or, when absent from ADPP,
#'   falls back to rows where `PPCAT` or `PARAM` contains "metab" (case-insensitive).
#'   `METABFL` is present in ADPP only when it was included as a grouping variable in
#'   the NCA run.
#' @param ... Additional arguments forwarded to [t_pkpt03_col()].
#' @export
t_pkpt03_MP_col <- function(data, ...) { # nolint: object_name_linter
  t_pkpt03_col(filter_metabolite_rows(data, "t_pkpt03_MP_col"), ...)
}

#' Mean Dose-Normalized PK Parameters Table (pkpt07)
#'
#' Filters ADPP to dose-normalized parameters and summarizes them with the
#' same column layout as [t_pkpt03_col()].  These parameters must have been
#' computed during the NCA run -- they are not derived on the fly.
#'
#' @param data A CDISC ADPP data frame (from `export_cdisc()$adpp`).
#' @param paramcd_var Column containing parameter codes used to detect
#'   dose-normalized parameters. Default: `"PARAMCD"`.
#' @param paramcd_filter Character vector of CDISC dose-normalized PARAMCDs to
#'   keep.  Defaults to the standard codes used in this package:
#'   `c("CMAXD", "AUCLSTD", "AUCIFOD", "AUCTLSTD")`.  Pass `NULL` to fall
#'   back to the regex `grepl("[A-Z0-9]D$", PARAMCD)` pattern, which keeps
#'   any code whose last two characters are an uppercase letter/digit followed
#'   by `D`.
#' @inheritParams t_pkpt03_col
#'
#' @return Named list of data frames (same format as [t_pkpt03_col()]).
#'
#' @examples
#' \dontrun{
#' adpp <- export_cdisc(res_nca)$adpp
#' tables <- t_pkpt07_norm(adpp)
#' # Include a custom dose-normalized code:
#' tables <- t_pkpt07_norm(adpp, paramcd_filter = c("CMAXD", "AUCLSTD", "MYPARAMD"))
#' }
#'
#' @export
t_pkpt07_norm <- function(
  data,
  paramcd_var    = "PARAMCD",
  paramcd_filter = c("CMAXD", "AUCLSTD", "AUCIFOD", "AUCTLSTD"),
  list_vars      = c("PPCAT"),
  strat_var      = c("TRT01A", "PARAM"),
  value_var      = "AVAL",
  col_group_var  = NULL,
  stats          = NULL
) {
  data <- filter_summary_excluded(data)

  if (paramcd_var %in% names(data)) {
    if (!is.null(paramcd_filter)) {
      data <- data[data[[paramcd_var]] %in% paramcd_filter, , drop = FALSE]
    } else {
      # Fallback regex: last two chars are [A-Z0-9] then D (e.g. CMAXD, AUCLSTD).
      # The simple "D$" pattern was too broad — it matched any code ending in D,
      # including non-dose-normalized codes like AUCCUMD or study-specific codes.
      data <- data[grepl("[A-Z0-9]D$", data[[paramcd_var]]), , drop = FALSE]
    }
  } else {
    .tlg_warn(
      "t_pkpt07_norm: column '", paramcd_var, "' not found in data; ",
      "dose-normalization filter could not be applied. All parameters are ",
      "included. Ensure PARAMCD is exported from your NCA run to use this table."
    )
  }
  if (nrow(data) == 0) {
    stop(
      "t_pkpt07_norm: no dose-normalized parameters found in ADPP. ",
      "Include dose-normalized NCA parameters (e.g. Cmax/D, AUClast/D) ",
      "in your NCA parameter selection to use this table."
    )
  }
  t_pkpt03_col(
    data,
    list_vars = list_vars,
    strat_var = strat_var,
    value_var = value_var,
    col_group_var = col_group_var,
    stats = stats
  )
}

#' Mean Urine Amount and Percent Recovered Table (pkpt08)
#'
#' Filters ADPP to urine specimen records and summarizes cumulative amount
#' excreted (Ae) and percentage of dose recovered (Fe%) with descriptive
#' statistics in columns.  Per the TLG catalog specification for pkpt08,
#' the summary includes n, Mean, SD, CV%, Median, Min, Max -- without
#' geometric mean or geometric CV% (those are omitted because urine recovery
#' parameters are not log-normally distributed by convention).
#'
#' @param data A CDISC ADPP data frame. Urine records are identified by
#'   `PPSPEC %in% urine_specs`.
#' @param urine_specs Character vector of specimen types considered urine,
#'   matched case-insensitively. Default: `c("URINE")`.
#' @inheritParams t_pkpt03_col
#'
#' @return Named list of data frames with one column per `strat_var` followed by
#'   `n`, `Mean`, `SD`, `CV_pct`, `Median`, `Min`, `Max`.
#'   Use [t_pkpt03_col()] instead if geometric mean statistics are needed.
#'
#' @examples
#' \dontrun{
#' adpp <- export_cdisc(res_nca)$adpp
#' tables <- t_pkpt08_uri(adpp)
#' }
#'
#' @importFrom stats sd median
#' @export
t_pkpt08_uri <- function(
  data,
  urine_specs = c("URINE"),
  list_vars   = c("PPCAT"),
  strat_var   = c("TRT01A", "PARAM"),
  value_var   = "AVAL",
  param_filter = NULL,
  col_group_var = NULL,
  stats       = NULL
) {
  data <- filter_summary_excluded(data)

  if ("PPSPEC" %in% names(data)) {
    # Case-insensitive match (CDISC value is "URINE"; source casing varies).
    data <- dplyr::filter(data, toupper(.data$PPSPEC) %in% toupper(urine_specs))
  } else {
    .tlg_warn(
      "t_pkpt08_uri: 'PPSPEC' column not found in data; the urine specimen ",
      "filter was not applied. All rows are treated as urine. If your data ",
      "contains non-urine records, the output will be incorrect. Ensure ",
      "PPSPEC is present in the ADPP parameter data (from export_cdisc()$adpp)."
    )
  }
  if (!is.null(param_filter) && length(param_filter) > 0 && "PARAM" %in% names(data)) {
    data <- data[data$PARAM %in% param_filter, , drop = FALSE]
  }
  if (nrow(data) == 0) {
    stop(
      "t_pkpt08_uri: no urine PK parameter data found in ADPP. ",
      "Ensure urine NCA parameters (e.g. Ae, Fe) were computed and ",
      "PPSPEC contains one of: ", paste(urine_specs, collapse = ", ")
    )
  }

  if (!value_var %in% names(data)) {
    stop("t_pkpt08_uri: missing required column: ", value_var)
  }

  # A table-split (list_vars) column is constant within each split, so keeping it
  # on the rows only adds a redundant constant column.
  split_strat <- intersect(strat_var, list_vars)
  if (length(split_strat) > 0) {
    .tlg_warn(
      "t_pkpt08_uri: stratification variable(s) also used to split tables and dropped from the ",
      "rows: ", paste(split_strat, collapse = ", "),
      ". Within a split these are constant; the value is shown in the group header instead."
    )
  }
  strat_var <- setdiff(strat_var, list_vars)
  missing_strat <- setdiff(strat_var, names(data))
  if (length(missing_strat) > 0) {
    .tlg_warn(
      "t_pkpt08_uri: stratification variable(s) not found in the data and skipped: ",
      paste(missing_strat, collapse = ", "),
      ". The table is grouped by the remaining variable(s) only."
    )
  }

  group_levels <- NULL
  if (!is.null(col_group_var)) {
    group_levels <- .resolve_col_group(
      col_group_var, data, reserved = c(strat_var, list_vars)
    )
  }

  split_and_apply(
    data, list_vars,
    function(df) {
      .build_pkpp_table(
        df, strat_var, value_var,
        function(v) .summarise_adpp(v, include_geo = FALSE),
        col_group_var = col_group_var, group_levels = group_levels,
        stats = stats
      )
    }
  )
}

#' GMR Table with Confidence Intervals (pkpt11)
#'
#' Computes geometric mean ratios (GMR) with 90% confidence intervals for
#' selected PK parameters, comparing each treatment arm to a reference arm.
#'
#' @param data A CDISC ADPP data frame.
#' @param ref_arm Character string identifying the reference treatment arm in
#'   `strat_var`. If `NULL` (default), the first arm in sorted order is used.
#' @param ci_level Confidence level for the geometric mean ratio CI.
#'   Default: `0.90`.
#' @param strat_var Single treatment-arm column that defines the comparison axis
#'   (each arm is compared against `ref_arm`). Default: `"TRT01A"`.  Unlike the
#'   summary tables, this must be a single column.
#' @param param_var Column containing parameter names shown as rows.
#'   Default: `"PARAM"`.
#' @inheritParams t_pkpt03_col
#'
#' @return Named list of data frames, one per combination of `list_vars`.
#'   Each data frame has columns: `strat_var`, `param_var`, `n_ref`, `n_trt`,
#'   `GMR`, `CI_lower`, `CI_upper`.
#'
#' @details
#' The confidence interval is computed on the log scale using a two-sample
#' t-test approach: `exp(log_ratio +/- t * SE)` where SE is derived from the
#' pooled within-group standard deviations on the log scale.
#'
#' @examples
#' \dontrun{
#' adpp <- export_cdisc(res_nca)$adpp
#' tables <- t_pkpt11_gmr(adpp, ref_arm = "Placebo")
#' }
#'
#' @importFrom stats qt sd
#' @export
t_pkpt11_gmr <- function(
  data,
  ref_arm   = NULL,
  ci_level  = 0.90,
  list_vars = c("PPCAT"),
  strat_var = "TRT01A",
  param_var = "PARAM",
  value_var = "AVAL"
) {
  required_cols <- c(value_var, strat_var, param_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("t_pkpt11_gmr: missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  data <- filter_summary_excluded(data)

  arms <- sort(unique(data[[strat_var]]))
  if (is.null(ref_arm)) ref_arm <- arms[1]
  if (!ref_arm %in% arms) {
    stop("t_pkpt11_gmr: ref_arm '", ref_arm, "' not found in '", strat_var, "'.")
  }
  trt_arms <- setdiff(arms, ref_arm)

  alpha   <- 1 - ci_level

  .gmr_row <- function(ref_vals, trt_vals, strat, param) {
    ref_log <- log(ref_vals[ref_vals > 0 & !is.na(ref_vals)])
    trt_log <- log(trt_vals[trt_vals > 0 & !is.na(trt_vals)])
    nr <- length(ref_log)
    nt <- length(trt_log)

    if (nr < 2 || nt < 2) {
      gmr <- ci_lo <- ci_hi <- NA_real_
    } else {
      log_ratio <- mean(trt_log) - mean(ref_log)
      se <- sqrt(sd(ref_log)^2 / nr + sd(trt_log)^2 / nt)
      if (se == 0) {
        # Both arms have identical log-PK values; within-group variance is zero
        # and the Welch df formula yields 0/0 = NaN.  max(NaN, 1) returns NaN
        # in R (max does not suppress NaN without na.rm = TRUE), so qt() would
        # produce NaN CI bounds instead of NA.  Return NA to signal that the CI
        # is undefined when there is no within-group variability.
        gmr   <- round(exp(log_ratio), 3)
        ci_lo <- ci_hi <- NA_real_
      } else {
        df     <- (se^2)^2 / ((sd(ref_log)^2 / nr)^2 / (nr - 1) +
                                (sd(trt_log)^2 / nt)^2 / (nt - 1))
        t_crit <- qt(1 - alpha / 2, df = max(df, 1, na.rm = TRUE))
        gmr    <- round(exp(log_ratio), 3)
        ci_lo  <- round(exp(log_ratio - t_crit * se), 3)
        ci_hi  <- round(exp(log_ratio + t_crit * se), 3)
      }
    }
    data.frame(
      strat    = strat,
      param    = param,
      n_ref    = nr,
      n_trt    = nt,
      GMR      = gmr,
      CI_lower = ci_lo,
      CI_upper = ci_hi,
      stringsAsFactors = FALSE
    )
  }

  make_table <- function(df) {
    # Deduplicate to one row per subject × parameter × stratum before computing
    # GMR.  ADPP multi-interval duplicates inflate n and produce falsely narrow CIs.
    # Include AVISIT in the key when present so multi-visit rows are preserved.
    if ("USUBJID" %in% names(df)) {
      dedup_cols <- intersect(
        c("USUBJID", strat_var, param_var, "AVISIT"),
        names(df)
      )
      df <- df[!duplicated(df[dedup_cols]), , drop = FALSE]
    }

    arms_in_split <- unique(df[[strat_var]])

    if (!ref_arm %in% arms_in_split) {
      .tlg_warn(
        "t_pkpt11_gmr: reference arm '", ref_arm, "' is absent from this ",
        "data split. Returning an empty table for this page."
      )
      return(data.frame())
    }

    trt_in_split <- intersect(trt_arms, arms_in_split)
    if (length(trt_in_split) == 0) {
      .tlg_warn(
        "t_pkpt11_gmr: no treatment arms other than '", ref_arm,
        "' found in this data split. Returning an empty table."
      )
      return(data.frame())
    }

    params   <- sort(unique(df[[param_var]]))
    ref_data <- df[df[[strat_var]] == ref_arm, , drop = FALSE]

    rows <- unlist(lapply(trt_in_split, function(s) {
      trt_data <- df[df[[strat_var]] == s, , drop = FALSE]
      lapply(params, function(p) {
        ref_v <- ref_data[[value_var]][ref_data[[param_var]] == p]
        trt_v <- trt_data[[value_var]][trt_data[[param_var]] == p]
        .gmr_row(ref_v, trt_v, s, p)
      })
    }), recursive = FALSE)

    result <- do.call(rbind, rows)
    names(result)[names(result) == "strat"] <- strat_var
    names(result)[names(result) == "param"] <- param_var
    rownames(result) <- NULL
    apply_labels(result, type = "ADPP")
  }

  split_and_apply(data, list_vars, make_table)
}
