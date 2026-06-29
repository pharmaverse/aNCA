#' Shared table builder for ADPP summary tables (pkpt03 / pkpt08 pattern).
#'
#' Deduplicates to one row per USUBJID x strat x param, then applies
#' `summary_fn` to the numeric values for each stratum/parameter combination.
#' Used internally by `t_pkpt03_col` and `t_pkpt08_uri` to avoid duplicating
#' the identical looping/cbind/rbind boilerplate.
#'
#' @param df Data frame (one split from `split_and_apply`).
#' @param strat_var,param_var,value_var Column name strings.
#' @param summary_fn Function that takes a numeric vector and returns a
#'   one-row `data.frame` of summary statistics.
#' @return A labeled `data.frame`.
#' @noRd
.build_pkpp_table <- function(df, strat_var, param_var, value_var, summary_fn) {
  if ("USUBJID" %in% names(df)) {
    # Include AVISIT in the dedup key when present so that rows from different
    # visits (genuinely different AVAL values) are kept.  AVISIT is absent from
    # single-interval ADPP; including it only when present is safe because
    # !duplicated() still collapses true within-visit duplicates (same
    # USUBJID × strat × param × AVISIT repeated per dose event).
    dedup_cols <- intersect(
      c("USUBJID", strat_var, param_var, "AVISIT"),
      names(df)
    )
    df <- df[!duplicated(df[dedup_cols]), , drop = FALSE]
  }
  strats <- sort(unique(df[[strat_var]]))
  params <- sort(unique(df[[param_var]]))
  rows <- lapply(strats, function(s) {
    sub_s <- df[df[[strat_var]] == s, , drop = FALSE]
    lapply(params, function(p) {
      vals <- sub_s[[value_var]][sub_s[[param_var]] == p]
      key  <- data.frame(strat = s, param = p, stringsAsFactors = FALSE)
      names(key) <- c(strat_var, param_var)
      cbind(key, summary_fn(vals), stringsAsFactors = FALSE)
    })
  })
  flat <- unlist(rows, recursive = FALSE)
  if (length(flat) == 0) return(data.frame())
  result <- do.call(rbind, flat)
  rownames(result) <- NULL
  .apply_stat_labels(apply_labels(result, type = "ADPP"))
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
#' @param strat_var Column for treatment/dose stratification. Default: `"TRT01A"`.
#' @param param_var Column containing parameter names shown as rows.
#'   Default: `"PARAM"`.
#' @param value_var Column containing the numeric analysis value. Default: `"AVAL"`.
#'
#' @return A named list of data frames, one per combination of `list_vars`.
#'   Each data frame has columns: `strat_var`, `param_var`, `n`, `Mean`, `SD`,
#'   `CV_pct`, `GeoMean`, `GeoCV_pct`, `Median`, `Min`, `Max`.
#'
#' @examples
#' \dontrun{
#' adpp <- export_cdisc(res_nca)$adpp
#' tables <- t_pkpt03_col(adpp)
#' tables[[1]]
#' }
#'
#' @importFrom stats sd median
#' @export
t_pkpt03_col <- function(
  data,
  list_vars  = c("PPCAT"),
  strat_var  = "TRT01A",
  param_var  = "PARAM",
  value_var  = "AVAL"
) {
  required_cols <- c(value_var, strat_var, param_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("t_pkpt03_col: missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (nrow(data) == 0) return(list(data.frame()))

  split_and_apply(
    data, list_vars,
    function(df) .build_pkpp_table(df, strat_var, param_var, value_var, .summarise_adpp)
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
  strat_var      = "TRT01A",
  param_var      = "PARAM",
  value_var      = "AVAL"
) {
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
    warning(
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
    param_var = param_var,
    value_var = value_var
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
#' @return Named list of data frames with columns: `strat_var`, `param_var`,
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
  strat_var   = "TRT01A",
  param_var   = "PARAM",
  value_var   = "AVAL"
) {
  if ("PPSPEC" %in% names(data)) {
    # Case-insensitive match (CDISC value is "URINE"; source casing varies).
    data <- data[toupper(data$PPSPEC) %in% toupper(urine_specs), , drop = FALSE]
  } else {
    warning(
      "t_pkpt08_uri: 'PPSPEC' column not found in data; the urine specimen ",
      "filter was not applied. All rows are treated as urine. If your data ",
      "contains non-urine records, the output will be incorrect. Ensure ",
      "PPSPEC is present in the ADPP parameter data (from export_cdisc()$adpp)."
    )
  }
  if (nrow(data) == 0) {
    stop(
      "t_pkpt08_uri: no urine PK parameter data found in ADPP. ",
      "Ensure urine NCA parameters (e.g. Ae, Fe) were computed and ",
      "PPSPEC contains one of: ", paste(urine_specs, collapse = ", ")
    )
  }

  required_cols <- c(value_var, strat_var, param_var)
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("t_pkpt08_uri: missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  split_and_apply(
    data, list_vars,
    function(df) {
      .build_pkpp_table(
        df, strat_var, param_var, value_var,
        function(v) .summarise_adpp(v, include_geo = FALSE)
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
      warning(
        "t_pkpt11_gmr: reference arm '", ref_arm, "' is absent from this ",
        "data split. Returning an empty table for this page."
      )
      return(data.frame())
    }

    trt_in_split <- intersect(trt_arms, arms_in_split)
    if (length(trt_in_split) == 0) {
      warning(
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
