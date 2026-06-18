#' Summary PK Parameters Table — statistics in columns (pkpt03)
#'
#' Summarizes pharmacokinetic parameters from ADPP data. Returns one data frame
#' per visit/analyte combination with PK parameters as rows and descriptive
#' statistics as columns.
#'
#' @param data A CDISC ADPP data frame (from `export_cdisc()$adpp`).
#' @param list_vars Character vector of columns used to split output into
#'   separate tables. Default: `c("AVISIT", "PPCAT")`.
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
  list_vars  = c("AVISIT", "PPCAT"),
  strat_var  = "TRT01A",
  param_var  = "PARAM",
  value_var  = "AVAL"
) {
  required_cols <- c(value_var, strat_var, param_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("t_pkpt03_col: missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  present_list_vars <- intersect(list_vars, names(data))

  if (nrow(data) == 0) return(list(data.frame()))

  .summarise_pk <- function(vals) {
    vals <- vals[!is.na(vals)]
    pos  <- vals[vals > 0]
    n    <- length(vals)
    mn   <- if (n > 0) mean(vals) else NA_real_
    s    <- if (n > 1) sd(vals)   else NA_real_
    gm   <- if (length(pos) > 0) exp(mean(log(pos))) else NA_real_
    gs   <- if (length(pos) > 1) sd(log(pos))        else NA_real_
    data.frame(
      n       = n,
      Mean    = round(mn,  3),
      SD      = round(s,   3),
      CV_pct  = if (!is.na(mn) && mn != 0 && !is.na(s)) round(s / mn * 100, 1) else NA_real_,
      GeoMean = round(gm,  3),
      GeoCV_pct = if (!is.na(gs)) round(sqrt(exp(gs^2) - 1) * 100, 1) else NA_real_,
      Median  = if (n > 0) round(median(vals), 3) else NA_real_,
      Min     = if (n > 0) round(min(vals),    3) else NA_real_,
      Max     = if (n > 0) round(max(vals),    3) else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  make_table <- function(df) {
    strats <- unique(df[[strat_var]])
    params <- unique(df[[param_var]])
    rows <- lapply(strats, function(s) {
      sub_s <- df[df[[strat_var]] == s, , drop = FALSE]
      lapply(params, function(p) {
        vals <- sub_s[[value_var]][sub_s[[param_var]] == p]
        key  <- data.frame(
          strat = s, param = p, stringsAsFactors = FALSE
        )
        names(key) <- c(strat_var, param_var)
        cbind(key, .summarise_pk(vals), stringsAsFactors = FALSE)
      })
    })
    flat <- unlist(rows, recursive = FALSE)
    if (length(flat) == 0) return(data.frame())
    result <- do.call(rbind, flat)
    rownames(result) <- NULL
    apply_labels(result)
  }

  if (length(present_list_vars) == 0) {
    return(list(all = make_table(data)))
  }

  split_keys <- do.call(
    interaction,
    c(lapply(present_list_vars, function(v) as.character(data[[v]])),
      list(sep = " / ", drop = TRUE))
  )

  tables <- lapply(levels(split_keys), function(key) {
    make_table(data[split_keys == key, , drop = FALSE])
  })
  setNames(tables, levels(split_keys))
}

#' @describeIn t_pkpt03_col Summary of metabolite-to-parent ratios (stats in columns).
#'   Filters to metabolite rows using `METABFL` (preferred) or, when absent from ADPP,
#'   falls back to rows where `PPCAT` or `PARAM` contains "metab" (case-insensitive).
#'   `METABFL` is present in ADPP only when it was included as a grouping variable in
#'   the NCA run.
#' @export
t_pkpt03_MP_col <- function(data, ...) {
  # Preferred: METABFL flag
  if ("METABFL" %in% names(data) &&
      any(!is.na(data$METABFL) & data$METABFL != "")) {
    data <- data[!is.na(data$METABFL) & data$METABFL != "", , drop = FALSE]
    return(t_pkpt03_col(data, ...))
  }

  # Fallback: PPCAT (= PARAM in ADPP) containing "metab"
  for (col in c("PPCAT", "PARAM")) {
    if (col %in% names(data)) {
      metab_vals <- unique(data[[col]][grepl("metab", data[[col]], ignore.case = TRUE)])
      if (length(metab_vals) > 0) {
        data <- data[grepl("metab", data[[col]], ignore.case = TRUE), , drop = FALSE]
        return(t_pkpt03_col(data, ...))
      }
    }
  }

  stop(
    "No metabolite data found. ",
    "METABFL is absent or all missing, and no PPCAT/PARAM values contain 'metab'. ",
    "To use this table, include METABFL as a grouping variable in your NCA run, ",
    "or ensure metabolite rows are labelled with 'metab' in PPCAT or PARAM."
  )
}

#' Mean Dose-Normalized PK Parameters Table (pkpt07)
#'
#' Filters ADPP to dose-normalized parameters (those whose `PARAMCD` ends in
#' `"D"`, e.g. `CMAXD`, `AUCTLSTD`) and summarizes them with the same column
#' layout as [t_pkpt03_col()]. These parameters must have been computed during
#' the NCA run — they are not derived on the fly.
#'
#' @param data A CDISC ADPP data frame (from `export_cdisc()$adpp`).
#' @param paramcd_var Column containing parameter codes used to detect
#'   dose-normalized parameters. Default: `"PARAMCD"`.
#' @inheritParams t_pkpt03_col
#'
#' @return Named list of data frames (same format as [t_pkpt03_col()]).
#'
#' @examples
#' \dontrun{
#' adpp <- export_cdisc(res_nca)$adpp
#' tables <- t_pkpt07_norm(adpp)
#' }
#'
#' @export
t_pkpt07_norm <- function(
  data,
  paramcd_var = "PARAMCD",
  list_vars   = c("AVISIT", "PPCAT"),
  strat_var   = "TRT01A",
  param_var   = "PARAM",
  value_var   = "AVAL"
) {
  if (paramcd_var %in% names(data)) {
    data <- data[grepl("D$", data[[paramcd_var]]), , drop = FALSE]
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
#' Filters ADPP to urine specimen records and delegates to [t_pkpt03_col()].
#'
#' @param data A CDISC ADPP data frame. Urine records are identified by
#'   `PPSPEC %in% urine_specs`.
#' @param urine_specs Character vector of specimen types considered urine.
#'   Default: `c("URINE", "Urine")`.
#' @inheritParams t_pkpt03_col
#'
#' @return Named list of data frames (same format as [t_pkpt03_col()]).
#'
#' @examples
#' \dontrun{
#' adpp <- export_cdisc(res_nca)$adpp
#' tables <- t_pkpt08_uri(adpp)
#' }
#'
#' @export
t_pkpt08_uri <- function(
  data,
  urine_specs = c("URINE", "Urine"),
  list_vars   = c("AVISIT", "PPCAT"),
  strat_var   = "TRT01A",
  param_var   = "PARAM",
  value_var   = "AVAL"
) {
  if ("PPSPEC" %in% names(data)) {
    data <- data[data$PPSPEC %in% urine_specs, , drop = FALSE]
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

  present_list_vars <- intersect(list_vars, names(data))

  .summarise_uri <- function(vals) {
    vals <- vals[!is.na(vals)]
    n    <- length(vals)
    mn   <- if (n > 0) mean(vals)   else NA_real_
    s    <- if (n > 1) stats::sd(vals) else NA_real_
    data.frame(
      n      = n,
      Mean   = round(mn, 3),
      SD     = round(s,  3),
      CV_pct = if (!is.na(mn) && mn != 0 && !is.na(s))
                 round(s / mn * 100, 1) else NA_real_,
      Median = if (n > 0) round(stats::median(vals), 3) else NA_real_,
      Min    = if (n > 0) round(min(vals), 3) else NA_real_,
      Max    = if (n > 0) round(max(vals), 3) else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  make_table <- function(df) {
    strats <- unique(df[[strat_var]])
    params <- unique(df[[param_var]])
    rows <- lapply(strats, function(s) {
      sub_s <- df[df[[strat_var]] == s, , drop = FALSE]
      lapply(params, function(p) {
        vals <- sub_s[[value_var]][sub_s[[param_var]] == p]
        key  <- data.frame(strat = s, param = p, stringsAsFactors = FALSE)
        names(key) <- c(strat_var, param_var)
        cbind(key, .summarise_uri(vals), stringsAsFactors = FALSE)
      })
    })
    flat <- unlist(rows, recursive = FALSE)
    if (length(flat) == 0) return(data.frame())
    result <- do.call(rbind, flat)
    rownames(result) <- NULL
    apply_labels(result)
  }

  if (length(present_list_vars) == 0) {
    return(list(all = make_table(data)))
  }

  split_keys <- do.call(
    interaction,
    c(lapply(present_list_vars, function(v) as.character(data[[v]])),
      list(sep = " / ", drop = TRUE))
  )

  tables <- lapply(levels(split_keys), function(key) {
    make_table(data[split_keys == key, , drop = FALSE])
  })
  setNames(tables, levels(split_keys))
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
#' t-test approach: `exp(log_ratio ± t * SE)` where SE is derived from the
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
  list_vars = c("AVISIT", "PPCAT"),
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
  present_list_vars <- intersect(list_vars, names(data))

  .gmr_row <- function(ref_vals, trt_vals, strat, param) {
    ref_log <- log(ref_vals[ref_vals > 0 & !is.na(ref_vals)])
    trt_log <- log(trt_vals[trt_vals > 0 & !is.na(trt_vals)])
    nr <- length(ref_log); nt <- length(trt_log)

    if (nr < 2 || nt < 2) {
      gmr <- ci_lo <- ci_hi <- NA_real_
    } else {
      log_ratio <- mean(trt_log) - mean(ref_log)
      se <- sqrt(sd(ref_log)^2 / nr + sd(trt_log)^2 / nt)
      df <- (se^2)^2 / ((sd(ref_log)^2 / nr)^2 / (nr - 1) +
                         (sd(trt_log)^2 / nt)^2 / (nt - 1))
      t_crit <- qt(1 - alpha / 2, df = max(df, 1))
      gmr   <- round(exp(log_ratio), 3)
      ci_lo <- round(exp(log_ratio - t_crit * se), 3)
      ci_hi <- round(exp(log_ratio + t_crit * se), 3)
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
    params   <- unique(df[[param_var]])
    ref_data <- df[df[[strat_var]] == ref_arm, , drop = FALSE]

    rows <- unlist(lapply(trt_arms, function(s) {
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
    apply_labels(result)
  }

  if (length(present_list_vars) == 0) {
    return(list(all = make_table(data)))
  }

  split_keys <- do.call(
    interaction,
    c(lapply(present_list_vars, function(v) as.character(data[[v]])),
      list(sep = " / ", drop = TRUE))
  )

  tables <- lapply(levels(split_keys), function(key) {
    make_table(data[split_keys == key, , drop = FALSE])
  })
  setNames(tables, levels(split_keys))
}
