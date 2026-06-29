#' Summary Concentration Table (pkct01)
#'
#' Summarizes PK concentration data by treatment/dose group and nominal timepoint.
#' Returns one data frame per analyte/specimen combination containing descriptive
#' statistics across subjects at each scheduled timepoint.
#'
#' @param data A CDISC ADNCA data frame (from `export_cdisc()$adnca`).
#' @param list_vars Character vector of columns used to split the output into
#'   separate tables. Default: `c("PARAM", "PCSPEC")`.
#' @param strat_var Column name used for treatment/dose stratification.
#'   Default: `"TRT01A"`.
#' @param time_var Column name for the nominal timepoint axis.
#'   Default: `"NFRLT"` (nominal time from first dose).
#' @param visit_var Column name for the visit/period reference label.
#'   Default: `"ATPTREF"`.
#' @param blq_var Column containing the character analysis value used to detect
#'   BLQ records. Default: `"AVALC"`. Records where this column equals `"BLQ"`
#'   are counted separately and excluded from numeric summaries. When `blq_var`
#'   is absent (as in `export_cdisc()$adnca`, which does not include `AVALC`),
#'   BLQ is detected via `AVAL == 0`, consistent with the package convention
#'   for post-imputation BLQ encoding.
#'
#' @return A named list of data frames, one per unique combination of
#'   `list_vars`.  Each data frame contains columns for `strat_var`,
#'   `visit_var`, `time_var`, and the statistics:
#'   `n`, `n_blq`, `Mean`, `SD`, `CV_pct`, `Median`, `GeoMean`, `GeoCV_pct`, `Min`, `Max`.
#'
#' @details
#' BLQ values are excluded from all numeric statistics and counted in `n_blq`.
#' When `blq_var` is present, BLQ is identified as `df[[blq_var]] == "BLQ"`.
#' When `blq_var` is absent, `AVAL == 0` is used as the fallback BLQ indicator.
#' `GeoMean` is computed on positive `AVAL` values only.
#'
#' @examples
#' \dontrun{
#' adnca <- export_cdisc(res_nca)$adnca
#' tables <- t_pkct01(adnca)
#' tables[[1]]
#' }
#'
#' @importFrom stats sd median
#' @export
t_pkct01 <- function( # nolint: cyclocomp_linter
  data,
  list_vars = c("PARAM", "PCSPEC"),
  strat_var = "TRT01A",
  time_var  = "NFRLT",
  visit_var = "ATPTREF",
  blq_var   = "AVALC"
) {
  required_cols <- c("AVAL", strat_var, time_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("t_pkct01: missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  present_visit_var <- if (visit_var %in% names(data)) visit_var else NULL
  has_blq_col <- blq_var %in% names(data)

  row_vars <- c(strat_var, present_visit_var, time_var)

  .summarise_group <- function(df) {
    aval_num <- df$AVAL
    is_blq <- if (has_blq_col) {
      # Guard against NA in blq_var: NA != "BLQ" → NA, coerce to FALSE so those
      # rows are neither counted as BLQ nor silently passed into numeric stats.
      !is.na(df[[blq_var]]) & df[[blq_var]] == "BLQ"
    } else {
      !is.na(df$AVAL) & df$AVAL == 0
    }
    aval_num[is_blq] <- NA_real_

    # n = quantifiable + BLQ (regardless of whether AVAL is NA for BLQ rows).
    # Using only !is.na(AVAL) would undercount when AVALC="BLQ" but AVAL=NA,
    # causing n_blq > n — an impossible table entry.
    n_total  <- sum(!is.na(df$AVAL) | is_blq)
    n_blq    <- sum(is_blq, na.rm = TRUE)
    vals     <- aval_num[!is.na(aval_num)]
    pos_vals <- vals[vals > 0]
    gs       <- if (length(pos_vals) > 1) sd(log(pos_vals)) else NA_real_

    data.frame(
      n         = n_total,
      n_blq     = n_blq,
      Mean      = if (length(vals) > 0) round(mean(vals),   3) else NA_real_,
      SD        = if (length(vals) > 1) round(sd(vals),     3) else NA_real_,
      CV_pct    = if (length(vals) > 1 && mean(vals) != 0)
        round(sd(vals) / mean(vals) * 100, 1) else NA_real_,
      Median    = if (length(vals) > 0) round(median(vals), 3) else NA_real_,
      GeoMean   = if (length(pos_vals) > 0) round(exp(mean(log(pos_vals))), 3) else NA_real_,
      GeoCV_pct = if (!is.na(gs)) round(sqrt(exp(gs^2) - 1) * 100, 1) else NA_real_,
      Min       = if (length(vals) > 0) round(min(vals), 3) else NA_real_,
      Max       = if (length(vals) > 0) round(max(vals), 3) else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  make_table <- function(df) {
    # Coerce grouping columns to character so that R's NA becomes the string "NA"
    # before interaction().  interaction(..., drop = TRUE) never creates a factor
    # level for R's NA, so rows with NA in strat_var or time_var would be silently
    # dropped (e.g. unscheduled samples with NFRLT = NA).  Using the string "NA"
    # keeps those rows visible in the table under an explicit "NA" label.
    group_cols <- lapply(row_vars, function(v) {
      x <- as.character(df[[v]])
      x[is.na(x)] <- "NA"
      x
    })
    groups <- do.call(
      interaction,
      c(group_cols, list(sep = " | ", drop = TRUE))
    )

    rows <- lapply(levels(groups), function(grp) {
      sub <- df[groups == grp, , drop = FALSE]
      if (nrow(sub) == 0) return(NULL)
      key <- sub[1, row_vars, drop = FALSE]
      cbind(key, .summarise_group(sub), stringsAsFactors = FALSE)
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) return(data.frame())

    result <- do.call(rbind, rows)

    # Order so each stratum's rows are contiguous: by strat_var, then the visit
    # reference, then the (numeric) nominal time.  The key columns retain their
    # original types here (numeric NFRLT, character TRT01A/ATPTREF), so order()
    # sorts time numerically rather than lexically; NA keys sort last.
    result <- result[do.call(order, result[row_vars]), , drop = FALSE]
    rownames(result) <- NULL
    .apply_stat_labels(apply_labels(result))
  }

  split_and_apply(data, list_vars, make_table)
}

#' @describeIn t_pkct01 Stratify by dose instead of treatment arm (first dose).
#' @param ... Additional arguments forwarded to [t_pkct01()].
#' @export
t_pkct01_dose <- function(data, ...) {
  t_pkct01(data, strat_var = "DOSEA", ...)
}

#' @describeIn t_pkct01 Summarize using time after dose (TAD) nominal time.
#' @export
t_pkct01_tad <- function(data, ...) {
  t_pkct01(data, time_var = "NRRLT", ...)
}

#' @describeIn t_pkct01 Stratify by dose using TAD nominal time.
#' @export
t_pkct01_dose_tad <- function(data, ...) {
  t_pkct01(data, strat_var = "DOSEA", time_var = "NRRLT", ...)
}
