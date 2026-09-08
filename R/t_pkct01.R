#' Summary Concentration Table (pkct01)
#'
#' Summarizes PK concentration data by treatment/dose group and nominal timepoint.
#' Returns one data frame per analyte/specimen combination containing descriptive
#' statistics across subjects at each scheduled timepoint.
#'
#' @param data A CDISC ADNCA data frame (from `export_cdisc()$adnca`).
#' @param list_vars Character vector of columns used to split the output into
#'   separate tables. Default: `c("PARAM", "PCSPEC")`.
#' @param strat_var One or more columns whose combination defines the table rows
#'   (stratification). Default: `c("TRT01A", "ATPTREF", "NFRLT")` -- treatment
#'   arm, visit reference, and nominal timepoint.  Add or remove columns to change
#'   how the rows are grouped.  Any variable that is also a `list_vars`
#'   (table-split) column is dropped from the rows, since it is constant within
#'   each split.
#' @param value_var Column containing the numeric analysis value. Default:
#'   `"AVAL"`.
#' @param blq_var Column containing the character analysis value used to detect
#'   BLQ records. Default: `"AVALC"`. Records where this column equals `"BLQ"`
#'   are counted separately and excluded from numeric summaries. When `blq_var`
#'   is absent (as in `export_cdisc()$adnca`, which does not include `AVALC`),
#'   BLQ is detected via `value_var == 0`, consistent with the package convention
#'   for post-imputation BLQ encoding.
#' @param time_var Column that the `time_filter` applies to (the nominal timepoint
#'   column). Default: `"NFRLT"`.  Row grouping is controlled entirely by
#'   `strat_var`; this argument only names the column that `time_filter` subsets.
#' @param time_filter Optional vector of `time_var` values to keep. `NULL`
#'   (default) keeps every timepoint.
#' @param col_group_var Optional subject-level column (e.g. `"SEX"`, `"RACE"`)
#'   whose values become side-by-side comparison column groups: the full
#'   statistic block is repeated once per level, nested under a group header.
#'   `NULL` (default) produces the standard flat table. Must differ from
#'   `strat_var` and the `list_vars`.
#' @param stats Optional character vector of statistics to display, chosen from
#'   `c("n", "n_blq", "Mean", "SD", "CV_pct", "Median", "GeoMean", "GeoCV_pct",
#'   "Min", "Max")`. `NULL` (default) shows all of them.
#' @param title Table title. Supports `$VAR` / `!VAR` annotation syntax (see
#'   [parse_annotation()]). Attached to each returned table as a `tlg_title`
#'   attribute.
#' @param subtitle Per-table subtitle. `NULL` (default) names each `list_vars`
#'   split variable and its value, so a table split by analyte and specimen says
#'   which combination it covers.
#' @param footnote Table footnote, attached as a `tlg_footnote` attribute.
#'
#' @return A named list of data frames, one per unique combination of
#'   `list_vars`.  Each data frame has one column per `strat_var` followed by the
#'   statistics:
#'   `n`, `n_blq`, `Mean`, `SD`, `CV_pct`, `Median`, `GeoMean`, `GeoCV_pct`, `Min`, `Max`.
#'   When `col_group_var` is set, the statistic columns are prefixed per group
#'   level and a `col_groups` attribute drives the rendered two-level header.
#'
#' @details
#' BLQ values are excluded from all numeric statistics and counted in `n_blq`.
#' When `blq_var` is present, BLQ is identified as `df[[blq_var]] == "BLQ"`.
#' When `blq_var` is absent, `value_var == 0` is used as the fallback BLQ
#' indicator. `GeoMean` is computed on positive `value_var` values only.
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
  strat_var = c("TRT01A", "ATPTREF", "NFRLT"),
  value_var = "AVAL",
  blq_var   = "AVALC",
  time_var  = "NFRLT",
  time_filter = NULL,
  col_group_var = NULL,
  stats     = NULL,
  title     = NULL,
  subtitle  = NULL,
  footnote  = NULL
) {
  if (!value_var %in% names(data)) {
    stop("t_pkct01: missing required column: ", value_var)
  }

  if (!is.null(time_filter) && length(time_filter) > 0 && time_var %in% names(data)) {
    data <- data[as.character(data[[time_var]]) %in% as.character(time_filter), , drop = FALSE]
  }

  has_blq_col <- blq_var %in% names(data)

  # A table-split (list_vars) column is constant within each split, so keeping it
  # on the rows only adds a redundant constant column.  Intersect with the data
  # columns so an absent (e.g. single-arm) grouping var is skipped rather than
  # crashing interaction().
  split_strat <- intersect(strat_var, list_vars)
  if (length(split_strat) > 0) {
    .tlg_warn(
      "t_pkct01: stratification variable(s) also used to split tables and dropped from the ",
      "rows: ", paste(split_strat, collapse = ", "),
      ". Within a split these are constant; the value is shown in the group header instead."
    )
  }
  requested_strat <- setdiff(strat_var, list_vars)
  row_vars <- intersect(requested_strat, names(data))
  # Tell the user which stratification variables were dropped for being absent
  # (e.g. `DOSEA` on the "by Dose" variant when dose amount is not in the
  # concentration data) so it is clear why the table grouped by fewer variables.
  missing_strat <- setdiff(requested_strat, names(data))
  if (length(missing_strat) > 0) {
    .tlg_warn(
      "t_pkct01: stratification variable(s) not found in the data and skipped: ",
      paste(missing_strat, collapse = ", "),
      ". The table is grouped by the remaining variable(s) only."
    )
  }

  group_levels <- NULL
  if (!is.null(col_group_var)) {
    group_levels <- .resolve_col_group(
      col_group_var, data, reserved = c(row_vars, list_vars)
    )
  }

  .summarise_group <- function(df) {
    aval_num <- df[[value_var]]
    is_blq <- if (has_blq_col) {
      # Guard against NA in blq_var: NA != "BLQ" → NA, coerce to FALSE so those
      # rows are neither counted as BLQ nor silently passed into numeric stats.
      !is.na(df[[blq_var]]) & df[[blq_var]] == "BLQ"
    } else {
      !is.na(aval_num) & aval_num == 0
    }
    aval_num[is_blq] <- NA_real_

    # n = quantifiable + BLQ (regardless of whether value is NA for BLQ rows).
    # Using only !is.na(value) would undercount when AVALC="BLQ" but value=NA,
    # causing n_blq > n — an impossible table entry.
    n_total  <- sum(!is.na(df[[value_var]]) | is_blq)
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
    if (length(row_vars) == 0) {
      # No row-grouping variables: a single summary row over all rows.
      cell_stats <- if (is.null(col_group_var)) {
        .summarise_group(df)
      } else {
        .pivot_group_blocks(df, col_group_var, group_levels, .summarise_group)
      }
      result <- .apply_stat_labels(apply_labels(cell_stats))
      if (!is.null(col_group_var)) {
        attr(result, "col_groups") <-
          .make_col_groups(group_levels, names(.summarise_group(df[0, , drop = FALSE])))
      }
      return(.select_stats(result, stats))
    }

    # Coerce grouping columns to character so that R's NA becomes the string "NA"
    # before interaction().  interaction(..., drop = TRUE) never creates a factor
    # level for R's NA, so rows with NA in a grouping variable would be silently
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
      cell_stats <- if (is.null(col_group_var)) {
        .summarise_group(sub)
      } else {
        .pivot_group_blocks(sub, col_group_var, group_levels, .summarise_group)
      }
      cbind(key, cell_stats, stringsAsFactors = FALSE)
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) return(data.frame())

    result <- do.call(rbind, rows)

    # Order so each stratum's rows are contiguous.  .natural_sort_key() makes the
    # sort numeric-aware, so numeric NFRLT, factor levels, and character labels
    # with embedded numbers (e.g. "DOSE 10" after "DOSE 2", arms "100 mg" after
    # "50 mg") all order naturally rather than lexically; NA keys sort last.
    order_keys <- lapply(row_vars, function(v) .natural_sort_key(result[[v]]))
    result <- result[do.call(order, order_keys), , drop = FALSE]
    rownames(result) <- NULL
    result <- .apply_stat_labels(apply_labels(result))
    if (!is.null(col_group_var)) {
      attr(result, "col_groups") <-
        .make_col_groups(group_levels, names(.summarise_group(df[0, , drop = FALSE])))
    }
    .select_stats(result, stats)
  }

  # Derived here rather than in the signature: the default has to follow whatever
  # the user picked as split variables, and a default argument cannot see them.
  if (is.null(subtitle)) subtitle <- .split_subtitle(data, intersect(list_vars, names(data)))

  # Attached outside make_table() so every one of its return paths (including the
  # empty-table one) carries the labels.
  split_and_apply(data, list_vars, function(df) {
    .attach_table_labs(make_table(df), df, title, subtitle, footnote)
  })
}

#' @describeIn t_pkct01 Stratify by dose instead of treatment arm (first dose).
#' @param ... Additional arguments forwarded to [t_pkct01()].
#' @export
t_pkct01_dose <- function(data, strat_var = c("DOSEA", "ATPTREF", "NFRLT"), ...) {
  t_pkct01(data, strat_var = strat_var, ...)
}

#' @describeIn t_pkct01 Summarize using time after dose (TAD) nominal time.
#' @export
t_pkct01_tad <- function(data, strat_var = c("TRT01A", "ATPTREF", "NRRLT"), ...) {
  t_pkct01(data, strat_var = strat_var, time_var = "NRRLT", ...)
}

#' @describeIn t_pkct01 Stratify by dose using TAD nominal time.
#' @export
t_pkct01_dose_tad <- function(data, strat_var = c("DOSEA", "ATPTREF", "NRRLT"), ...) {
  t_pkct01(data, strat_var = strat_var, time_var = "NRRLT", ...)
}
