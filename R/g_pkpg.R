#' Boxplot of Primary PK Parameters (pkpg03)
#'
#' Produces one boxplot per PK parameter with treatment arms on the x-axis.
#' Returns a named list of ggplot objects, one per `PPCAT` × `PPSPEC`
#' combination in the ADPP data.
#'
#' @param data A CDISC ADPP data frame (from `export_cdisc()$adpp`).
#' @param strat_var Column used for x-axis grouping (treatment arms).
#'   Default: `"TRT01A"`.
#' @param param_var Column whose unique values each become a separate plot.
#'   Default: `"PARAM"`.
#' @param value_var Column containing the numeric analysis value. Default: `"AVAL"`.
#' @param list_vars Columns used to split output into separate plot pages.
#'   Default: `c("PPCAT", "PPSPEC")`.
#' @param all_points Logical. When `TRUE`, individual data points are overlaid
#'   on the boxes (pkpg04 style). Default: `FALSE`.
#' @param title Optional plot title string.
#' @param subtitle Optional plot subtitle string.
#' @param footnote Optional footnote string.
#' @param ylab Y-axis label. Defaults to the label attribute of `value_var`.
#'
#' @return A named list of ggplot objects.
#'
#' @examples
#' \dontrun{
#' adpp <- export_cdisc(res_nca)$adpp
#' plots <- p_pkpg03_boxp(adpp)
#' plots[[1]]
#' }
#'
#' @import dplyr
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter geom_text stat_summary
#'   labs theme_bw theme element_text element_blank facet_wrap
#' @importFrom rlang .data
#' @export
p_pkpg03_boxp <- function(
  data,
  strat_var = "TRT01A",
  param_var = "PARAM",
  value_var = "AVAL",
  list_vars = c("PPCAT", "PPSPEC"),
  all_points = FALSE,
  title     = NULL,
  subtitle  = NULL,
  footnote  = NULL,
  ylab      = NULL
) {
  required_cols <- c(value_var, strat_var, param_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("p_pkpg03_boxp: missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  data <- data[!is.na(data[[value_var]]), , drop = FALSE]
  if (nrow(data) == 0) return(list())

  data[[strat_var]] <- as.factor(data[[strat_var]])

  y_label <- if (!is.null(ylab)) ylab else .get_var_label(data, value_var)

  .make_plot <- function(df) {
    # Deduplicate to one row per subject × parameter × stratum.  ADPP multi-interval
    # data has identical rows per dose event; duplicates inflate jitter point counts
    # and distort summary statistics in stat_summary.  Done inside .make_plot (after
    # split_and_apply) so that list_vars columns don't interfere with the dedup key.
    # AVISIT is included when present so multi-visit rows are preserved.
    if ("USUBJID" %in% names(df)) {
      dedup_cols <- intersect(c("USUBJID", strat_var, param_var, "AVISIT"), names(df))
      df <- df[!duplicated(df[dedup_cols]), , drop = FALSE]
    }
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x     = .data[[strat_var]],
        y     = .data[[value_var]],
        fill  = .data[[strat_var]]
      )
    ) +
      ggplot2::geom_boxplot(outlier.shape = if (all_points) NA else 19,
                            alpha = 0.7, width = 0.5) +
      ggplot2::stat_summary(
        fun      = "mean",
        shape    = 8,
        size     = 0.8,
        color    = "black",
        show.legend = FALSE
      ) +
      ggplot2::facet_wrap(stats::as.formula(paste("~", param_var)),
                          scales = "free_y") +
      ggplot2::labs(
        title    = title,
        subtitle = subtitle,
        caption  = footnote,
        x        = NULL,
        y        = y_label,
        fill     = NULL
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title      = ggplot2::element_text(family = "sans", size = 14, color = "black"),
        plot.subtitle   = ggplot2::element_text(family = "sans", size = 11, color = "black"),
        axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
        legend.position = "none"
      )

    if (all_points) {
      p <- p + ggplot2::geom_jitter(width = 0.15, size = 1.5, alpha = 0.6)
    }

    # Outlier labels: only shown when all_points = FALSE (pkpg03 style).
    # When all_points = TRUE (pkpg04), every dot is drawn by geom_jitter with
    # a random x-offset, so a geom_text anchored at the un-jittered factor
    # centre would visually float away from the jitter point.  In pkpg04, all
    # data points are already visible as individual dots, so outlier labelling
    # is redundant and would be misleading.
    if (!all_points && "USUBJID" %in% names(df)) {
      outlier_df <- df %>%
        dplyr::group_by(.data[[param_var]], .data[[strat_var]]) %>%
        dplyr::mutate(
          .q1     = quantile(.data[[value_var]], 0.25, na.rm = TRUE),
          .q3     = quantile(.data[[value_var]], 0.75, na.rm = TRUE),
          .iqr    = .q3 - .q1,
          .is_out = !is.na(.data[[value_var]]) &
            (.data[[value_var]] < .q1 - 1.5 * .iqr |
             .data[[value_var]] > .q3 + 1.5 * .iqr)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data[[".is_out"]])

      if (nrow(outlier_df) > 0) {
        p <- p + ggplot2::geom_text(
          data        = outlier_df,
          ggplot2::aes(label = .data[["USUBJID"]]),
          size        = 2.5,
          hjust       = -0.2,
          color       = "black",
          show.legend = FALSE,
          inherit.aes = TRUE
        )
      }
    }

    p
  }

  split_and_apply(data, list_vars, .make_plot)
}

#' @describeIn p_pkpg03_boxp Boxplot with all individual data points overlaid (pkpg04).
#' @export
p_pkpg04_boxp <- function(data, ...) {
  p_pkpg03_boxp(data, all_points = TRUE, ...)
}

#' Boxplot of Metabolite/Parent PK Parameter Ratios (pkpg06)
#'
#' Filters ADPP to metabolite rows using the same fallback logic as
#' [t_pkpt03_MP_col()] (METABFL preferred, then PPCAT/PARAM grep for "metab"),
#' then delegates to [p_pkpg03_boxp()].
#'
#' @inheritParams p_pkpg03_boxp
#'
#' @return A named list of ggplot objects (same format as [p_pkpg03_boxp()]).
#'
#' @examples
#' \dontrun{
#' adpp <- export_cdisc(res_nca)$adpp
#' plots <- p_pkpg06_mp(adpp)
#' plots[[1]]
#' }
#'
#' @export
p_pkpg06_mp <- function(data, ...) {
  p_pkpg03_boxp(filter_metabolite_rows(data, "p_pkpg06_mp"), ...)
}

#' Mean Urine PK Parameter Profile Plot (pkpg01)
#'
#' Computes mean (± SD) of a urine PK parameter per treatment arm across
#' collection intervals and draws a connected line plot.  Designed for ADPP
#' data filtered to `PPSPEC %in% urine_specs` (e.g. cumulative amount excreted
#' or fraction of dose recovered).  Returns one ggplot per unique combination
#' of `list_vars`.
#'
#' @param data A CDISC ADPP data frame (from `export_cdisc()$adpp`).
#' @param strat_var Column for treatment arm colour/grouping. Default: `"TRT01A"`.
#' @param param_var Column whose unique values label each x-axis position.
#'   Default: `"PARAM"`.
#' @param value_var Column containing the numeric analysis value. Default: `"AVAL"`.
#' @param urine_specs Character vector of `PPSPEC` values treated as urine.
#'   Default: `c("URINE", "Urine")`.
#' @param list_vars Columns used to split output into separate plots.
#'   Default: `c("PPCAT")`.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param footnote Optional footnote / caption.
#' @param xlab X-axis label. Default: `"Collection Interval"`.
#' @param ylab Y-axis label. Defaults to the label attribute of `value_var`.
#'
#' @return A named list of ggplot objects.
#'
#' @examples
#' \dontrun{
#' adpp <- export_cdisc(res_nca)$adpp
#' plots <- p_pkpg01_cum(adpp)
#' plots[[1]]
#' }
#'
#' @import dplyr
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_errorbar labs
#'   theme_bw theme element_text
#' @importFrom rlang .data
#' @importFrom stats sd
#' @export
p_pkpg01_cum <- function(
  data,
  strat_var      = "TRT01A",
  param_var      = "PARAM",
  value_var      = "AVAL",
  urine_specs    = c("URINE", "Urine"),
  paramcd_filter = "RCAMINT",
  time_end_var   = "PPENINT",
  list_vars      = c("PPCAT"),
  title          = "Mean Cumulative Amount Recovered in Urine",
  subtitle       = NULL,
  footnote       = NULL,
  xlab           = NULL,
  ylab           = NULL
) {
  if ("PPSPEC" %in% names(data)) {
    data <- data[data$PPSPEC %in% urine_specs, , drop = FALSE]
  }
  if (!is.null(paramcd_filter) && "PARAMCD" %in% names(data)) {
    data <- data[data$PARAMCD %in% paramcd_filter, , drop = FALSE]
  }
  if (nrow(data) == 0) return(list())

  required_cols <- c(value_var, strat_var, param_var)
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("p_pkpg01_cum: missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  data <- data[!is.na(data[[value_var]]), , drop = FALSE]
  if (nrow(data) == 0) return(list())

  data[[strat_var]] <- as.factor(data[[strat_var]])

  y_label <- if (!is.null(ylab)) ylab else .get_var_label(data, value_var)

  .parse_ppenint <- function(x) {
    x <- as.character(x)
    # ISO 8601 duration: "PT12H" → 12, "PT1.5H" → 1.5, "Inf" → Inf
    hrs <- suppressWarnings(as.numeric(x))
    iso <- grepl("^PT[0-9.]+H$", x, ignore.case = TRUE)
    hrs[iso] <- as.numeric(sub("^PT([0-9.]+)H$", "\\1", x[iso], ignore.case = TRUE))
    hrs
  }

  # use_numeric_x and x_var are evaluated per split subset inside .make_urine_plot
  # so that each PPCAT page uses the correct axis based on its own PPENINT values.
  # Evaluating them on the full dataset before splitting causes blank pages when
  # one PPCAT subset has all-NA PPENINT after parsing.
  .make_urine_plot <- function(df, page_title = title) {
    # Deduplicate to one row per subject × stratum × x-axis variable.  ADPP
    # multi-interval data repeats the same value per dose event; duplicates make
    # sd_val = 0, collapsing error bars to flat lines.
    # AVISIT is included when present so multi-visit rows are preserved.
    if ("USUBJID" %in% names(df)) {
      dedup_cols <- intersect(c("USUBJID", strat_var, param_var, "AVISIT"), names(df))
      df <- df[!duplicated(df[dedup_cols]), , drop = FALSE]
    }
    # Determine x-axis type from this subset's data only
    use_num <- time_end_var %in% names(df) &&
      !all(is.na(df[[time_end_var]])) &&
      !all(is.infinite(.parse_ppenint(df[[time_end_var]])))

    if (use_num) {
      df[[time_end_var]] <- .parse_ppenint(df[[time_end_var]])
      use_num <- !all(is.na(df[[time_end_var]]))
    }

    if (use_num) {
      x_var         <- time_end_var
      x_axis_label  <- if (!is.null(xlab)) xlab else "Time (hours)"
    } else {
      param_levels      <- sort(unique(df[[param_var]]))
      df[[param_var]]   <- factor(df[[param_var]], levels = param_levels)
      x_var             <- param_var
      x_axis_label      <- if (!is.null(xlab)) xlab else "Collection Interval"
    }

    summary_df <- df %>%
      dplyr::group_by(.data[[strat_var]], .data[[x_var]]) %>%
      dplyr::summarise(
        mean_val = mean(.data[[value_var]], na.rm = TRUE),
        sd_val   = stats::sd(.data[[value_var]], na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(
        ymin = mean_val - sd_val,
        ymax = mean_val + sd_val
      )

    p <- ggplot2::ggplot(
      summary_df,
      ggplot2::aes(
        x     = .data[[x_var]],
        y     = .data[["mean_val"]],
        color = .data[[strat_var]],
        group = .data[[strat_var]]
      )
    ) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
        width = if (use_num) 0.5 else 0.2, alpha = 0.7
      ) +
      ggplot2::labs(
        title    = page_title,
        subtitle = subtitle,
        caption  = footnote,
        x        = x_axis_label,
        y        = y_label,
        color    = NULL
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title      = ggplot2::element_text(family = "sans", size = 14, color = "black"),
        plot.subtitle   = ggplot2::element_text(family = "sans", size = 11, color = "black"),
        axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
        legend.position = "bottom"
      )

    if (use_num) {
      x_breaks <- sort(unique(summary_df[[x_var]]))
      p <- p + ggplot2::scale_x_continuous(breaks = x_breaks)
    }

    p
  }

  split_and_apply(data, list_vars, .make_urine_plot)
}

#' @describeIn p_pkpg01_cum Mean percentage of dose recovered in urine (pkpg01 %).
#'   Identical to [p_pkpg01_cum()] but defaults to a % dose recovered title and
#'   y-axis label.
#' @export
p_pkpg01_per <- function(
  data,
  paramcd_filter = "FREXINT",
  title          = "Mean Percentage of Dose Recovered in Urine",
  ylab           = "Percent Dose Recovered (%)",
  ...
) {
  p_pkpg01_cum(data, paramcd_filter = paramcd_filter, title = title, ylab = ylab, ...)
}

#' Dose-Proportionality Scatter Plot with Power-Model Regression (pkpg02)
#'
#' Plots individual AVAL values against dose (DOSEA) on a log-log scale with
#' one facet per PK parameter.  A power-model regression line
#' (log y = α + β × log x) is overlaid on each facet together with the slope
#' estimate and its confidence interval, enabling visual assessment of
#' dose-proportionality (β = 1).
#'
#' @param data A CDISC ADPP data frame (from `export_cdisc()$adpp`).
#' @param dose_var  Column containing the administered dose. Default: `"DOSEA"`.
#' @param value_var Column containing the PK parameter value. Default: `"AVAL"`.
#' @param param_var Column used as facet variable. Default: `"PARAM"`.
#' @param strat_var Column used for point colour. Default: `"TRT01A"`.
#' @param list_vars Columns used to split output into separate plots.
#'   Default: `c("PPCAT", "PPSPEC")`.
#' @param ci_level  Confidence level for the slope CI. Default: `0.90`.
#' @param log_scale Logical. When `TRUE` (default), both axes are log10-scaled.
#' @param title    Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param footnote Optional footnote / caption.
#' @param xlab     X-axis label. Defaults to `dose_var` label + unit.
#' @param ylab     Y-axis label. Defaults to the label attribute of `value_var`.
#'
#' @return A named list of ggplot objects.
#'
#' @examples
#' \dontrun{
#' adpp <- export_cdisc(res_nca)$adpp
#' plots <- p_pkpg02_doseprop(adpp)
#' plots[[1]]
#' }
#'
#' @import dplyr
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_text geom_errorbar
#'   facet_wrap labs scale_x_log10 scale_y_log10 theme_bw theme element_text
#' @importFrom rlang .data
#' @importFrom stats lm coef confint predict sd
#' @export
p_pkpg02_doseprop <- function(
  data,
  dose_var  = "DOSEA",
  value_var = "AVAL",
  param_var = "PARAM",
  strat_var = "TRT01A",
  list_vars = c("PPCAT", "PPSPEC"),
  ci_level  = 0.90,
  log_scale = TRUE,
  title     = NULL,
  subtitle  = NULL,
  footnote  = NULL,
  xlab      = NULL,
  ylab      = NULL
) {
  required_cols <- c(value_var, dose_var, param_var)
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("p_pkpg02_doseprop: missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  data <- data[
    !is.na(data[[value_var]]) & !is.na(data[[dose_var]]) &
    data[[value_var]] > 0     & data[[dose_var]] > 0, ,
    drop = FALSE
  ]
  if (nrow(data) == 0) return(list())

  x_label <- if (!is.null(xlab)) xlab else {
    dose_u <- if ("DOSEU" %in% names(data)) unique(data[["DOSEU"]])[1] else ""
    base   <- .get_var_label(data, dose_var)
    if (nchar(dose_u) > 0) paste0(base, " (", dose_u, ")") else base
  }
  y_label <- if (!is.null(ylab)) ylab else .get_var_label(data, value_var)

  .fit_power <- function(df_param) {
    df_f <- df_param[df_param[[value_var]] > 0 & df_param[[dose_var]] > 0, , drop = FALSE]
    if (nrow(df_f) < 3) return(NULL)
    fit_df <- data.frame(logy = log(df_f[[value_var]]), logx = log(df_f[[dose_var]]))
    fit <- tryCatch(stats::lm(logy ~ logx, data = fit_df), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    ci <- tryCatch(
      stats::confint(fit, level = ci_level),
      error = function(e) matrix(c(NA, NA, NA, NA), nrow = 2)
    )
    dose_seq <- exp(seq(log(min(df_f[[dose_var]])), log(max(df_f[[dose_var]])), length.out = 80))
    pred_y   <- exp(stats::predict(fit, newdata = data.frame(logx = log(dose_seq))))
    pred_df  <- setNames(
      data.frame(dose_seq, pred_y),
      c(dose_var, value_var)
    )
    list(pred_df  = pred_df,
         slope    = stats::coef(fit)[["logx"]],
         slope_ci = ci["logx", ],
         adj_r2   = summary(fit)$adj.r.squared)
  }

  .make_dp_plot <- function(df) {
    # Deduplicate to one row per subject × parameter × stratum.  ADPP multi-interval
    # data has identical rows per dose event; duplicates make sd_val = 0, collapsing
    # error bars to flat lines while inflating the scatter point count.
    # AVISIT is included when present so multi-visit rows are preserved.
    if ("USUBJID" %in% names(df)) {
      dedup_cols <- intersect(c("USUBJID", param_var, strat_var, "AVISIT"), names(df))
      df <- df[!duplicated(df[dedup_cols]), , drop = FALSE]
    }
    params <- unique(df[[param_var]])

    fit_results <- lapply(params, function(p) {
      fit <- .fit_power(df[df[[param_var]] == p, , drop = FALSE])
      if (!is.null(fit)) {
        fit$pred_df[[param_var]] <- p
        fit$param <- p
      }
      fit
    })
    fit_results <- Filter(Negate(is.null), fit_results)

    point_aes <- if (strat_var %in% names(df)) {
      ggplot2::aes(color = .data[[strat_var]])
    } else {
      ggplot2::aes()
    }

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[dose_var]], y = .data[[value_var]])) +
      ggplot2::geom_point(point_aes, size = 2, alpha = 0.8) +
      ggplot2::facet_wrap(stats::as.formula(paste("~", param_var)), scales = "free") +
      ggplot2::labs(
        title    = title,
        subtitle = subtitle,
        caption  = footnote,
        x        = x_label,
        y        = y_label,
        color    = NULL
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title      = ggplot2::element_text(family = "sans", size = 14, color = "black"),
        plot.subtitle   = ggplot2::element_text(family = "sans", size = 11, color = "black"),
        axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
        legend.position = "bottom"
      )

    if (length(fit_results) > 0) {
      pred_all <- do.call(rbind, lapply(fit_results, `[[`, "pred_df"))
      p <- p + ggplot2::geom_line(
        data         = pred_all,
        ggplot2::aes(x = .data[[dose_var]], y = .data[[value_var]]),
        color        = "black",
        linewidth    = 0.8,
        inherit.aes  = FALSE
      )

      annot_df <- do.call(rbind, lapply(fit_results, function(fit) {
        setNames(
          data.frame(
            fit$param,
            sprintf("β = %.3f [%.3f, %.3f]\nadj. R² = %.3f",
                    fit$slope, fit$slope_ci[1], fit$slope_ci[2],
                    fit$adj_r2),
            -Inf, Inf,
            stringsAsFactors = FALSE
          ),
          c(param_var, "label", dose_var, value_var)
        )
      }))
      p <- p + ggplot2::geom_text(
        data        = annot_df,
        ggplot2::aes(x = .data[[dose_var]], y = .data[[value_var]], label = .data[["label"]]),
        # hjust = 0.05 places text just inside the left panel edge (x = -Inf).
        # hjust < 0 pushes text outside the left edge where it is clipped and
        # invisible with ggplot2's default clip = "on".
        hjust       = 0.05,
        vjust       = 1.5,
        size        = 2.8,
        color       = "black",
        inherit.aes = FALSE
      )
    }

    dose_summary <- df %>%
      dplyr::group_by(.data[[dose_var]], .data[[param_var]]) %>%
      dplyr::summarise(
        mean_val = mean(.data[[value_var]], na.rm = TRUE),
        sd_val   = stats::sd(.data[[value_var]], na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(
        # Per TLG catalog pkpg02: omit error bars when SD >= mean to avoid
        # non-positive lower bounds on the log scale (log(<=0) is undefined).
        ymin = ifelse(is.na(.data[["sd_val"]]) | .data[["sd_val"]] >= .data[["mean_val"]],
                      NA_real_, .data[["mean_val"]] - .data[["sd_val"]]),
        ymax = ifelse(is.na(.data[["sd_val"]]) | .data[["sd_val"]] >= .data[["mean_val"]],
                      NA_real_, .data[["mean_val"]] + .data[["sd_val"]])
      )

    p <- p +
      ggplot2::geom_errorbar(
        data        = dose_summary,
        ggplot2::aes(
          x    = .data[[dose_var]],
          ymin = .data[["ymin"]],
          ymax = .data[["ymax"]]
        ),
        width       = 0.05,
        color       = "black",
        inherit.aes = FALSE,
        na.rm       = TRUE
      ) +
      ggplot2::geom_point(
        data        = dose_summary,
        ggplot2::aes(x = .data[[dose_var]], y = .data[["mean_val"]]),
        shape       = 2,
        size        = 3,
        color       = "black",
        inherit.aes = FALSE
      )

    if (log_scale) {
      p <- p + ggplot2::scale_x_log10() + ggplot2::scale_y_log10()
    }
    p
  }

  split_and_apply(data, list_vars, .make_dp_plot)
}
