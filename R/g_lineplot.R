#' Generate a Unified Line Plot for PK Data
#'
#' This function creates a ggplot2 line plot for pharmacokinetic (PK) data.
#' The function supports various customizations including log scales, faceting
#' and threshold lines.
#'
#'
#' @param data A data.frame containing the data to be plotted. This should be
#'   pre-processed by either `process_data_individual` or `process_data_mean`.
#' @param x_var A character string specifying the column name for the x-axis.
#' @param y_var A character string specifying the column name for the y-axis.
#' @param x_unit Optional character string specifying the column name for the x-axis unit.
#' @param y_unit Optional character string specifying the column name for the y-axis unit.
#' @param color_by A character vector specifying the column(s) from the original
#'   dataset that are used to determine the color of the lines and points.
#' @param color_labels Optional character vector of labels for the color legend.
#'   Default is `NULL` (uses `color_by` values).
#' @param facet_by A character vector of column names to facet the plot by.
#'   Default is `NULL` for no faceting.
#' @param group_by A character vector specifying the column names used to group
#'  the lines. Default is NULL for no grouping.
#' @param facet_count_n A character string specifying the column name used to
#'   count unique subjects per facet. Default is `NULL` (no counts shown).
#' @param x_limits Numeric vector of length 2 for x-axis limits (min, max).
#'   Default is `NULL` (no limits).
#' @param y_limits Numeric vector of length 2 for y-axis limits (min, max).
#'   Default is `NULL` (no limits).
#' @param ylog_scale A logical value (`TRUE` or `FALSE`) indicating whether to use
#'  a logarithmic scale for the y-axis.
#' @param lock_y_axis Logical; if `TRUE`, faceted plots use one shared
#'   y-axis range while x-axis scales remain free. Default is `FALSE`.
#' @param threshold_value A numeric value for the y-intercept of the threshold line.
#'   Only used if `show_threshold` is `TRUE`.
#' @param palette A character string specifying the color palette to use. Default is
#'   "default" palette.
#' @param tooltip_vars Character vector of column names to include in the tooltip.
#' @param labels_df A data.frame for variable label lookups.
#' @param vline_var Optional character string specifying the column name for vertical
#' lines.
#' @param linetype_by Optional character vector specifying the column name for line types.
#' @param show_legend Logical; whether to display the plot legend. Default is `TRUE`.
#' @returns A `ggplot` object representing the line plot.
#'
#' @import ggplot2
#' @import dplyr
#' @examples
#' library(dplyr)
#' ind_data <- expand.grid(
#'   time_var = c(0, 1, 2, 4, 8, 12),
#'   USUBJID = c("Subject1", "Subject2")
#' ) %>%
#'   mutate(
#'     AVAL = ifelse(USUBJID == "Subject1", 50, 80) * exp(-0.5 * time_var) + rnorm(n(), 0, 1),
#'     PARAM = "Analyte1",
#'     DOSEA = "Dose 1",
#'     RRLTU = "hours",
#'     AVALU = "ng/mL"
#'   )
#'
#' p <- g_lineplot(
#'   data = ind_data,
#'   x_var = "time_var",
#'   y_var = "AVAL",
#'   color_by = "USUBJID"
#' )
#' print(p)
#' @export
g_lineplot <- function(data,
                       x_var,
                       y_var,
                       x_unit = NULL,
                       y_unit = NULL,
                       color_by,
                       color_labels = NULL,
                       facet_by = NULL,
                       group_by = NULL,
                       facet_count_n = NULL,
                       x_limits = NULL,
                       y_limits = NULL,
                       ylog_scale = FALSE,
                       lock_y_axis = FALSE,
                       threshold_value = NULL,
                       palette = "default",
                       tooltip_vars = NULL,
                       labels_df = NULL,
                       vline_var = NULL,
                       linetype_by = NULL,
                       show_legend = TRUE) {
  if (nrow(data) == 0) {
    return(error_plot("No data available for the plot"))
  }

  y_family <- intersect(c("SD_min", "SD_max", "CI_lower", "CI_upper"), names(data))
  unit_res <- .resolve_facet_units(
    data = data,
    y_var = y_var,
    y_unit = y_unit,
    facet_by = facet_by,
    lock_y_axis = lock_y_axis,
    linetype_by = linetype_by,
    y_family = y_family
  )
  if (!is.null(unit_res$warning)) {
    warning(unit_res$warning)
  }
  data <- unit_res$data

  color_labels <- .resolve_color_labels(color_by, color_labels, labels_df)
  x_lab <- .build_axis_label(x_var, x_unit, data, labels_df)
  y_lab <- .build_axis_label(y_var, y_unit, data, labels_df, unit_override = unit_res$target)
  title <- "PK Concentration - Time Profile"

  data <- .build_tooltip(data, tooltip_vars, labels_df)
  plot_data <- .build_plot_data(
    data, x_var, color_by, group_by, linetype_by,
    facet_by, facet_count_n, unit_res$annotate_units
  )
  needs_facet_label <- (!is.null(facet_count_n) && length(facet_by) > 0) ||
    length(unit_res$annotate_units) > 0
  facet_label_var <- if (needs_facet_label) {
    "facet_label"
  } else {
    facet_by
  }

  aes_args <- .build_aes(x_var, y_var, group_by, linetype_by)

  plt <- ggplot(plot_data, do.call(aes, aes_args)) +
    geom_line() +
    geom_point() +
    labs(
      x = x_lab,
      y = y_lab,
      title = title,
      color = .build_color_legend_title(color_by, color_labels)
    ) +
    theme_bw()
  # Hide linetype legend when distinction is already encoded in color_var
  if (!is.null(linetype_by)) {
    plt <- plt + labs(linetype = "") + guides(linetype = "none")
  }
  if (!show_legend) {
    plt <- plt + theme(legend.position = "none")
  }
  # Add optional layers
  optional_layers <- list(
    .add_colour_palette(palette),
    .add_axis_limits(x_limits, y_limits),
    .add_y_scale(ylog_scale),
    .add_faceting(facet_label_var, lock_y_axis),
    .add_thr(threshold_value),
    .add_vline(data, vline_var)
  )
  plt + optional_layers
}

# --- Helper Functions (Internal) ---

#' Resolve color labels from labels_df if not explicitly provided
#' @noRd
.resolve_color_labels <- function(color_by, color_labels, labels_df) {
  if (!is.null(color_labels) || is.null(labels_df)) {
    return(color_labels)
  }
  vapply(
    color_by,
    function(x) get_label(variable = x, labels_df = labels_df),
    FUN.VALUE = character(1)
  )
}

#' Prepare plot data with color, group, and facet variables
#' @noRd
.build_plot_data <- function(data, x_var, color_by, group_by, linetype_by,
                             facet_by, facet_count_n, annotate_units = character(0)) {
  group_by_vars <- if (!is.null(group_by)) {
    if (!is.null(linetype_by)) c(group_by, linetype_by) else group_by
  } else {
    NULL
  }
  plot_data <- data %>%
    mutate(
      color_var = interaction(!!!syms(color_by), sep = ", "),
      group_var = if (!is.null(group_by_vars)) interaction(!!!syms(group_by_vars)) else NULL
    )

  # When linetype_by is present ("both" mode), append non-empty labels
  # to color_var so the color legend distinguishes default from dose-normalized
  if (!is.null(linetype_by) && linetype_by %in% names(plot_data)) {
    base_levels <- levels(plot_data$color_var)
    dn_label <- setdiff(unique(plot_data[[linetype_by]]), "")
    plot_data <- plot_data %>%
      mutate(
        color_var = ifelse(
          nchar(!!sym(linetype_by)) > 0,
          paste0(as.character(color_var), " (", !!sym(linetype_by), ")"),
          as.character(color_var)
        )
      )
    # Order legend: default entries first, then dose-normalized
    ordered_levels <- c(
      base_levels,
      paste0(base_levels, " (", dn_label, ")")
    )
    plot_data$color_var <- factor(plot_data$color_var, levels = ordered_levels)
  }

  plot_data <- plot_data %>% arrange(!!sym(x_var))

  build_labels <- length(facet_by) > 0 &&
    (!is.null(facet_count_n) || length(annotate_units) > 0)
  if (build_labels) {
    plot_data <- .build_facet_labels(plot_data, facet_by, facet_count_n, annotate_units)
  }
  plot_data
}

#' Build aesthetic mapping for the line plot
#' @importFrom rlang sym
#' @noRd
.build_aes <- function(x_var, y_var, group_by, linetype_by) {
  aes_args <- list(
    x = sym(x_var),
    y = sym(y_var),
    color = sym("color_var"),
    group = if (!is.null(group_by)) sym("group_var") else NULL,
    text = sym("tooltip_text")
  )
  if (!is.null(linetype_by)) {
    aes_args$linetype <- sym(linetype_by)
  }
  aes_args
}

#' Build color legend title from labels
#' @noRd
.build_color_legend_title <- function(color_by, color_labels) {
  if (!is.null(color_labels)) {
    paste(ifelse(is.na(color_labels), color_by, color_labels), collapse = "\n")
  } else {
    paste(color_by, collapse = ", ")
  }
}

#' Build axis label with optional unit suffix
#'
#' `unit_override` forces a single unit string (used when facet units have been
#' harmonised to a shared target), bypassing the data-derived unit collapse.
#' @noRd
.build_axis_label <- function(var, unit_col, data, labels_df, unit_override = NULL) {
  unit_str <- if (!is.null(unit_override)) {
    unit_override
  } else if (is.null(unit_col)) {
    NULL
  } else {
    paste0(unique(data[[unit_col]]), collapse = ", ")
  }
  label <- get_label(var, labels_df = labels_df)
  if (is.null(unit_str)) label else paste0(label, " [", unit_str, "]")
}

#' Resolve y-axis units across facets
#'
#' Handles unit alignment for faceted plots in two stages:
#' * Within each panel: a panel may pool several units when the analyte/matrix
#'   (`PARAM` x `PCSPEC`) is not part of the facet key. Compatible units are
#'   converted to the panel's most frequent unit. Panels still mixing units
#'   (incompatible dimensions) keep their values, show the units comma-separated
#'   in the strip, and drive a notification suggesting which grouping columns to
#'   add to `facet by`.
#' * Across panels: when `lock_y_axis` is `TRUE` and facets span several units,
#'   values are converted to a shared target unit where possible; facets whose
#'   unit cannot be converted are flagged so their strip can carry the unit.
#'
#' Skipped entirely when there is no y-unit column, no faceting, or dose-
#' normalised "both" mode is active (`linetype_by` set, units mixed by design).
#'
#' @returns A list with `data` (possibly converted), `target` (shared unit or
#'   `NULL`), `annotate_units` (named unit string per affected facet), and
#'   `warning` (mismatch notification or `NULL`).
#' @noRd
.resolve_facet_units <- function(data, y_var, y_unit, facet_by, lock_y_axis,
                                 linetype_by, y_family = character(0)) {
  empty <- list(
    data = data, target = NULL, annotate_units = character(0),
    warning = NULL
  )
  no_facets <- is.null(facet_by) || length(facet_by) == 0
  if (is.null(y_unit) || !y_unit %in% names(data) || no_facets) {
    return(empty)
  }
  # Dose-normalised "both" mode mixes units by design; leave untouched.
  if (!is.null(linetype_by)) {
    return(empty)
  }

  y_cols <- unique(c(y_var, y_family))

  # A single panel may legitimately pool several units when the analyte/matrix
  # (PARAM x PCSPEC) is not part of the facet key. Where those units are
  # compatible we converge each panel onto its own most-frequent unit; where
  # they are not, we surface them per facet (comma-separated) and warn.
  panel_res <- .converge_panel_units(data, y_var, y_unit, facet_by, y_cols)
  data <- panel_res$data
  panel_annotate <- panel_res$annotate_units
  incompatible_panels <- panel_res$incompatible

  # Across-facet alignment: only when locking a shared y-axis is requested.
  all_units <- unique(as.character(data[[y_unit]]))
  all_units <- all_units[!is.na(all_units)]
  target <- NULL
  across_annotate <- character(0)
  non_convertible <- character(0)
  if (isTRUE(lock_y_axis) && length(all_units) > 1) {
    align <- .align_units_across_facets(data, y_unit, all_units, y_cols, facet_by)
    data <- align$data
    target <- align$target
    across_annotate <- align$annotate_units
    non_convertible <- align$non_convertible
  }

  annotate_units <- .merge_annotations(panel_annotate, across_annotate)
  warning_msg <- .build_unit_warning(
    incompatible_panels = incompatible_panels,
    non_convertible = non_convertible,
    data = data,
    facet_by = facet_by,
    y_unit = y_unit
  )

  list(
    data = data,
    target = target,
    annotate_units = annotate_units,
    warning = warning_msg
  )
}

#' Converge units within each facet panel
#'
#' For every panel that pools more than one unit, convert compatible units to the
#' panel's most frequent unit. Panels left with several units (incompatible
#' dimensions) are recorded so their strip can show the units comma-separated.
#' @noRd
.converge_panel_units <- function(data, y_var, y_unit, facet_by, y_cols) {
  facet_key <- as.character(interaction(data[facet_by], drop = TRUE))
  row_unit <- as.character(data[[y_unit]])
  annotate_units <- character(0)
  incompatible <- character(0)

  for (panel in unique(facet_key)) {
    idx <- which(facet_key == panel)
    units <- unique(row_unit[idx][!is.na(row_unit[idx])])
    if (length(units) <= 1) next

    counts <- table(row_unit[idx])
    p_target <- names(sort(counts, decreasing = TRUE))
    p_target <- sort(p_target[counts[p_target] == max(counts)])[1]

    factors <- get_conversion_factor(units, p_target)
    names(factors) <- units
    for (u in setdiff(units[!is.na(factors)], p_target)) {
      rows <- idx[row_unit[idx] == u]
      for (col in y_cols) {
        data[[col]][rows] <- data[[col]][rows] * factors[[u]]
      }
      data[[y_unit]][rows] <- p_target
      row_unit[rows] <- p_target
    }

    remaining <- unique(row_unit[idx][!is.na(row_unit[idx])])
    if (length(remaining) > 1) {
      annotate_units[panel] <- paste(sort(remaining), collapse = ", ")
      incompatible[panel] <- annotate_units[panel]
    }
  }

  list(data = data, annotate_units = annotate_units, incompatible = incompatible)
}

#' Align units across facets onto a shared target when locking the y-axis
#' @noRd
.align_units_across_facets <- function(data, y_unit, all_units, y_cols, facet_by) {
  counts <- table(as.character(data[[y_unit]]))
  target <- names(sort(counts, decreasing = TRUE))
  target <- sort(target[counts[target] == max(counts)])[1]

  factors <- get_conversion_factor(all_units, target)
  names(factors) <- all_units
  non_convertible <- all_units[is.na(factors)]

  row_unit <- as.character(data[[y_unit]])
  for (u in setdiff(all_units[!is.na(factors)], target)) {
    rows <- which(row_unit == u)
    if (length(rows) == 0) next
    for (col in y_cols) {
      data[[col]][rows] <- data[[col]][rows] * factors[[u]]
    }
    data[[y_unit]][rows] <- target
  }

  annotate_units <- character(0)
  if (length(non_convertible) > 0) {
    facet_key <- as.character(interaction(data[facet_by], drop = TRUE))
    unit_now <- as.character(data[[y_unit]])
    for (panel in unique(facet_key)) {
      pu <- unique(unit_now[facet_key == panel])
      hit <- pu[pu %in% non_convertible]
      if (length(hit) > 0) annotate_units[panel] <- hit[1]
    }
  }

  list(
    data = data, target = target,
    annotate_units = annotate_units, non_convertible = non_convertible
  )
}

#' Merge per-panel unit annotations, panel-level taking precedence
#' @noRd
.merge_annotations <- function(panel_annotate, across_annotate) {
  merged <- across_annotate
  for (panel in names(panel_annotate)) {
    merged[panel] <- panel_annotate[panel]
  }
  merged
}

#' Suggest the grouping columns that would split mixed-unit panels
#'
#' Given the panels that still mix units, find the columns (beyond the current
#' facet key) whose values vary inside those panels. Splitting on them would
#' separate the units. Returns a character vector of column names.
#' @noRd
.suggest_facet_cols <- function(data, facet_by, y_unit, mixed_panels,
                                candidates = c("PARAM", "PCSPEC")) {
  candidates <- intersect(candidates, names(data))
  candidates <- setdiff(candidates, facet_by)
  if (length(candidates) == 0 || length(mixed_panels) == 0) {
    return(character(0))
  }
  facet_key <- as.character(interaction(data[facet_by], drop = TRUE))
  unit_now <- as.character(data[[y_unit]])

  useful <- character(0)
  for (col in candidates) {
    helps <- FALSE
    for (panel in mixed_panels) {
      idx <- which(facet_key == panel)
      panel_units <- unique(unit_now[idx][!is.na(unit_now[idx])])
      if (length(panel_units) <= 1) next
      # Splitting by `col` helps if at least one of its values maps to a
      # strict subset of the panel's units (i.e. it reduces heterogeneity).
      units_per_value <- tapply(
        unit_now[idx], as.character(data[[col]][idx]),
        function(u) length(unique(u[!is.na(u)]))
      )
      if (any(units_per_value < length(panel_units))) {
        helps <- TRUE
        break
      }
    }
    if (helps) useful <- c(useful, col)
  }
  useful
}

#' Build the unit-mismatch warning message
#'
#' Combines incompatible within-panel units and non-convertible across-facet
#' units into a single message, with a dynamic suggestion of which grouping
#' columns to add to `facet by` to separate them.
#' @noRd
.build_unit_warning <- function(incompatible_panels, non_convertible,
                                data, facet_by, y_unit) {
  mixed_panels <- names(incompatible_panels)
  units_seen <- unique(c(
    unlist(strsplit(unname(incompatible_panels), ", ")),
    non_convertible
  ))
  units_seen <- sort(units_seen[nzchar(units_seen)])
  if (length(mixed_panels) == 0 && length(non_convertible) == 0) {
    return(NULL)
  }

  msg <- paste0(
    "Y-axis units could not be aligned: incompatible units shown per facet (",
    paste(units_seen, collapse = ", "), ")."
  )

  suggest <- .suggest_facet_cols(data, facet_by, y_unit, mixed_panels)
  if (length(suggest) > 0) {
    msg <- paste0(
      msg, " Consider adding ", paste(suggest, collapse = " and "),
      " to 'facet by' to separate them."
    )
  } else if (length(mixed_panels) > 0) {
    msg <- paste0(msg, " Consider adding more variables to 'facet by'.")
  }
  msg
}

#' Build tooltip text column on data
#' @noRd
.build_tooltip <- function(data, tooltip_vars, labels_df) {
  if (is.null(tooltip_vars)) {
    data$tooltip_text <- rep(NA_character_, nrow(data))
    return(data)
  }
  if (!is.null(labels_df)) {
    data$tooltip_text <- generate_tooltip_text(data, labels_df, tooltip_vars, "ADNCA")
  } else {
    valid_vars <- intersect(tooltip_vars, names(data))
    if (length(valid_vars) > 0) {
      parts <- lapply(valid_vars, \(v) paste0(v, ": ", data[[v]]))
      data$tooltip_text <- paste(parts, collapse = "<br>")
    }
  }

  data
}

#' @noRd
.add_y_scale <- function(ylog_scale) {
  if (!ylog_scale) {
    return(NULL)
  }
  scale_y_log10(
    labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  )
}

#' @noRd
.add_faceting <- function(facet_by, lock_y_axis = FALSE) {
  if (is.null(facet_by) || length(facet_by) == 0) {
    return(NULL)
  }
  scales <- if (isTRUE(lock_y_axis)) "free_x" else "free"
  facet_wrap(vars(!!!syms(facet_by)), scales = scales)
}

#' Build facet strip labels
#'
#' Appends an optional subject count `(n=...)` when `facet_count_n` is supplied
#' and an optional unit suffix `[unit]` for facets listed in `annotate_units`
#' (keyed by the `interaction()` of `facet_by`).
#' @noRd
.build_facet_labels <- function(data, facet_by, facet_count_n = NULL,
                                annotate_units = character(0)) {
  add_count <- !is.null(facet_count_n)
  use_precomputed_count <- add_count &&
    grepl("count", facet_count_n, ignore.case = TRUE)

  data <- data %>%
    mutate(
      .facet_label_values = purrr::pmap_chr(
        across(all_of(facet_by)),
        function(...) {
          vals <- list(...)
          paste(paste(names(vals), vals, sep = ": "), collapse = " | ")
        }
      ),
      .facet_key = as.character(interaction(across(all_of(facet_by)), drop = TRUE))
    )

  if (add_count) {
    data <- data %>%
      group_by(!!!syms(facet_by)) %>%
      mutate(.facet_n = {
        values <- .data[[facet_count_n]]
        if (use_precomputed_count && is.numeric(values) && n_distinct(values) == 1) {
          values[1]
        } else {
          n_distinct(values)
        }
      }) %>%
      ungroup()
  }

  data <- data %>%
    mutate(
      .facet_unit = if (length(annotate_units) > 0) {
        unname(annotate_units[.facet_key])
      } else {
        NA_character_
      },
      facet_label = .facet_label_values,
      facet_label = if (add_count) {
        paste0(facet_label, " (n=", .facet_n, ")")
      } else {
        facet_label
      },
      facet_label = ifelse(
        is.na(.facet_unit), facet_label,
        paste0(facet_label, " [", .facet_unit, "]")
      )
    )

  drop_cols <- intersect(
    c(".facet_label_values", ".facet_n", ".facet_key", ".facet_unit"),
    names(data)
  )
  data %>% select(-all_of(drop_cols))
}

#' @noRd
.add_axis_limits <- function(x_limits, y_limits) {
  has_x <- is.numeric(x_limits) && length(x_limits) == 2 && any(is.finite(x_limits))
  has_y <- is.numeric(y_limits) && length(y_limits) == 2 && any(is.finite(y_limits))

  if (!has_x && !has_y) {
    return(NULL)
  }

  xlim_vals <- if (has_x) x_limits else NULL
  ylim_vals <- if (has_y) y_limits else NULL

  coord_cartesian(xlim = xlim_vals, ylim = ylim_vals)
}

#' @noRd
.add_thr <- function(thr) {
  if (!is.numeric(thr) || length(thr) != 1 || !is.finite(thr)) {
    return(NULL)
  }
  geom_hline(yintercept = thr, linetype = "dotted", color = "red")
}

#' @noRd
.add_vline <- function(data, vline_var) {
  if (is.null(vline_var)) {
    return(NULL)
  }
  geom_vline(data = data, aes(xintercept = !!sym(vline_var)), linetype = "dotted", color = "blue")
}

#' @noRd
.add_mean_layers <- function(sd_min, sd_max, ci, color_by, y_var, x_var, group_var) {
  # 1. Error bars
  error_bar_layer <- NULL
  if (isTRUE(sd_min) || isTRUE(sd_max)) {
    ymin_val <- if (isTRUE(sd_min)) sym("SD_min") else sym(y_var)
    ymax_val <- if (isTRUE(sd_max)) sym("SD_max") else sym(y_var)
    error_bar_layer <- geom_errorbar(
      aes(
        x = .data[[x_var]],
        ymin = !!ymin_val,
        ymax = !!ymax_val,
        color = color_var,
        group = .data[[group_var]]
      ),
      inherit.aes = FALSE,
      width = 0.4
    )
  }
  # 2. CI Ribbon
  ci_ribbon_layer <- NULL
  if (isTRUE(ci)) {
    ci_ribbon_layer <- list(
      geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = color_var), alpha = 0.3),
      guides(fill = "none")
    )
  }
  # Return a list of all layers
  list(error_bar_layer, ci_ribbon_layer)
}

#' @importFrom ggplot2 scale_color_viridis_d
#' @noRd
.add_colour_palette <- function(palette) {
  if (palette %in% c("plasma", "cividis", "inferno")) {
    scale_color_viridis_d(option = palette)
  } else {
    NULL
  }
}
