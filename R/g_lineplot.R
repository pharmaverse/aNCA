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
#'   count unique subjects per facet. Default is "USUBJID".
#' @param x_limits Numeric vector of length 2 for x-axis limits (min, max).
#'   Default is `NULL` (no limits).
#' @param y_limits Numeric vector of length 2 for y-axis limits (min, max).
#'   Default is `NULL` (no limits).
#' @param ylog_scale A logical value (`TRUE` or `FALSE`) indicating whether to use
#'  a logarithmic scale for the y-axis.
#' @param threshold_value A numeric value for the y-intercept of the threshold line.
#'   Only used if `show_threshold` is `TRUE`.
#' @param palette A character string specifying the color palette to use. Default is
#'   "default" palette.
#' @param tooltip_vars Character vector of column names to include in the tooltip.
#' @param labels_df A data.frame for variable label lookups.
#' @param vline_var Optional character string specifying the column name for vertical
#' lines.
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
#'  p <- g_lineplot(
#'    data = ind_data,
#'    x_var = "time_var",
#'    y_var = "AVAL",
#'    color_by = "USUBJID"
#'    )
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
                       facet_count_n = "USUBJID",
                       x_limits = NULL,
                       y_limits = NULL,
                       ylog_scale = FALSE,
                       threshold_value = NULL,
                       palette = "default",
                       tooltip_vars = NULL,
                       labels_df = NULL,
                       vline_var = NULL,
                       show_legend = TRUE) {

  if (nrow(data) == 0) {
    return(error_plot("No data available for the plot"))
  }

  # Derive color labels from labels_df if not explicitly provided
  if (is.null(color_labels) && !is.null(labels_df)) {
    color_labels <- vapply(
      color_by,
      function(x) get_label(variable = x, labels_df = labels_df),
      FUN.VALUE = character(1)
    )
  }

  x_lab <- .build_axis_label(x_var, x_unit, data, labels_df)
  y_lab <- .build_axis_label(y_var, y_unit, data, labels_df)
  title <- "PK Concentration - Time Profile"

  data <- .build_tooltip(data, tooltip_vars, labels_df, x_lab)
  # Create color var for aesthetic mapping
  plot_data <- data %>%
    mutate(
      color_var = interaction(!!!syms(color_by), sep = ", "),
      group_var = if (!is.null(group_by)) interaction(!!!syms(group_by)) else NULL
    ) %>%
    arrange(!!sym(x_var))

  facet_label_var <- facet_by
  if (!is.null(facet_count_n) && length(facet_by) > 0) {
    plot_data <- .build_facet_labels(plot_data, facet_by, facet_count_n)
    facet_label_var <- "facet_label"
  }

  plt <- ggplot(plot_data, aes(
    x = !!sym(x_var),
    y = !!sym(y_var),
    color = color_var,
    group = if (!is.null(group_by)) group_var else NULL,
    text = tooltip_text
  )) +
    geom_line() +
    geom_point() +
    labs(
      x = x_lab,
      y = y_lab,
      title = title,
      color = if (!is.null(color_labels)) paste(na.omit(color_labels), collapse = "\n") else
        paste(color_by, collapse = ", ")
    ) +
    theme_bw()
  # Apply legend
  if (!show_legend) {
    plt <- plt + theme(legend.position = "none")
  }
  # Add optional layers
  optional_layers <- list(
    .add_colour_palette(palette),
    .add_axis_limits(x_limits, y_limits),
    .add_y_scale(ylog_scale),
    .add_faceting(facet_label_var),
    .add_thr(threshold_value),
    .add_vline(data, vline_var)
  )
  plt + optional_layers
}

# --- Helper Functions (Internal) ---

#' Build axis label with optional unit suffix
#' @noRd
.build_axis_label <- function(var, unit_col, data, labels_df) {
  unit_str <- if (is.null(unit_col)) NULL else paste0(unique(data[[unit_col]]), collapse = ", ")
  label <- get_label(var, labels_df = labels_df)
  if (is.null(unit_str)) label else paste0(label, " [", unit_str, "]")
}

#' Build tooltip text column on data
#' @noRd
.build_tooltip <- function(data, tooltip_vars, labels_df, x_lab) {
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

  if (grepl("Nom.", x_lab, ignore.case = TRUE)) {
    data$tooltip_text <- paste0("<b>Mean</b> ", data$tooltip_text)
  }

  data
}

#' @noRd
.add_y_scale <- function(ylog_scale) {
  if (!ylog_scale) {
    return(NULL)
  }
  scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000), labels = scales::comma)
}

#' @noRd
.add_faceting <- function(facet_by) {
  if (is.null(facet_by) || length(facet_by) == 0) {
    return(NULL)
  }
  facet_wrap(vars(!!!syms(facet_by)), scales = "free")
}

#' @noRd
.build_facet_labels <- function(data, facet_by, facet_count_n) {

  use_precomputed_count <- grepl("count", facet_count_n, ignore.case = TRUE)

  data %>%
    mutate(
      .facet_label_values = purrr::pmap_chr(
        across(all_of(facet_by)),
        function(...) {
          vals <- list(...)
          paste(paste(names(vals), vals, sep = ": "), collapse = " | ")
        }
      )
    ) %>%
    group_by(!!!syms(facet_by)) %>%
    mutate(.facet_n = {
      values <- .data[[facet_count_n]]
      if (use_precomputed_count && is.numeric(values) && n_distinct(values) == 1) {
        values[1]
      } else {
        n_distinct(values)
      }
    }) %>%
    ungroup() %>%
    mutate(facet_label = paste0(.facet_label_values, " (n=", .facet_n, ")")) %>%
    select(-.facet_label_values, -.facet_n)
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
      guides(fill = "none"),
      labs(title = paste0("Mean PK Concentration - Time Profile",
                          if (isTRUE(ci)) " (95% CI)" else ""))
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
