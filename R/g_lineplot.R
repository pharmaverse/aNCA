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
#' @param facet_by A character vector of column names to facet the plot by.
#'   Default is `NULL` for no faceting.
#' @param group_by A character vector specifying the column names used to group
#'  the lines. Default is NULL for no grouping.
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
                       facet_by = NULL,
                       group_by = NULL,
                       x_limits = NULL,
                       y_limits = NULL,
                       ylog_scale = FALSE,
                       threshold_value = NULL,
                       palette = "default",
                       tooltip_vars = NULL,
                       labels_df = NULL,
                       vline_var = NULL,
                       linetype_by = NULL) {

  if (nrow(data) == 0) {
    return(error_plot("No data available for the plot"))
  }

  # Concatenate unique units, sep by ","
  #TODO: potential to facet if > 1 unit (#848)
  x_lab_unit <- if (is.null(x_unit)) NULL else paste0(unique(data[[x_unit]]), collapse = ", ")
  x_lab <- if (is.null(x_lab_unit)) "Time" else paste0("Time [", x_lab_unit, "]")
  y_lab_unit <- if (is.null(y_unit)) NULL else paste0(unique(data[[y_unit]]), collapse = ", ")
  y_lab <- if (is.null(y_lab_unit)) "Concentration" else paste0("Concentration [", y_lab_unit, "]")
  title <- "PK Concentration - Time Profile"

  # --- Tooltip Construction ---
  if (!is.null(tooltip_vars)) {
    if (!is.null(labels_df)) {
      # Generate tooltip if labels_df available
      data$tooltip_text <- generate_tooltip_text(data, labels_df, tooltip_vars, "ADNCA")
    } else {
      # Fallback to simple paste if labels_df is missing
      valid_vars <- intersect(tooltip_vars, names(data))
      if (length(valid_vars) > 0) {
        parts <- lapply(valid_vars, \(v) paste0(v, ": ", data[[v]]))
        data$tooltip_text <- paste(parts, collapse = "<br>")
      }
    }
  } else {
    data$tooltip_text <- rep(NA_character_, nrow(data))
  }
  # Create color var for aesthetic mapping
  plot_data <- data %>%
    mutate(
      color_var = interaction(!!!syms(color_by), sep = ", "),
      group_var = if (!is.null(group_by)) interaction(!!!syms(group_by)) else NULL
    ) %>%
    arrange(!!sym(x_var))

  aes_args <- list(
    x = rlang::sym(x_var),
    y = rlang::sym(y_var),
    color = rlang::sym("color_var"),
    group = if (!is.null(group_by)) rlang::sym("group_var") else NULL,
    text = rlang::sym("tooltip_text")
  )
  if (!is.null(linetype_by)) {
    aes_args$linetype <- rlang::sym(linetype_by)
  }

  plt <- ggplot(plot_data, do.call(aes, aes_args)) +
    geom_line() +
    geom_point() +
    labs(
      x = x_lab,
      y = y_lab,
      title = title,
      color = paste(color_by, collapse = ", ")
    ) +
    theme_bw()

  # Add optional layers
  optional_layers <- list(
    .add_colour_palette(palette),
    .add_axis_limits(x_limits, y_limits),
    .add_y_scale(ylog_scale),
    .add_faceting(facet_by),
    .add_thr(threshold_value),
    .add_vline(data, vline_var)
  )
  plt + optional_layers
}

# --- Helper Functions (Internal) ---
# These functions contain the optional layers logic

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
      labs(color = paste0(paste(color_by, collapse = ", "), " (95% CI)"))
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
