#' General Line Plot Function
#'
#' Creates a ggplot line plot for either individual or mean PK data.
#'
#' @param data A processed data frame.
#' @param x_var,y_var,group_var Strings specifying the variables for aesthetics.
#' @param ... Additional plotting parameters passed as a list.
#' @returns A ggplot object.
#' @export
g_lineplot <- function(data, x_var, y_var, group_var, ...) {
  args <- list(...)
  
  # Set up plot labels - assumes individual plot unless mean columns are present
  is_mean_plot <- all(c("Mean", "SD", "N") %in% names(data))
  if (is_mean_plot) {
    x_lab <- paste0("Nominal Time [", unique(data$RRLTU), "]")
    y_lab <- paste0("Mean Concentration [", unique(data$AVALU), "]")
    title <- "Mean PK Concentration - Time Profile"
  } else {
    x_lab <- paste0("Time [", unique(data$RRLTU), "]")
    y_lab <- paste0("Concentration [", unique(data$AVALU), "]")
    title <- "PK Concentration - Time Profile"
  }
  
  plt <- ggplot(data, aes(
    x = .data[[x_var]],
    y = .data[[y_var]],
    color = color_var,
    group = .data[[group_var]]
  )) +
    geom_line() +
    geom_point() +
    labs(
      x = x_lab,
      y = y_lab,
      title = title,
      color = paste(args$colorby_var, collapse = ", ")
    ) +
    theme_bw()
  
  # Add optional layers
  if (!is.null(args$palette)) {
    plt <- plt + scale_color_manual(values = args$palette)
  }
  
  if (args$yaxis_scale == "log") {
    plt <- plt +
      scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000), labels = scales::comma)
  }
  
  if (!is.null(args$facet_by) && length(args$facet_by) > 0) {
    plt <- plt + facet_wrap(vars(!!!syms(args$facet_by)), scales = "free")
  }
  
  if (isTRUE(args$show_threshold)) {
    plt <- plt + geom_hline(yintercept = args$threshold_value, linetype = "dotted", color = "red")
  }
  
  if (isTRUE(args$show_dose)) {
    dose_info <- args$dose_data %>%
      select(all_of(unique(c(args$facet_by, "TIME_DOSE", "DOSEA")))) %>%
      distinct() %>%
      filter(!is.na(TIME_DOSE))
    plt <- plt + geom_vline(data = dose_info, aes(xintercept = TIME_DOSE), linetype = "dotted", color = "grey")
  }
  
  # Mean plot specific layers
  if (is_mean_plot) {
    if (isTRUE(args$show_sd_min) || isTRUE(args$show_sd_max)) {
      ymin_val <- if (isTRUE(args$show_sd_min)) data$SD_min else data$Mean
      ymax_val <- if (isTRUE(args$show_sd_max)) data$SD_max else data$Mean
      plt <- plt + geom_errorbar(aes(ymin = ymin_val, ymax = ymax_val), width = 0.4)
    }
    if (isTRUE(args$show_ci)) {
      plt <- plt +
        geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = color_var), alpha = 0.3) +
        guides(fill = "none") +
        labs(color = paste0(paste(args$colorby_var, collapse = ", "), " (95% CI)"))
    }
  }
  
  return(plt)
}
