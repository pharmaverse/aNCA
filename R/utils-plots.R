#' Tooltip construction helper
#' @param data the dataframe
#' @param tooltip_vars character vector of tooltip variables to extract
#' @param labels_df data.frame used for label lookups in tooltips
#' @returns data with added tooltip_text column
.handle_tooltips <- function(data, tooltip_vars, labels_df) {

  data$tooltip_text <- NA_character_

  if (nrow(data) > 0) {
    # 1. Round numeric tooltip variables for cleaner display
    numeric_tt_vars <- intersect(tooltip_vars, names(data)[sapply(data, is.numeric)])
    if (length(numeric_tt_vars) > 0) {
      data <- data %>%
        mutate(across(all_of(numeric_tt_vars), ~ round(., digits = 2)))
    }

    # 2. Generate Tooltip Text
    if (!is.null(tooltip_vars)) {
      if (!is.null(labels_df)) {
        # Use the shared helper function if available and labels provided
        data$tooltip_text <- generate_tooltip_text(data, labels_df, tooltip_vars, "ADNCA")
      } else {
        # Fallback: Create simple "Var: Value" string
        valid_vars <- intersect(tooltip_vars, names(data))
        if (length(valid_vars) > 0) {
          parts <- lapply(valid_vars, \(v) paste0(v, ": ", data[[v]]))
          data$tooltip_text <- paste(parts, collapse = "<br>")
        }
      }
    }
  }

  data
}

#' Format numeric axis breaks for display
#'
#' Renders whole numbers without a decimal part, so a break at `12` is labeled `12` rather
#' than `12.0`, and leaves fractional breaks such as `-0.083` untouched. Used both as the
#' `labels` argument of the concentration plot scales and as the default of
#' [filter_breaks()], so the breaks are filtered against the labels that actually get drawn.
#'
#' @param x A numeric vector of break positions.
#'
#' @returns A character vector of labels, one per element of `x`.
#' @keywords internal
format_axis_labels <- function(x) {
  ifelse(x %% 1 == 0, as.character(as.integer(x)), as.character(x))
}

#' Replace a concentration plot's x scale with filtered breaks
#'
#' Applies [filter_breaks()] to `break_values` and puts the surviving breaks on the plot's x
#' scale. Must be added after any faceting, because a faceted plot splits the panel and so
#' has room for fewer labels than the same plot drawn as a single panel.
#'
#' @param plot            A ggplot object, complete apart from its x scale.
#' @param break_values    A numeric vector of candidate breaks.
#' @param min_cm_distance A numeric of the minimum distance between breaks.
#'
#' @returns The plot with its x scale replaced.
#' @keywords internal
add_filtered_x_scale <- function(plot, break_values, min_cm_distance) {
  suppressMessages(
    plot +
      scale_x_continuous(
        guide = guide_axis(n.dodge = 1),
        breaks = filter_breaks(
          break_values,
          min_cm_distance = min_cm_distance,
          plot = plot
        ),
        labels = format_axis_labels
      )
  )
}

#' Create a simple error plot with a message
#'
#' This internal function generates a minimal ggplot2 plot displaying a given error message.
#' It is used to return a plot object with a custom error message.
#'
#' @param msg Character string. The error message to display in the plot.
#'
#' @returns A ggplot object with the error message displayed.
#' @keywords internal
error_plot <- function(msg) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = msg, size = 6, hjust = 0.5, vjust = 0.5) +
    theme_void() +
    ggtitle("Error")
}
