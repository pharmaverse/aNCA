#' Create a Faceted Scatter Plot for QC
#'
#' Generates a customizable, faceted ggplot scatter plot, typically for quality
#' control (QC). The function supports dynamic labels and automatically
#' prepares tooltips for interactive use with `ggplotly`.
#'
#' @param data A data.frame containing the plotting data.
#' @param x_var Character. The column name to be used for the x-axis.
#' @param y_var Character. The column name to be used for the y-axis.
#' @param colour_var Character. The column name to map to the colour aesthetic.
#' @param grouping_vars Character vector. Column names used to create vertical
#'   facets (panels).
#' @param labels_df A data.frame used by helper functions (`get_label`,
#'   `generate_tooltip_text`) to look up variable labels.
#' @param title Character. The main title of the plot.
#'
#' @return A `ggplot` object ready to be printed or passed to `ggplotly`.
#'
#'
#' @examples
#' # Sample data
#' qc_data <- data.frame(
#'   TIME = 1:6,
#'   RESULT = c(5, 6, 8, 9, 12, 11),
#'   DOSE = as.factor(c(10, 10, 20, 20, 30, 30)),
#'   ARM = rep(c("A", "B", "C"), each = 2)
#'   )
#' label_data <- data.frame() # Dummy labels object
#'
#' # Generate the plot
#' faceted_qc_plot(
#'   data = qc_data,
#'   x_var = "TIME",
#'   y_var = "RESULT",
#'   colour_var = "DOSE",
#'   grouping_vars = "ARM",
#'   labels_df = label_data,
#'   title = "Sample QC Plot"
#'   )
#'
#' @export
faceted_qc_plot <- function(data,
                            x_var,
                            y_var,
                            colour_var,
                            grouping_vars,
                            labels_df = data.frame(),
                            title = NULL) {

  # Include all variables from the plot in the tooltips
  tooltip_vars <- c(x_var, y_var, colour_var, grouping_vars)

  # Build the tooltip text for each row
  processed_data <- data %>%
    mutate(
      colour_factored = as.factor(!!sym(colour_var)),
      tooltip_text = generate_tooltip_text(
        data = .,
        labels_df = labels_df,
        tooltip_vars = tooltip_vars,
        type = "ADPC"
      )
    )

  plt <- ggplot(processed_data,
                aes(x = !!sym(x_var),
                    y = !!sym(y_var),
                    colour = colour_factored,
                    text = tooltip_text)) +
    geom_point(size = 1.5) +
    facet_grid(rows = vars(!!!syms(grouping_vars)), scales = "free_y", space = "free_y") +
    labs(
      x = get_label(variable = x_var, type =  "ADPC", labels_df = labels_df),
      y = get_label(variable = y_var, type =  "ADPC", labels_df = labels_df),
      title = title,
      subtitle = paste("Subjects grouped by",
                       paste(grouping_vars, collapse = ", ")),
      colour = get_label(variable = colour_var, type =  "ADPC", labels_df = labels_df)
    ) +
    theme_bw() +
    theme(
      # Keep cohort labels horizontal
      strip.text.y = element_text(angle = -90),
      # Adjust spacing between panels
      panel.spacing = unit(0.2, "lines")
    )

  plt
}