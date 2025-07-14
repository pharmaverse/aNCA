#' Create a Faceted Scatter Plot for QC
#'
#' Generates a customizable, faceted ggplot scatter plot, typically for quality
#' control (QC). The function supports dynamic labels and automatically
#' prepares tooltips for interactive use with `ggplotly`.
#'
#' @param data A data.frame containing the plotting data.
#' @param x_var Character. The column name to be used for the x-axis.
#' @param y_var Character. The column name to be used for the y-axis.
#' @param color_var Character. The column name to map to the color aesthetic.
#' @param grouping_vars Character vector. Column names used to create vertical
#'   facets (panels).
#' @param labels_df A data.frame used by helper functions (`get_label`,
#'   `generate_tooltip_text`) to look up variable labels.
#' @param title Character. The main title of the plot.
#'
#' @return A `ggplot` object ready to be printed or passed to `ggplotly`.
#'
#' @examples
#' \dontrun{
#' # Dummy helper functions required for the example to run
#' get_label <- function(df, var, type) paste(var, "(units)")
#' generate_tooltip_text <- function(data, ...) "details"
#'
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
#'   color_var = "DOSE",
#'   grouping_vars = "ARM",
#'   labels_df = label_data,
#'   title = "Sample QC Plot"
#'   )
#' }
#'
#' @export
faceted_qc_plot <- function(data,
                            x_var,
                            y_var,
                            color_var,
                            grouping_vars,
                            labels_df,
                            title) {

  # Include all variables from the plot in the tooltips
  tooltip_vars <- c(x_var, y_var, color_var, grouping_vars)

  # Build the tooltip text for each row
  processed_data <- data %>%
    mutate(
      color_factored = as.factor(!!sym(color_var)),
      tooltip_text = generate_tooltip_text(., labels_df, tooltip_vars, "ADPC")
      )

  plt <- ggplot(processed_data,
                aes(x = !!sym(x_var),
                    y = !!sym(y_var),
                    color = color_factored,
                    text = tooltip_text)) +
    geom_point(size = 1.5) +
    facet_grid(rows = vars(!!!syms(grouping_vars)), scales = "free_y", space = "free_y") +
    labs(
      x = get_label(labels_df, x_var, "ADPC"),
      y = get_label(labels_df, y_var, "ADPC"),
      title = title,
      subtitle = paste("Subjects grouped by",
                       paste(grouping_vars, collapse = ", ")),
      color = get_label(labels_df, color_var, "ADPC")
      ) +
    theme_bw() +
    theme(
      # Keep cohort labels horizontal
      strip.text.y = element_text(angle = -90),
      # Adjust spacing between panels
      panel.spacing = unit(0.2, "lines")
    )

  return(plt)
}

data <- data.frame(
  "USUBJID" = as.character(c(rep(1:5, each = 3))),
  "COHORT" = c(rep("ARM A", 9), rep("ARM B", 6)),
  "SEX" = c(rep("F", 3), rep("M", 12)),
  "AFRLT" = c(0, 2, 4,
              0, 3, 5,
              0, 3, 6,
              0.5, 2, 4,
              0, 1.5, 5),
  "DOSEA" = c(rep(c(11, 100, 110), 5))
)

ggplotly(dose_plot(data = data,
                   x_var = "AFRLT",
                   y_var = "USUBJID",
                   color_var = "DOSEA",
                   grouping_vars = c("COHORT", "SEX"),
                   labels_df = LABELS),
         tooltip = "text")



df = read.csv("inst/shiny/data/Dummy_complex_data.csv")

ggplotly(dose_plot(data = df,
                   x_var = "AFRLT",
                   y_var = "USUBJID",
                   color_var = "DOSEA",
                   grouping_vars = c("TRT01A", "SEX"),
                   labels_df = LABELS),
         tooltip = "text")
