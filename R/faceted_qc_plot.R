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
                            title = NULL,
                            show_PK_samples = TRUE,
                            show_actual_dose_bars = TRUE,
                            show_nominal_dose_lines = TRUE,
                            as.plotly = FALSE) {

  # Include all variables from the plot in the tooltips
  tooltip_vars <- c(x_var, y_var, colour_var, grouping_vars)
  
  all_colour_levels <- unique(data[[colour_var]])
  all_y_levels <- sort(unique(data[[y_var]]))
  
  # Get dynamic labels for axes and legend
  x_axis_title <- get_label(labels_df, x_var, "ADPC")
  y_axis_title <- get_label(labels_df, y_var, "ADPC")
  legend_title <- get_label(labels_df, colour_var, "ADPC")

  # Build the tooltip text for each row
  processed_data <- data %>%
    mutate(
      colour_factored = factor(!!sym(colour_var), levels = all_colour_levels),
      tooltip_text = generate_tooltip_text(., labels_df, tooltip_vars, "ADPC")
    )

  plot_list <- processed_data %>%
    group_by(across(all_of(grouping_vars))) %>%
    group_split() %>%
  
  
  # solution 1: make ggplot subplots and convert to plotly only if required
  plot_list <- imap(split_processed_data, ~{

    group_name <- .x %>%
      select(all_of(grouping_vars)) %>%
      slice(1) %>%
      paste(collapse = " | ")

    p <- ggplot(data = .x,
           aes(x = !!sym(x_var),
               y = !!sym(y_var),
               colour = colour_factored,
               text = tooltip_text)) +
      geom_point(size = 1) +
      scale_colour_discrete(drop = FALSE) +
      theme_bw() +
      theme(strip.text.y = element_text(angle = -90),
            panel.spacing = unit(0.2, "lines"))


    if(as.plotly) ggplotly(p, tooltip = "text") else p

  })

  
  # solution 2: make ggplotly subplots directly
  # imap(~{
  #   
  #   group_name <- .x %>% 
  #     select(all_of(grouping_vars)) %>% 
  #     slice(1) %>% 
  #     paste(collapse = " | ")
  #   
  #   plot_ly(
  #     data = .x,
  #     x = ~get(x_var),
  #     y = ~get(y_var),
  #     color = ~colour_factored,
  #     text = ~tooltip_text,
  #     type = "scatter",
  #     mode = "markers",
  #     showlegend = (.y == 1)
  #   ) %>%
  #     layout(annotations = list(
  #       x = 0.5, y = 1.05, text = group_name,
  #       showarrow = FALSE, xref = "paper", yref = "paper",
  #       xanchor = "left", yanchor = "middle"
  #     ))
  # })
  # 
  # subplot(plot_list,
  #         nrows = length(plot_list),
  #         shareX = TRUE,
  #         shareY = TRUE,
  #         titleX = TRUE,
  #         titleY = TRUE
  # ) %>%
  #   layout(
  #     title = title,
  #     legend = list(title = list(text = legend_title)),
  #     # REQ 3 & 4: Set axis titles (Y-title will only appear once)
  #     xaxis = list(title = x_axis_title),
  #     yaxis = list(
  #       title = y_axis_title,
  #       # REQ 5: Show all unique y-values on the axis
  #       tickmode = 'array',
  #       tickvals = all_y_levels,
  #       ticktext = all_y_levels
  #     ),
  #     # REQ 1 (cont.): Add margin space for the subplot titles on the right
  #     margin = list(r = 180) 
  #   )
    # geom_point(size = 1.5) +
    # facet_grid(rows = vars(!!!syms(grouping_vars)), scales = "free_y", space = "free_y") +
    # labs(
    #   x = get_label(labels_df, x_var, "ADPC"),
    #   y = get_label(labels_df, y_var, "ADPC"),
    #   title = title,
    #   subtitle = paste("Subjects grouped by",
    #                    paste(grouping_vars, collapse = ", ")),
    #   colour = get_label(labels_df, colour_var, "ADPC")
    # ) +
    # theme_bw() +
    # theme(
    #   # Keep cohort labels horizontal
    #   strip.text.y = element_text(angle = -90),
    #   # Adjust spacing between panels
    #   panel.spacing = unit(0.2, "lines")
    # )

  if (as.plotly) {
    plt %>%
      ggplotly(tooltip = "text") %>%
      layout(title = list(
        text = paste0(p$labels$title, "<br><sup>", p$labels$subtitle, "</sup>")
      ))
  } else {
    plt
  }
}