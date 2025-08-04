#' Create a Faceted Quality Control (QC) Plot
#'
#' Generates a faceted QC plot by layering concentration data (as
#' black shapes) and dose data (as colored points). It creates a single, unified
#' legend for both data types and can return either a static `ggplot` or an
#' interactive `plotly` object.
#'
#' @details Unless specified, the variables required as arguments are expected to
#' be present in both `data_conc` and `data_dose`.
#'
#' @param data_conc A data.frame containing concentration data (e.g., PK samples).
#' @param data_dose An optional data.frame containing dosing information.
#' @param x_var Character. The column name to be used for the x-axis.
#' @param y_var Character. The column name to be used for the y-axis.
#' @param colour_var Character. The column in `data_dose` to map to color.
#' @param shape_var Character. The column in `data_conc` to map to shape.
#' @param grouping_vars Character vector. Column names to use for faceting.
#' @param other_tooltip_vars Optional character vector of additional column names
#'   to include in the tooltip.
#' @param x_var_units Character. The column name containing the units for the
#'   x-axis variable. It is expected that this column contains a single unique value.
#' @param colour_var_units Character. The column name for the units of the
#'   colour variable in `data_dose`. It is expected that this column contains a
#'   single unique value.
#' @param labels_df A data.frame used by helper functions to look up variable labels.
#' @param title Character. The main title for the plot.
#' @param show_pk_samples Logical. If `TRUE`, plots the concentration data.
#' @param show_doses Logical. If `TRUE`, plots the dose data.
#' @param as_plotly Logical. If `TRUE`, converts the final plot to an interactive
#'   `plotly` object.
#'
#' @return A `ggplot` object or, if `as_plotly = TRUE`, a `plotly` object.
#'
#' @importFrom purrr pmap_chr
#' @importFrom scales hue_pal
#'
#' @export
#' @examples
#' # Dummy helper functions required for the example
#' # Sample concentration data
#' conc_data <- data.frame(
#'   USUBJID = rep(paste0("S-", 1:2), each = 2),
#'   ACTUAL_TIME = c(0, 24, 0, 24),
#'   SAMPLE_TYPE = rep(c("PLASMA", "URINE"), 2),
#'   COHORT = "A",
#'   TIME_UNIT = "hr"
#'   )
#'
#' # Sample dose data
#' dose_data <- data.frame(
#'   USUBJID = rep(paste0("S-", 1:2), each = 1),
#'   ACTUAL_TIME = c(0, 0),
#'   DOSE_LEVEL = c(100, 100),
#'   COHORT = "A",
#'   DOSE_UNIT = "mg"
#'  )
#'
#' # Generate the plot
#' faceted_qc_plot(
#'   data_conc = conc_data,
#'   data_dose = dose_data,
#'   x_var = "ACTUAL_TIME",
#'   y_var = "USUBJID",
#'   colour_var = "DOSE_LEVEL",
#'   shape_var = "SAMPLE_TYPE",
#'   grouping_vars = "COHORT",
#'   x_var_units = "TIME_UNIT",
#'   colour_var_units = "DOSE_UNIT",
#'   title = "Sample Dosing and PK Plot"
#'  )
#' @export
faceted_qc_plot <- function(data_conc,
                            data_dose = NULL,
                            x_var,
                            y_var,
                            colour_var,
                            shape_var,
                            grouping_vars,
                            other_tooltip_vars = NULL,
                            x_var_units = NULL,
                            colour_var_units = NULL,
                            labels_df = data.frame(),
                            title = NULL,
                            show_pk_samples = TRUE,
                            show_doses = TRUE,
                            as_plotly = FALSE) {

  # Define boolean flags
  plot_conc_data <- show_pk_samples && !is.null(data_conc)
  plot_dose_data <- show_doses && !is.null(data_dose) && nrow(data_dose) > 0

  # Define the levels for the shape and colour variables
  if (plot_conc_data) {
    shape_levels <- sort(unique(data_conc[[shape_var]]))
  } else {
    shape_levels <- character()
  }

  if (plot_dose_data) {
    colour_levels <- as.character(sort(unique(data_dose[[colour_var]])))
  } else {
    colour_levels <- character()
  }

  # Select variables to include in the plotly tooltips
  tooltip_vars <- c(y_var, grouping_vars, other_tooltip_vars, x_var, colour_var)

  # Preprocess the data
  plot_data_list <- list()
  if (plot_conc_data) {
    plot_data_list$conc <- data_conc %>%
      mutate(
        legend_group = as.character(!!sym(shape_var)),
        tooltip_text = generate_tooltip_text(., labels_df, tooltip_vars, "ADPC")
      )
  }
  if (plot_dose_data) {
    plot_data_list$dose <- data_dose %>%
      mutate(
        legend_group = as.character(!!sym(colour_var)),
        tooltip_text = generate_tooltip_text(., labels_df, tooltip_vars, "ADPC")
      )
  }

  # Return an empty plot for empty datasets
  if (length(plot_data_list) == 0)
    return(ggplot() + labs(title = "No data to display."))

  # Combine data and create tooltip text
  all_legend_levels <- unique(c(shape_levels, colour_levels))
  processed_data <- bind_rows(plot_data_list) %>%
    mutate(
      legend_group = factor(legend_group, levels = all_legend_levels),
      facet_title = pmap_chr(
        select(., all_of(grouping_vars)),
        ~ paste(list(...), collapse = ", ")
      )
    )

  # Define shapes
  shape_values <- setNames(
    c(
      # select specific shapes for PK samples
      rep(c(1, 4, 5, 0, 2, 6, 3), length.out = length(shape_levels)),
      # select a filled-in circle for doses
      rep(16, length(colour_levels))
    ),
    all_legend_levels
  )

  # Define colors: black for PK samples, hue palette for doses
  if (length(colour_levels) > 0) {
    dose_colours <- scales::hue_pal()(length(colour_levels))
  } else {
    dose_colours <- character()
  }
  colour_values <- setNames(c(rep("black", length(shape_levels)), dose_colours), all_legend_levels)

  # If unique and available, format units to a string for plot labels
  x_unit_lab <- processed_data %>% format_unit_string(x_var_units)
  colour_unit_lab <- processed_data %>% format_unit_string(colour_var_units)

  # Define a title for the legend
  legend_title <- paste(
    paste(
      if (plot_conc_data) get_label(labels_df, shape_var, "ADPC") else "",
      if (plot_dose_data) paste0(get_label(labels_df, colour_var, "ADPC"), colour_unit_lab) else "",
      sep = "<br>"
    ),
    "<br>"
  )

  # Build the plot
  p <- ggplot(
    processed_data,
    aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      text = tooltip_text,
      colour = legend_group,
      shape = legend_group
    )
  ) +
    geom_point(aes(alpha = legend_group), size = 2.5, stroke = 0.25) +
    facet_wrap(vars(facet_title), scales = "free_y", ncol = 1) +

    # Apply the manual scales
    scale_shape_manual(name = legend_title, values = shape_values) +
    scale_colour_manual(name = legend_title, values = colour_values) +

    # Make doses semi-transparent and hide the alpha legend
    scale_alpha_manual(values = setNames(c(rep(1, length(shape_levels)),
                                           rep(0.6, length(colour_levels))),
                                         all_legend_levels), guide = "none") +
    labs(
      x = paste0(get_label(labels_df, x_var, "ADPC"), x_unit_lab),
      y = get_label(labels_df, y_var, "ADPC"),
      title = title
    ) +
    theme_bw()

  if (as_plotly) {
    ggplotly(p, tooltip = "text") %>%
      layout(title = list(text = p$labels$title), legend = list(traceorder = "normal"))
  } else {
    p
  }
}


#' Formats a unit string if a unique unit exists
#' @param data The data frame to check.
#' @param unit_var The column name of the unit variable.
#' @return A formatted string like " (hr)" or an empty string "".
format_unit_string <- function(data, unit_var) {
  # Return "" if the unit variable is not specified or doesn't exist
  if (is.null(unit_var) || !all(unit_var %in% names(data))) {
    return("")
  }

  # Get the distinct unit values
  distinct_units <- data %>%
    select(all_of(unit_var)) %>%
    distinct()

  # If there is exactly one unique unit, format it. Otherwise, return "".
  if (nrow(distinct_units) == 1) {
    unit_value <- distinct_units %>% pull()
    paste0(" (", unit_value, ")")
  } else {
    ""
  }
}