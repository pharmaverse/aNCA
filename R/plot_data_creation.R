#' Create an Individual PK Line Plot
#'
#' Generates a line plot for individual pharmacokinetic (PK) concentration-time profiles (spaghetti plots).
#' Supports filtering by subject, analyte, specimen, and profile, and allows customization of axes, color, faceting, and tooltips.
#'
#' @param pknca_data A PKNCAdata object containing the raw PK concentration data.
#' @param color_by Character vector specifying the column(s) used to color the lines and points.
#' @param facet_by Character vector of column names to facet the plot by. Default is `NULL` (no faceting).
#' @param ylog_scale Logical; whether to use a logarithmic scale for the y-axis. Default is `FALSE`.
#' @param threshold_value Numeric; y-intercept for a horizontal threshold line. Default is `NULL` (no threshold).
#' @param show_dose Logical; if `TRUE`, vertical lines for dose times are shown. Default is `FALSE`.
#' @param palette Optional palette name or named character vector of colors for the plot. Default is "default".
#' @param tooltip_vars Character vector of column names to include in the tooltip. Default is `NULL`.
#' @param labels_df Optional data.frame for variable label lookups (for tooltips). Default is `NULL`.
#' @param filtering_list Named list of filters (column = allowed values). Default is `NULL` (no filtering).
#' @param use_time_since_last_dose Logical; if `TRUE`, x-axis represents time since last dose. Default is `FALSE` (time since first dose).
#'
#' @return A `ggplot` object representing the individual PK line plot.
#' @seealso [g_lineplot()], [process_data_individual()]
#' @export
exploration_individualplot <- function(
    pknca_data,
    color_by,
    facet_by = NULL,
    ylog_scale = FALSE,
    threshold_value = NULL,
    show_dose = FALSE,
    palette = "default",
    tooltip_vars = NULL,
    labels_df = NULL,
    filtering_list = NULL,
    use_time_since_last_dose = FALSE
) {

  individual_data <- process_data_individual(
    pknca_data = pknca_data,
    filtering_list = filtering_list,
    ylog_scale = ylog_scale,
    show_dose = show_dose,
    use_time_since_last_dose = use_time_since_last_dose
  )

  g_lineplot(
    data = individual_data,
    x_var = pknca_data$conc$columns$time,
    y_var = pknca_data$conc$columns$concentration,
    x_unit = pknca_data$conc$columns$timeu,
    y_unit = pknca_data$conc$columns$concu,
    color_by = color_by,
    facet_by = facet_by,
    group_by = pknca_data$conc$columns$subject,
    ylog_scale = ylog_scale,
    threshold_value = threshold_value,
    palette = palette,
    tooltip_vars = tooltip_vars,
    labels_df = labels_df,
    vline_var = if (show_dose) "TIME_DOSE" else NULL
  )
}


#' Create a Mean PK Line Plot
#'
#' Generates a line plot for mean pharmacokinetic (PK) concentration-time profiles.
#' Supports error bars (SD), confidence intervals, faceting, and custom tooltips, and allows filtering by analyte, specimen, and profile.
#'
#' @inheritParams exploration_individualplot
#' @param sd_min Logical; if `TRUE`, plot lower SD error bars. Default is `FALSE`.
#' @param sd_max Logical; if `TRUE`, plot upper SD error bars. Default is `FALSE`.
#' @param ci Logical; if `TRUE`, plot 95% confidence interval ribbon. Default is `FALSE`.
#' @param tooltip_vars Character vector of column names to include in the tooltip. Default includes dose group vars and "Mean".
#' @return A `ggplot` object representing the mean PK line plot, with error bars and/or confidence intervals if requested.
#' @seealso [g_lineplot()], [process_data_mean()]
#' @export
exploration_meanplot <- function(
    pknca_data,
    color_by,
    facet_by = NULL,
    ylog_scale = FALSE,
    threshold_value = NULL,
    show_dose = FALSE,
    palette = "default",
    sd_min = FALSE,
    sd_max = FALSE,
    ci = FALSE,
    tooltip_vars = c(group_vars(pknca_data$dose), "Mean"),
    labels_df = NULL,
    filtering_list = NULL,
    use_time_since_last_dose = FALSE
) {

  mean_data <- process_data_mean(
    pknca_data = pknca_data,
    extra_grouping_vars = c(color_by, facet_by),
    filtering_list = filtering_list,
    ylog_scale = ylog_scale,
    show_dose = show_dose,
    use_time_since_last_dose = use_time_since_last_dose
  )

  plot <- g_lineplot(
    data = mean_data,
    x_var = "TIME_NOMINAL",
    y_var = "Mean",
    x_unit = pknca_data$conc$columns$timeu,
    y_unit = pknca_data$conc$columns$concu,
    color_by = color_by,
    facet_by = facet_by,
    group_by = color_by,
    ylog_scale = ylog_scale,
    threshold_value = threshold_value,
    palette = palette,
    tooltip_vars = tooltip_vars,
    labels_df = labels_df,
    vline_var = if (show_dose) "TIME_DOSE" else NULL
  )

  # If there is no mean data, just return the plot
  if (nrow(mean_data) == 0) {
    return(plot)
  }

  # Add final layers for meanplot with error bars and CIs
  finalize_meanplot(
    plot = plot,
    sd_min = sd_min,
    sd_max = sd_max,
    ci = ci,
    color_by = color_by,
    x_var = "TIME_NOMINAL",
    y_var = "Mean"
  )
}

#' Process data for individual line plot
#'
#' Creates a filtered data frame for individual spaghetti plots, with optional alignment to last dose
#' (time since last dose) and derivation of dose times in a new column (TIME_DOSE).
#'
#' @param pknca_data A PKNCAdata object containing concentration and dose data.
#' @param filtering_list Named list of filters (column = allowed values). Default is `NULL` (no filtering).
#' @param ylog_scale Logical; if `TRUE`, removes non-positive concentrations. Default is `FALSE`.
#' @param conc_col Name of the concentration column. Default is "AVAL".
#' @param show_dose Logical; if `TRUE`, derives dose times and includes TIME_DOSE column. Default is `FALSE`.
#' @param use_time_since_last_dose Logical; if `TRUE`, x-axis represents time since last dose. Default is `FALSE` (time since first dose).
#'
#' @return Data frame filtered and ready for individual spaghetti plots, with optional TIME_DOSE column.
#' @keywords internal
#' @noRd
process_data_individual <- function(pknca_data,
                                    filtering_list = NULL,
                                    ylog_scale = FALSE,
                                    conc_col = "AVAL",
                                    show_dose = FALSE,
                                    use_time_since_last_dose = FALSE) {
  data <- if (show_dose || use_time_since_last_dose) {
    data <- derive_last_dose_time(
      pknca_data = pknca_data,
      conc_time_col = pknca_data$conc$columns$time
    )
    if (use_time_since_last_dose) {
      data <- dplyr::mutate(data, !!pknca_data$conc$columns$time := !!sym(pknca_data$conc$columns$time) - TIME_DOSE)
    }
    data
  } else {
    pknca_data$conc$data
  }
  processed_data <- filter_by_list(data, filtering_list) %>%
    dplyr::filter(!is.na(!!sym(conc_col)))
  if (isTRUE(ylog_scale)) {
    processed_data <- processed_data %>% dplyr::filter(!!sym(conc_col) > 0)
  }
  processed_data
}

#' Process data for mean PK line plot
#'
#' Creates a summarised data frame for mean PK concentration-time profiles, with optional alignment to last dose and dose time aggregation.
#'
#' @param pknca_data A PKNCAdata object containing concentration and dose data.
#' @param extra_grouping_vars Character vector of extra grouping variables to include in summary. Default is `NULL`.
#' @param filtering_list Named list of filters (column = allowed values). Default is `NULL` (no filtering).
#' @param ylog_scale Logical; if `TRUE`, removes non-positive means. Default is `FALSE`.
#' @param show_dose Logical; if `TRUE`, derives dose times and includes TIME_DOSE column. Default is `FALSE`.
#' @param use_time_since_last_dose Logical; if `TRUE`, x-axis represents time since last dose. Default is `FALSE` (time since first dose).
#'
#' @return Data frame summarised by group, with columns for Mean, SD, N, SE, SD_min, SD_max, CI_lower, CI_upper, and optional TIME_DOSE.
#' @keywords internal
#' @noRd
process_data_mean <- function(pknca_data,
                              extra_grouping_vars = NULL,
                              filtering_list = NULL,
                              ylog_scale = FALSE,
                              show_dose = FALSE,
                              use_time_since_last_dose = FALSE) {
  # Deduce columns and data
  x_var_unit <- pknca_data$conc$columns$timeu
  y_var_unit <- pknca_data$conc$columns$concu
  x_var <- if (use_time_since_last_dose) "NRRLT" else pknca_data$conc$columns$time.nominal
  y_var <- pknca_data$conc$columns$concentration
  dose_group_cols <- setdiff(group_vars(pknca_data$dose), pknca_data$conc$columns$subject)
  grouping_cols <- unique(c(extra_grouping_vars, "TIME_NOMINAL", x_var_unit, y_var_unit, dose_group_cols))
  grouping_cols <- if (show_dose) c(grouping_cols, "TIME_DOSE") else grouping_cols

  data <- if (show_dose) {
    derive_last_dose_time(
      pknca_data = pknca_data,
      conc_time_col = x_var
    )
  } else {
    pknca_data$conc$data
  }

  # Always create a TIME_NOMINAL column for the x variable
  data <- dplyr::mutate(data, TIME_NOMINAL = !!rlang::sym(x_var))

  processed <- filter_by_list(data, filtering_list) %>%
    dplyr::filter(!is.na(!!rlang::sym(y_var)))

  if (show_dose && !is.null(dose_group_cols) && !is.null(x_var)) {
    processed <- processed %>%
      dplyr::group_by(!!!rlang::syms(dose_group_cols), TIME_NOMINAL) %>%
      dplyr::mutate(TIME_DOSE = mean(TIME_DOSE, na.rm = TRUE)) %>%
      dplyr::ungroup()
  }

  summarised_data <- processed %>%
    dplyr::group_by(!!!rlang::syms(grouping_cols)) %>%
    dplyr::summarise(
      Mean = round(mean(!!rlang::sym(y_var), na.rm = TRUE), 3),
      SD = sd(!!rlang::sym(y_var), na.rm = TRUE),
      N = dplyr::n(),
      SE = SD / sqrt(N),
      .groups = "drop"
    ) %>%
    dplyr::filter(N >= 3) %>%
    dplyr::mutate(
      SD_min = Mean - SD,
      SD_max = Mean + SD,
      CI_lower = Mean - 1.96 * SE,
      CI_upper = Mean + 1.96 * SE
    )
  if (isTRUE(ylog_scale)) {
    summarised_data <- summarised_data %>%
      dplyr::filter(Mean > 0)
  }
  summarised_data
}

#' Filter a data frame by a list of column-value pairs
#'
#' Filters a data frame by a named list of column-value pairs, using apply_filters for logic.
#'
#' @param data A data frame to filter.
#' @param filtering_list A named list where each name is a column and each value is a vector of allowed values.
#'
#' @return Filtered data frame.
#' @keywords internal
#' @noRd
filter_by_list <- function(data, filtering_list) {
  if (is.null(filtering_list) || length(filtering_list) == 0) return(data)
  filters <- lapply(names(filtering_list), function(var) {
    list(column = var, condition = "==", value = filtering_list[[var]])
  })
  apply_filters(data, filters)
}

#' Finalize mean PK plot by adding label prefixes and mean layers (SD error bars, confidence intervals)
#'
#' @param plot The ggplot object to finalize.
#' @param sd_min Logical; if TRUE, plot lower SD error bars.
#' @param sd_max Logical; if TRUE, plot upper SD error bars.
#' @param ci Logical; if TRUE, plot 95% confidence interval ribbon.
#' @param color_by Character vector for color grouping.
#' @param y_var Name of the y variable.
#' @param x_var Name of the x variable.
#'
#' @return Finalized ggplot object for mean PK plot.
#' @keywords internal
#' @noRd
finalize_meanplot <- function(plot, sd_min, sd_max, ci, color_by, y_var, x_var) {
  plot_build <- ggplot2::ggplot_build(plot)

  plot +
    labs(
      x = paste("Nominal", plot_build$plot$labels$x),
      y = paste("Mean", plot_build$plot$labels$y),
      title = paste("Mean", plot_build$plot$labels$title)
    ) +
    list(
      .add_mean_layers(
        sd_min = sd_min,
        sd_max = sd_max,
        ci = ci,
        color_by = color_by,
        y_var = y_var,
        x_var = x_var,
        group_var = "color_var"
      )
    )
}

#' Derive last dose time for each sample in the concentration data of a PKNCAdata object
#'
#' Adds a TIME_DOSE column to the concentration data, representing the last dose time for each sample.
#'
#' @param pknca_data A PKNCAdata object containing concentration and dose data.
#' @param conc_time_col Name of the time column in concentration data. Default is pknca_data$conc$columns$time.
#'
#' @return Data frame with TIME_DOSE column added, representing the last dose time for each sample.
#' @keywords internal
#' @noRd
derive_last_dose_time <- function(pknca_data, conc_time_col = pknca_data$conc$columns$time) {
  conc_data <- pknca_data$conc$data
  dose_data <- pknca_data$dose$data
  dose_time_col <- pknca_data$dose$columns$time
  dose_group_vars <- group_vars(pknca_data$dose)

  dplyr::left_join(
    conc_data,
    dose_data %>%
      dplyr::mutate(TIME_DOSE = !!rlang::sym(dose_time_col)) %>%
      dplyr::select(!!!rlang::syms(c(dose_group_vars, "TIME_DOSE"))),
    by = dose_group_vars
  ) %>%
    dplyr::filter(TIME_DOSE <= !!rlang::sym(conc_time_col)) %>%
    dplyr::group_by(!!!rlang::syms(setdiff(names(conc_data), "TIME_DOSE"))) %>%
    dplyr::arrange(TIME_DOSE) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
}