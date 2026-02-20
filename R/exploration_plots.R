#' Create an Individual PK Line Plot
#'
#' Generates a line plot for individual pharmacokinetic (PK) concentration-time profiles
#'
#' @param pknca_data A PKNCAdata object containing the raw PK concentration data.
#' @param color_by Character vector specifying the column(s) used to color the lines and points.
#' @param facet_by Character vector of column names to facet the plot by.
#' Default is `NULL` (no faceting).
#' @param show_facet_n Logical; if `TRUE`, shows the number of subjects in each facet.
#' Default is `FALSE`.
#' @param ylog_scale Logical; whether to use a logarithmic scale for the y-axis. Default is `FALSE`.
#' @param show_legend Logical; whether to display the plot legend. Default is `TRUE`.
#' @param threshold_value Numeric; y-intercept for a horizontal threshold line.
#' Default is `NULL` (no threshold).
#' @param x_limits Numeric vector of length 2 for x-axis limits (min, max).
#' Default is `NULL` (no limits).
#' @param y_limits Numeric vector of length 2 for y-axis limits (min, max).
#' Default is `NULL` (no limits).
#' @param show_dose Logical; if `TRUE`, vertical lines for dose times are shown. Default is `FALSE`.
#' @param palette Optional palette name or named character vector of colors for the plot.
#' Default is "default" color palette.
#' @param tooltip_vars Character vector of column names to include in the tooltip.
#' Default is `NULL`.
#' @param labels_df Optional data.frame for variable label lookups (for tooltips).
#' Default is `NULL`.
#' @param filtering_list Named list of filters (column = allowed values).
#' Default is `NULL` (no filtering).
#' @param use_time_since_last_dose Logical; if `TRUE`, x-axis represents time since last dose.
#' Default is `FALSE` (time since first dose).
#'
#' @return A `ggplot` object representing the individual PK line plot.
#' @export
exploration_individualplot <- function(
    pknca_data,
    color_by,
    facet_by = NULL,
    show_facet_n = FALSE,
    ylog_scale = FALSE,
    show_legend = TRUE,
    threshold_value = NULL,
    x_limits = NULL,
    y_limits = NULL,
    show_dose = FALSE,
    palette = "default",
    tooltip_vars = NULL,
    labels_df = NULL,
    filtering_list = NULL,
    use_time_since_last_dose = FALSE) {
  individual_data <- process_data_individual(
    pknca_data = pknca_data,
    filtering_list = filtering_list,
    ylog_scale = ylog_scale,
    show_dose = show_dose,
    use_time_since_last_dose = use_time_since_last_dose
  )

  # If no tooltip variables defined use some default ones
  if (is.null(tooltip_vars)) {
    tooltip_vars <- unique(c(
      pknca_data$conc$columns$time,
      pknca_data$conc$columns$timeu,
      pknca_data$conc$columns$concentration,
      pknca_data$conc$columns$concu,
      color_by
    ))
  }
  g_lineplot(
    data = individual_data,
    x_var = pknca_data$conc$columns$time,
    y_var = pknca_data$conc$columns$concentration,
    x_unit = pknca_data$conc$columns$timeu,
    y_unit = pknca_data$conc$columns$concu,
    color_by = color_by,
    facet_by = facet_by,
    facet_count_n = if (isTRUE(show_facet_n)) pknca_data$conc$columns$subject else NULL,
    group_by = pknca_data$conc$columns$subject,
    x_limits = x_limits,
    y_limits = y_limits,
    ylog_scale = ylog_scale,
    threshold_value = threshold_value,
    palette = palette,
    tooltip_vars = tooltip_vars,
    labels_df = labels_df,
    vline_var = if (show_dose) "TIME_DOSE" else NULL,
    show_legend = show_legend
  )
}


#' Create a Mean PK Line Plot
#'
#' Generates a line plot for mean pharmacokinetic (PK) concentration-time profiles
#'
#' @inheritParams exploration_individualplot
#' @param sd_min Logical; if `TRUE`, plot lower SD error bars. Default is `FALSE`.
#' @param sd_max Logical; if `TRUE`, plot upper SD error bars. Default is `FALSE`.
#' @param ci Logical; if `TRUE`, plot 95% confidence interval ribbon. Default is `FALSE`.
#' @param tooltip_vars Character vector of column names to include in the tooltip.
#' Default includes dose group vars and "AVAL" (renamed from Mean).
#' @param x_limits Numeric vector of length 2 for x-axis limits (min, max).
#' Default is `NULL` (no limits).
#' @param y_limits Numeric vector of length 2 for y-axis limits (min, max).
#' Default is `NULL` (no limits).
#' @return A `ggplot` object representing the mean PK line plot,
#' with error bars and/or confidence intervals if requested.
#' @export
exploration_meanplot <- function(
  pknca_data,
  color_by,
  facet_by = NULL,
  show_facet_n = FALSE,
  ylog_scale = FALSE,
  show_legend = TRUE,
  threshold_value = NULL,
  show_dose = FALSE,
  palette = "default",
  sd_min = FALSE,
  sd_max = FALSE,
  ci = FALSE,
  tooltip_vars = NULL,
  labels_df = NULL,
  filtering_list = NULL,
  use_time_since_last_dose = FALSE,
  x_limits = NULL,
  y_limits = NULL
) {

  mean_data <- process_data_mean(
    pknca_data = pknca_data,
    extra_grouping_vars = c(color_by, facet_by),
    facet_by = facet_by,
    filtering_list = filtering_list,
    ylog_scale = ylog_scale,
    show_dose = show_dose,
    use_time_since_last_dose = use_time_since_last_dose
  )

  # The time variable will always be the first one
  x_var <- names(mean_data)[1]

  # If no tooltip variable specified, use the default ones
  if (is.null(tooltip_vars)) {
    tooltip_vars <- unique(c(
      "AVAL",
      pknca_data$conc$columns$concu,
      x_var,
      pknca_data$conc$columns$timeu,
      color_by
    ))
  }
  # Override AVAL label so tooltip and y-axis show "Mean Analysis Value"
  conc_label <- get_label(
    pknca_data$conc$columns$concentration, labels_df = labels_df
  )
  if (!is.null(labels_df)) {
    labels_df <- rbind(labels_df, data.frame(
      Dataset = "ADNCA", Variable = "AVAL",
      Label = paste("Mean", conc_label),
      stringsAsFactors = FALSE
    ))
  }

  plot <- g_lineplot(
    data = mean_data %>% dplyr::rename(AVAL = Mean),
    x_var = x_var,
    y_var = "AVAL",
    x_unit = pknca_data$conc$columns$timeu,
    y_unit = pknca_data$conc$columns$concu,
    color_by = color_by,
    facet_by = facet_by,
    group_by = color_by,
    facet_count_n = if (isTRUE(show_facet_n)) "USUBJID_COUNT" else NULL,
    x_limits = x_limits,
    y_limits = y_limits,
    ylog_scale = ylog_scale,
    threshold_value = threshold_value,
    palette = palette,
    tooltip_vars = tooltip_vars,
    labels_df = labels_df,
    vline_var = if (show_dose) "TIME_DOSE" else NULL,
    show_legend = show_legend
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
    x_var = x_var,
    y_var = "AVAL"
  )
}

#' Process data for individual line plot
#'
#' Creates a filtered data frame for individual spaghetti plots
#'
#' @param pknca_data A PKNCAdata object containing concentration and dose data.
#' @param filtering_list Named list of filters (column = allowed values).
#' Default is `NULL` (no filtering).
#' @param ylog_scale Logical; if `TRUE`, removes non-positive concentrations.
#' Default is `FALSE`.
#' @param conc_col Name of the concentration column. Default is "AVAL".
#' @param show_dose Logical; if `TRUE`, derives dose times and includes TIME_DOSE column.
#' Default is `FALSE`.
#' @param use_time_since_last_dose Logical; if `TRUE`, x-axis represents time since last dose.
#' Default is `FALSE` (time since first dose).
#'
#' @return Data frame filtered and ready for individual spaghetti plots,
#' with optional TIME_DOSE column.
#' @keywords internal
#' @noRd
process_data_individual <- function(pknca_data,
                                    filtering_list = NULL,
                                    ylog_scale = FALSE,
                                    conc_col = "AVAL",
                                    show_dose = FALSE,
                                    use_time_since_last_dose = FALSE) {
  # Derive dose times if requested
  data <- if (show_dose || use_time_since_last_dose) {
    data <- derive_last_dose_time(
      pknca_data = pknca_data,
      conc_time_col = pknca_data$conc$columns$time
    )
    # Adjust time variable with dose time if using time since last dose
    if (use_time_since_last_dose) {
      data <- dplyr::mutate(
        data,
        !!pknca_data$conc$columns$time := !!sym(pknca_data$conc$columns$time) - TIME_DOSE
      )
    }
    data
  } else {
    pknca_data$conc$data
  }

  # Handle dose profile duplicates if filtering by ATPTREF so pre-dose samples
  # assigned only to one profile are retained
  if ("ATPTREF" %in% names(filtering_list)) {
    if ("ARRLT" %in% names(data) && any(data$ARRLT < 0 & data$AFRLT > 0)) {
      data <- dose_profile_duplicates(
        data,
        groups = c(group_vars(pknca_data$conc), "ATPTREF"),
        dosno = "ATPTREF"
      )
    }
  }

  processed_data <- data
  # Apply filtering
  if (!is.null(filtering_list) && length(filtering_list) > 0) {
    processed_data <- filter_by_list(processed_data, filtering_list)
  }
  processed_data <- processed_data %>%
    dplyr::filter(!is.na(!!sym(conc_col)))
  # Remove non-positive concentrations if log scale is selected (for posterior plotting)
  if (isTRUE(ylog_scale)) {
    processed_data <- processed_data %>% dplyr::filter(!!sym(conc_col) > 0)
  }
  processed_data
}

#' Process data for mean PK line plot
#'
#' Creates a summarised data frame for mean PK concentration-time profiles
#'
#' @param pknca_data A PKNCAdata object containing concentration and dose data.
#' @param extra_grouping_vars Character vector of extra grouping variables to include in summary.
#' Default is `NULL`.
#' @param facet_by Character vector of columns used for facet-specific counts.
#' @param filtering_list Named list of filters (column = allowed values).
#' Default is `NULL` (no filtering).
#' @param ylog_scale Logical; if `TRUE`, removes non-positive means. Default is `FALSE`.
#' @param show_dose Logical; if `TRUE`, derives dose times and includes TIME_DOSE column.
#' Default is `FALSE`.
#' @param use_time_since_last_dose Logical; if `TRUE`, x-axis represents time since last dose.
#' Default is `FALSE` (time since first dose).
#'
#' @return Data frame summarised by group, with columns for Mean, SD, N, SE, SD_min, SD_max,
#' CI_lower, CI_upper, and optional TIME_DOSE.
#' @keywords internal
#' @noRd
process_data_mean <- function(pknca_data,
                              extra_grouping_vars = NULL,
                              facet_by = NULL,
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
  grouping_cols <- unique(
    c(extra_grouping_vars, x_var, x_var_unit, y_var_unit, dose_group_cols)
  )
  grouping_cols <- if (show_dose) c(grouping_cols, "TIME_DOSE") else grouping_cols

  processed <- .prepare_mean_data(
    pknca_data, x_var, y_var, dose_group_cols,
    filtering_list, show_dose
  )

  # Calculate summary statistics by grouping columns
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
    ) %>%
    # Make sure the nominal time column is always the first column
    select(any_of(c(x_var)), everything())

  if (!is.null(facet_by) && length(facet_by) > 0) {
    subj_col <- pknca_data$conc$columns$subject
    facet_counts <- processed %>%
      dplyr::distinct(!!!rlang::syms(facet_by), !!rlang::sym(subj_col)) %>%
      dplyr::group_by(!!!rlang::syms(facet_by)) %>%
      dplyr::summarise(USUBJID_COUNT = dplyr::n_distinct(!!rlang::sym(subj_col)), .groups = "drop")

    summarised_data <- summarised_data %>%
      dplyr::left_join(facet_counts, by = facet_by)
  }

  # Remove non-positive means if log scale is selected (for posterior plotting)
  if (isTRUE(ylog_scale)) {
    summarised_data <- summarised_data %>%
      dplyr::filter(Mean > 0)
  }
  summarised_data
}

#' Prepare data for mean plot: derive dose times, filter, and adjust
#' @noRd
.prepare_mean_data <- function(pknca_data, x_var, y_var, dose_group_cols,
                               filtering_list, show_dose) {
  data <- if (show_dose) {
    derive_last_dose_time(pknca_data = pknca_data, conc_time_col = x_var)
  } else {
    pknca_data$conc$data
  }
  if (!is.null(filtering_list) && length(filtering_list) > 0) {
    data <- filter_by_list(data, filtering_list)
  }
  data <- data %>% dplyr::filter(!is.na(!!rlang::sym(y_var)))
  if (show_dose && !is.null(dose_group_cols) && !is.null(x_var)) {
    data <- data %>%
      dplyr::group_by(!!!rlang::syms(c(dose_group_cols, x_var))) %>%
      dplyr::mutate(TIME_DOSE = mean(TIME_DOSE, na.rm = TRUE)) %>%
      dplyr::ungroup()
  }
  data
}

#' Filter a data frame by a list of column-value pairs
#'
#' Filters a data frame by a named list of column-value pairs, using apply_filters for logic.
#'
#' @param data A data frame to filter.
#' @param filtering_list A named list where each name is a column and each value is a vector
#' of allowed values.
#'
#' @return Filtered data frame.
#' @keywords internal
#' @noRd
filter_by_list <- function(data, filtering_list) {
  if (is.null(filtering_list) || length(filtering_list) == 0) {
    return(data)
  }
  filters <- lapply(names(filtering_list), function(var) {
    list(column = var, condition = "==", value = filtering_list[[var]])
  })
  apply_filters(data, filters)
}

#' Finalize mean PK plot by adding label prefixes and mean layers
#' (SD error bars, confidence intervals)
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
      x = paste(plot_build$plot$labels$x),
      y = paste(plot_build$plot$labels$y),
      title = paste0("Mean ", plot_build$plot$labels$title)
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
#' Adds a TIME_DOSE column to the concentration data, representing the last dose time
#' for each sample.
#'
#' @param pknca_data A PKNCAdata object containing concentration and dose data.
#' @param conc_time_col Name of the time column in concentration data.
#' Default is pknca_data$conc$columns$time.
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
    by = dose_group_vars,
    relationship = "many-to-many"
  ) %>%
    dplyr::filter(TIME_DOSE <= !!rlang::sym(conc_time_col)) %>%
    dplyr::group_by(!!!rlang::syms(setdiff(names(conc_data), "TIME_DOSE"))) %>%
    dplyr::arrange(TIME_DOSE) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
}
