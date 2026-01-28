#' Finalize mean PK plot with labels and mean layers
#'
#' @param plot The ggplot object to finalize.
#' @param sd_min Logical; if TRUE, plot lower SD error bars.
#' @param sd_max Logical; if TRUE, plot upper SD error bars.
#' @param ci Logical; if TRUE, plot 95% confidence interval ribbon.
#' @param color_by Character vector for color grouping.
#' @param y_var Name of the y variable.
#' @param x_var Name of the x variable.
#' @return Finalized ggplot object for mean PK plot.
#' @keywords internal
#' @noRd
finalize_meanplot <- function(plot, sd_min, sd_max, ci, color_by, y_var, x_var) {
  plot_build <- ggplot2::ggplot_build(plot)
  plot +
    labs(
      x = paste("Nominal", plot_build$labels$x),
      y = paste("Mean", plot_build$labels$y),
      title = paste("Mean", plot_build$labels$title)
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
#' @param pknca_data A PKNCAdata object containing concentration and dose data.
#' @param conc_time_col Name of the time column in concentration data (default: pknca_data$conc$columns$time).
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

#' Filter a data frame by a list of column-value pairs
#'
#' @param data A data frame to filter.
#' @param filtering_list A named list where each name is a column and each value is a vector of allowed values.
#' @return Filtered data frame.
#' @keywords internal
#' @noRd
filter_by_list <- function(data, filtering_list) {
  if (is.null(filtering_list) || length(filtering_list) == 0) return(data)
  purrr::reduce(
    names(filtering_list),
    function(df, var) {
      df %>% dplyr::filter(!!rlang::sym(var) %in% filtering_list[[var]])
    },
    .init = data
  )
}

#' Process data for individual line plot
#'
#' Creates a filtered data frame for individual spaghetti plots.
#'
#' @param data Raw data frame.
#' @param filtering_list Named list of filters (column = allowed values).
#' @param ylog_scale Logical, whether to use a logarithmic scale for the y-axis.
#' @param conc_col Name of the concentration column (default: "AVAL").
#' @returns `processed_data` filtered for the spaghetti plots.
#'
#' @import dplyr
#' @importFrom rlang sym syms
#' @seealso dose_profile_duplicates
#' @examples
#' base_df <- expand.grid(
#'   USUBJID = c("Subject1", "Subject2", "Subject3", "Subject4"),
#'   PARAM = c("Analyte1"),
#'   PCSPEC = c("Spec1"),
#'   ATPTREF = 1,
#'   AFRLT = 0:5
#' )
#' set.seed(123)
#' base_df$AVAL <- rnorm(nrow(base_df), mean = 50, sd = 10)
#'
#' result <- process_data_individual(
#'   data = base_df,
#'   filtering_list = list(
#'     USUBJID = c("Subject1", "Subject2"),
#'     PARAM = c("Analyte1"),
#'     PCSPEC = c("Spec1")
#'   ),
#'   ylog_scale = FALSE
#' )
#' @keywords internal
#' @noRd
process_data_individual <- function(data,
                                    filtering_list = NULL,
                                    ylog_scale = FALSE,
                                    conc_col = "AVAL") {
  processed_data <- filter_by_list(data, filtering_list) %>%
    dplyr::filter(!is.na(!!rlang::sym(conc_col)))
  if (isTRUE(ylog_scale)) {
    processed_data <- processed_data %>% dplyr::filter(!!rlang::sym(conc_col) > 0)
  }
  processed_data
}

#' Process data for mean PK line plot
#'
#' Creates a summarised data frame for mean PK concentration-time profiles, with optional alignment to last dose and dose time aggregation.
#'
#' @param data Raw concentration data frame (before dose alignment).
#' @param filtering_list Named list of filters (column = allowed values).
#' @param ylog_scale Logical, whether to use a logarithmic scale for the y-axis.
#' @param color_by,facet_by Optional grouping variables to be included in summary.
#' @param conc_col Name of the concentration column (default: "AVAL").
#' @param grouping_cols Character vector of columns to group by (default: c(color_by, facet_by, "RRLTU", "AVALU")).
#' @param align_last_dose Logical; if TRUE, align concentration data to last dose using `derive_last_dose_time` (requires `pknca_data` and `conc_time_col`).
#' @param pknca_data Optional PKNCAdata object for dose alignment (required if align_last_dose is TRUE).
#' @param conc_time_col Name of the time column to align on (required if align_last_dose is TRUE).
#' @param aggregate_dose_time Logical; if TRUE, aggregate dose times by group (requires `dose_group_cols` and `time_sample`).
#' @param dose_group_cols Character vector of columns for dose grouping (used if aggregate_dose_time is TRUE).
#' @param time_sample Name of the time variable for dose aggregation (used if aggregate_dose_time is TRUE).
#'
#' @return Data frame summarised by group, with columns for Mean, SD, N, SE, SD_min, SD_max, CI_lower, CI_upper.
#'
#' @import dplyr
#' @importFrom rlang sym syms
#' @keywords internal
#' @noRd
process_data_mean <- function(data,
                              filtering_list = NULL,
                              ylog_scale = FALSE,
                              color_by = NULL,
                              facet_by = NULL,
                              conc_col = "AVAL",
                              grouping_cols = c(color_by, facet_by, "RRLTU", "AVALU"),
                              aggregate_dose_time = FALSE,
                              dose_group_cols = NULL,
                              time_sample = NULL) {

  processed <- filter_by_list(data, filtering_list) %>%
    dplyr::filter(!is.na(!!rlang::sym(conc_col)))

  if (aggregate_dose_time && !is.null(dose_group_cols) && !is.null(time_sample)) {
    processed <- processed %>%
      dplyr::group_by(!!!rlang::syms(dose_group_cols), !!rlang::sym(time_sample)) %>%
      dplyr::mutate(TIME_DOSE = mean(TIME_DOSE, na.rm = TRUE)) %>%
      dplyr::ungroup()
  }

  summarised_data <- processed %>%
    dplyr::group_by(!!!rlang::syms(grouping_cols)) %>%
    dplyr::summarise(
      Mean = round(mean(!!rlang::sym(conc_col), na.rm = TRUE), 3),
      SD = sd(!!rlang::sym(conc_col), na.rm = TRUE),
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

#' Create an Individual PK Line Plot
#'
#' This function generates a line plot for individual pharmacokinetic (PK) concentration-time profiles (spaghetti plots).
#' It supports filtering by subject, analyte, specimen, and profile, and allows customization of axes, color, faceting, and tooltips.
#'
#' @param pknca_data A PKNCAdata object containing the raw PK concentration data.
#' @param color_by Character vector specifying the column(s) used to color the lines and points.
#' @param facet_by Character vector of column names to facet the plot by. Default is `NULL` (no faceting).
#' @param ylog_scale Logical; whether to use a logarithmic scale for the y-axis. Default is `FALSE`.
#' @param threshold_value Numeric; y-intercept for a horizontal threshold line. Default is `NULL` (no threshold).
#' @param palette Optional named character vector of colors for the plot. Names should match levels of the color variable. Default is `NULL`.
#' @param tooltip_vars Character vector of column names to include in the tooltip. Default is `NULL`.
#' @param labels_df Optional data.frame for variable label lookups (for tooltips). Default is `NULL`.
#' @param filtering_list Named list of filters (column = allowed values). Default is `NULL` (no filtering).
#' @param use_time_since_last_dose Logical; if `TRUE`, x-axis represents time since last dose. Default is `FALSE`, which uses time since first dose.
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
    palette = NULL,
    tooltip_vars = NULL,
    labels_df = NULL,
    filtering_list = NULL,
    use_time_since_last_dose = FALSE
) {

  data <- if (show_dose || use_time_since_last_dose) {
    data <- derive_last_dose_time(
      pknca_data = pknca_data,
      conc_time_col = pknca_data$conc$columns$time
    )
    if (use_time_since_last_dose) {
      data <- dplyr::mutate(data, !!pknca_data$conc$columns$time := !!rlang::sym(pknca_data$conc$columns$time) - TIME_DOSE)
    }
    data
  } else {
    pknca_data$conc$data
  }

  individual_data <- process_data_individual(
    data = data,
    filtering_list = filtering_list,
    ylog_scale = ylog_scale
  )

  x_var_unit <- pknca_data$conc$columns$timeu
  y_var_unit <- pknca_data$conc$columns$concu

  g_lineplot(
    data = individual_data,
    x_var = pknca_data$conc$columns$time,
    y_var = pknca_data$conc$columns$concentration,
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
#' This function generates a line plot for mean pharmacokinetic (PK) concentration-time profiles.
#' It supports error bars (SD), confidence intervals, faceting, and custom tooltips, and allows filtering by analyte, specimen, and profile.
#'
#' @inheritParams exploration_individualplot
#' @param sd_min Logical; if `TRUE`, plot lower SD error bars. Default is `FALSE`.
#' @param sd_max Logical; if `TRUE`, plot upper SD error bars. Default is `FALSE`.
#' @param ci Logical; if `TRUE`, plot 95% confidence interval ribbon. Default is `FALSE`.
#' @return A `ggplot` object representing the mean PK line plot.
#' @export
exploration_meanplot <- function(
    pknca_data,
    color_by,
    facet_by = NULL,
    ylog_scale = FALSE,
    threshold_value = NULL,
    show_dose = FALSE,
    palette = NULL,
    sd_min = FALSE,
    sd_max = FALSE,
    ci = FALSE,
    tooltip_vars = c(group_vars(pknca_data$dose), time_sample, "Mean"),
    labels_df = NULL,
    filtering_list = NULL,
    use_time_since_last_dose = FALSE
) {

  x_var_unit <- pknca_data$conc$columns$timeu
  y_var_unit <- pknca_data$conc$columns$concu
  time_sample <- pknca_data$conc$columns$time.nominal
  time_sample <- if (is.null(time_sample)) pknca_data$conc$columns$time else time_sample
  time_sample <- if (use_time_since_last_dose) "NRRLT" else time_sample
  dose_group_cols <- setdiff(group_vars(pknca_data$dose), pknca_data$conc$columns$subject)
  x_var <- time_sample
  y_var <- "Mean"
  grouping_cols <- unique(c(color_by, facet_by, time_sample, x_var_unit, y_var_unit, dose_group_cols))
  grouping_cols <- if (show_dose) c(grouping_cols, "TIME_DOSE") else grouping_cols

  data <- if (show_dose) {
    derive_last_dose_time(
      pknca_data = pknca_data,
      conc_time_col = time_sample
    )
  } else {
    pknca_data$conc$data
  }

  mean_data <- process_data_mean(
    data = data,
    filtering_list = filtering_list,
    grouping_cols = grouping_cols,
    ylog_scale = ylog_scale,
    aggregate_dose_time = show_dose,
    dose_group_cols = dose_group_cols,
    time_sample = time_sample
  )

  plot <- g_lineplot(
    data = mean_data,
    x_var = x_var,
    y_var = y_var,
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

  # If there is no mean data, return the error plot
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
    y_var = y_var,
    x_var = x_var
  )
}
