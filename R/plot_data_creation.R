#' Process data for individual line plot
#'
#' Creates a filtered data frame for individual spaghetti plots.
#'
#' @param data Raw data frame.
#' @param selected_usubjids,selected_analytes,selected_pcspec Inputs for filters.
#' @param profiles_selected Optional profiles to filter on.
#' @param ylog_scale Logical, whether to use a logarithmic scale for the y-axis.
#' @returns `processed_data` filtered for the spaghetti plots.
#'
#' @import dplyr
#' @importFrom rlang sym syms
#' @seealso dose_profile_duplicates
#' @examples
#' base_df <- expand.grid(
#' USUBJID = c("Subject1", "Subject2", "Subject3", "Subject4"),
#' PARAM = c("Analyte1"),
#' PCSPEC = c("Spec1"),
#' ATPTREF = 1,
#' AFRLT = 0:5
#' )
#' set.seed(123)
#' base_df$AVAL <- rnorm(nrow(base_df), mean = 50, sd = 10)
#'
#' result <- process_data_individual(
#' data = base_df,
#' selected_usubjids = c("Subject1", "Subject2"),
#' selected_analytes = c("Analyte1"),
#' selected_pcspec = c("Spec1"),
#' profiles_selected = NULL,
#' ylog_scale = FALSE
#' )
#' @export
process_data_individual <- function(data,
                                    selected_usubjids,
                                    selected_analytes,
                                    selected_pcspec,
                                    profiles_selected = NULL,
                                    ylog_scale = FALSE) {

  processed_data <- data %>%
    filter(
      USUBJID %in% selected_usubjids,
      PARAM %in% selected_analytes,
      PCSPEC %in% selected_pcspec,
      !is.na(AVAL)
    )

  if (isTRUE(ylog_scale)) {
    processed_data <- processed_data %>% filter(AVAL > 0)
  }

  if (!is.null(profiles_selected)) {
    if ("ARRLT" %in% names(processed_data) &&
          any(processed_data$ARRLT < 0 & processed_data$AFRLT > 0)) {
      processed_data <- dose_profile_duplicates(
        processed_data,
        groups = c("USUBJID", "PCSPEC", "PARAM", "ATPTREF"),
        dosno = "ATPTREF"
      )
    }
    processed_data <- processed_data %>% filter(ATPTREF %in% profiles_selected)
  }

  processed_data
}

#' Create a Mean PK Line Plot
#'
#' @param data Raw data frame.
#' @param selected_analytes,selected_pcspec,profiles_selected Inputs for filtering.
#' @param ylog_scale Logical, whether to use a logarithmic scale for the y-axis.
#' @param color_by,facet_by Optional grouping variables to be included in summary.
#' @returns `summarised_data` with Mean, SD, and CIs for the profiles selected.
#' @import dplyr
#' @importFrom rlang sym syms
#'
#' @examples
#' base_df <- expand.grid(
#' USUBJID = c("Subject1", "Subject2", "Subject3", "Subject4"),
#' PARAM = c("Analyte1"),
#' PCSPEC = c("Spec1"),
#' ATPTREF = 1,
#' NFRLT = 0:5,
#' AVALU = "ug/ml",
#' RRLTU = "hr"
#' )
#' set.seed(123)
#' base_df$AVAL <- rnorm(nrow(base_df), mean = 50, sd = 10)
#'
#' result <- process_data_mean(
#' data = base_df,
#' selected_analytes = c("Analyte1"),
#' selected_pcspec = c("Spec1"),
#' profiles_selected = NULL,
#' ylog_scale = FALSE
#' )
#' @export
process_data_mean <- function(data,
                              selected_analytes,
                              selected_pcspec,
                              profiles_selected = NULL,
                              ylog_scale = FALSE,
                              color_by = NULL,
                              facet_by = NULL) {

  processed <- data %>%
    filter(
      PARAM %in% selected_analytes,
      PCSPEC %in% selected_pcspec,
      !is.na(AVAL)
    )

  if (!is.null(profiles_selected)) {
    processed <- processed %>% filter(ATPTREF %in% profiles_selected)
  }

  time_col <- if (!is.null(profiles_selected)) "NRRLT" else "NFRLT"
  # Retrieve unique grouping variables
  grp_cols <- unique(c(color_by, time_col, facet_by, "RRLTU", "AVALU"))

  summarised_data <- processed %>%
    group_by(!!!syms(grp_cols)) %>%
    summarise(
      Mean = round(mean(AVAL, na.rm = TRUE), 3),
      SD = sd(AVAL, na.rm = TRUE),
      N = n(),
      SE = SD / sqrt(N),
      .groups = "drop"
    ) %>%
    filter(N >= 3) %>%
    mutate(
      SD_min = Mean - SD,
      SD_max = Mean + SD,
      CI_lower = Mean - 1.96 * SE,
      CI_upper = Mean + 1.96 * SE
    )

  if (isTRUE(ylog_scale)) {
    summarised_data <- summarised_data %>%
      filter(Mean > 0)
  }
  summarised_data
}




#' Create an Individual PK Line Plot
#'
#' This function generates a line plot for individual pharmacokinetic (PK) concentration-time profiles (spaghetti plots).
#' It supports filtering by subject, analyte, specimen, and profile, and allows customization of axes, color, faceting, and tooltips.
#'
#' @param pknca_data A PKNCAdata object containing the raw PK concentration data.
#' @param x_var Character string specifying the column name for the x-axis.
#' @param y_var Character string specifying the column name for the y-axis (typically "AVAL").
#' @param color_by Character vector specifying the column(s) used to color the lines and points.
#' @param facet_by Character vector of column names to facet the plot by. Default is `NULL` (no faceting).
#' @param ylog_scale Logical; whether to use a logarithmic scale for the y-axis. Default is `FALSE`.
#' @param threshold_value Numeric; y-intercept for a horizontal threshold line. Default is `NULL` (no threshold).
#' @param dose_data Optional data.frame with dosing information for vertical dose lines. Must include `TIME_DOSE` and any faceting variables. Default is `NULL`.
#' @param palette Optional named character vector of colors for the plot. Names should match levels of the color variable. Default is `NULL`.
#' @param tooltip_vars Character vector of column names to include in the tooltip. Default is `NULL`.
#' @param labels_df Optional data.frame for variable label lookups (for tooltips). Default is `NULL`.
#' @param selected_analytes Character vector of analyte names to filter. Default is `NULL` (no filter).
#' @param selected_pcspec Character vector of specimen names to filter. Default is `NULL` (no filter).
#' @param profiles_selected Optional vector of profile IDs to filter. Default is `NULL` (no filter).
#' @param selected_usubjids Character vector of subject IDs to filter. Default is `NULL` (no filter).
#'
#' @return A `ggplot` object representing the individual PK line plot.
#' @seealso [g_lineplot()], [process_data_individual()]
#' @export
exploration_individualplot <- function(
    pknca_data,
    # x_var,
    # y_var,
    color_by,
    facet_by = NULL,
    ylog_scale = FALSE,
    threshold_value = NULL,
    show_dose = FALSE,
    palette = NULL,
    tooltip_vars = NULL,
    labels_df = NULL,
    selected_analytes = NULL,
    selected_pcspec = NULL,
    profiles_selected = NULL,
    selected_usubjids = NULL
) {

  data <- if (show_dose) {
    time_dose <- pknca_data$dose$columns$time
    dose_group_vars <- group_vars(pknca_data$dose)

    left_join(
      pknca_data$conc$data,
      pknca_data$dose$data %>%
        mutate(TIME_DOSE = !!sym(time_dose)) %>%
        select(!!!syms(c(dose_group_vars, "TIME_DOSE"))),
      by = dose_group_vars
    ) %>%
      filter(TIME_DOSE <= !!sym(pknca_data$conc$columns$time)) %>%
      # For each sample time, get the most recent TIME_DOSE
      group_by(!!!syms(setdiff(names(pknca_data$conc$data), "TIME_DOSE"))) %>%
      arrange(TIME_DOSE) %>%
      slice_tail(n = 1)
  } else {
    pknca_data$conc$data
  }

  individual_data <- process_data_individual(
    data = data,
    selected_usubjids = selected_usubjids,
    selected_analytes = selected_analytes,
    selected_pcspec = selected_pcspec,
    profiles_selected = profiles_selected,
    ylog_scale = ylog_scale
  )

  x_var <- pknca_data$conc$columns$time # or ARRLT for 1 profile, which does not make sense...
  y_var <- pknca_data$conc$columns$concentration
  x_var_unit <- pknca_data$conc$columns$timeu
  y_var_unit <- pknca_data$conc$columns$concu
  group_by <- pknca_data$conc$columns$subject

  g_lineplot(
    data = individual_data,
    x_var = x_var,
    y_var = y_var,
    color_by = color_by,
    facet_by = facet_by,
    group_by = group_by,
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
#' @param pknca_data A PKNCAdata object containing the raw PK concentration data.
#' @param x_var Character string specifying the column name for the x-axis.
#' @param y_var Character string specifying the column name for the y-axis (typically "Mean").
#' @param color_by Character vector specifying the column(s) used to color the lines and points.
#' @param facet_by Character vector of column names to facet the plot by. Default is `NULL` (no faceting).
#' @param ylog_scale Logical; whether to use a logarithmic scale for the y-axis. Default is `FALSE`.
#' @param threshold_value Numeric; y-intercept for a horizontal threshold line. Default is `NULL` (no threshold).
#' @param dose_data Optional data.frame with dosing information for vertical dose lines. Must include `TIME_DOSE` and any faceting variables. Default is `NULL`.
#' @param palette Optional named character vector of colors for the plot. Names should match levels of the color variable. Default is `NULL`.
#' @param sd_min Logical; if `TRUE`, plot lower SD error bars. Default is `FALSE`.
#' @param sd_max Logical; if `TRUE`, plot upper SD error bars. Default is `FALSE`.
#' @param ci Logical; if `TRUE`, plot 95% confidence interval ribbon. Default is `FALSE`.
#' @param tooltip_vars Character vector of column names to include in the tooltip. Default is `NULL`.
#' @param labels_df Optional data.frame for variable label lookups (for tooltips). Default is `NULL`.
#' @param selected_analytes Character vector of analyte names to filter. Default is `NULL` (no filter).
#' @param selected_pcspec Character vector of specimen names to filter. Default is `NULL` (no filter).
#' @param profiles_selected Optional vector of profile IDs to filter. Default is `NULL` (no filter).
#'
#' @return A `ggplot` object representing the mean PK line plot.
#' @seealso [g_lineplot()], [process_data_mean()]
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
    tooltip_vars = NULL,
    labels_df = NULL,
    selected_analytes = NULL,
    selected_pcspec = NULL,
    profiles_selected = NULL
) {

  time_sample <- pknca_data$conc$columns$time
  data <- if (show_dose) {
    time_dose <- pknca_data$dose$columns$time
    dose_group_vars <- group_vars(pknca_data$dose)

    left_join(
      pknca_data$conc$data,
      pknca_data$dose$data %>%
        mutate(TIME_DOSE = !!sym(time_dose)) %>%
        select(!!!syms(c(dose_group_vars, "TIME_DOSE"))),
      by = dose_group_vars
    ) %>%
      filter(TIME_DOSE <= !!sym(time_sample)) %>%
      # For each sample time, get the most recent TIME_DOSE
      group_by(!!!syms(setdiff(names(pknca_data$conc$data), "TIME_DOSE"))) %>%
      arrange(TIME_DOSE) %>%
      slice_tail(n = 1)
  } else {
    pknca_data$conc$data
  }

  mean_data <- process_data_mean(
    data = data,
    selected_analytes = selected_analytes,
    selected_pcspec = selected_pcspec,
    profiles_selected = profiles_selected,
    color_by = color_by,
    facet_by = c(facet_by, "TIME_DOSE"),
    ylog_scale = ylog_scale
  )

  time_nominal_col <- pknca_data$conc$columns$time.nominal
  x_var <- if (!is.null(time_nominal_col)) time_nominal_col else pknca_data$conc$columns$time
  y_var <- "Mean"

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

  plot_build <- ggplot_build(plot)
  has_error_msg <- isTRUE(plot_build[[1]][[1]]$label == "No data available for the plot")
  if (has_error_msg) {
    return(plot)
  }
  plot <- plot +
    labs(
      x = paste("Nominal", plot_build$labels$x),
      y = paste("Mean", plot_build$labels$y),
      title = paste("Mean", plot_build$labels$title)
    )

  plot + list(
    .add_mean_layers(
      is_mean_plot = TRUE,
      sd_min = sd_min,
      sd_max = sd_max,
      ci = ci,
      color_by = color_by,
      y_var = y_var,
      x_var = x_var,
      group_var = "color_var")
  )
}

# sample_data
# exploration_meanplot(
#   data = sample_data,
#   x_var = "NFRLT",
#   y_var = "Mean",
#   color_by = "DOSEA",
#   sd_min = TRUE,
#   sd_max = TRUE,
#   ci = TRUE,
#   selected_analytes = "Analyte1",
#   selected_pcspec = "Spec1",
#   profiles_selected = NULL,
#   ylog_scale = FALSE,
#   threshold_value = NULL,
#   dose_data = NULL,
#   palette = NULL,
#   tooltip_vars = NULL,
#   labels_df = NULL
# )
# exploration_individualplot(
#   data = sample_data,
#   x_var = "NFRLT",
#   y_var = "AVAL",
#   color_by = "DOSEA",
#   selected_analytes = "Analyte1",
#   selected_pcspec = "Spec1",
#   profiles_selected = 1,
#   ylog_scale = FALSE,
#   threshold_value = NULL,
#   dose_data = NULL,
#   palette = NULL,
#   tooltip_vars = NULL,
#   labels_df = NULL,
#   selected_usubjids = "Subject1"
# )