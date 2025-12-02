#' Process data for individual lineplot
#'
#' @param data Raw data frame.
#' @param selected_usubjids,selected_analytes,selected_pcspec Inputs for filters.
#' @param profiles_selected Optional profiles to filter on. If not null, uses ARRLT as time_col.
#' @param ylog_scale Logical, whether to use a logarithmic scale for the y-axis.
#' Defaults to metadata_nca_variables.
#' @returns A list with processed_data and time_col.
#'
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

  time_col <- if (!is.null(profiles_selected)) "ARRLT" else "AFRLT"

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

  return(list(
    processed_data = processed_data,
    time_col = time_col
    )
  )
}

#' Create a Mean PK Line Plot
#'
#' @param data Raw data frame.
#' @param selected_analytes,selected_pcspec,profiles_selected Inputs for filtering.
#' @param colorby_var The variable(s) to group and color by.
#' @param facet_by The variable(s) to facet by.
#' @param ylog_scale Logical, whether to use a logarithmic scale for the y-axis.
#' @param show_threshold Logical, whether to show threshold line.
#' @param threshold_value Numeric, value for the threshold line.
#' @param show_dose Logical, whether to show dosing indicators.
#' @param palette A named color palette.
#' @param dose_data The raw data frame (or a derivative) for calculating dose times.
#' @param show_sd_min,show_sd_max,show_ci Logicals for error bars.
#' @param labels_df A data.frame used for label lookups in tooltips.
#' Defaults to metadata_nca_variables.
#' @returns A `ggplot` object.
#'
create_meanplot <- function(data,
                            selected_analytes,
                            selected_pcspec,
                            profiles_selected = NULL,
                            colorby_var,
                            facet_by = NULL,
                            ylog_scale = FALSE,
                            show_threshold = FALSE,
                            threshold_value = 0,
                            show_dose = FALSE,
                            palette = NULL,
                            dose_data = NULL,
                            show_sd_min = FALSE,
                            show_sd_max = TRUE,
                            show_ci =  FALSE,
                            labels_df = metadata_nca_variables) {

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

  summarised_data <- processed %>%
    group_by(!!!syms(colorby_var), !!sym(time_col), !!!syms(facet_by), RRLTU, AVALU) %>%
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

  validate(need(nrow(summarised_data) > 0, "No data with >= 3 points to calculate mean."))

  tt_vars <- unique(c("Mean", time_col, colorby_var))

  meanplot <- g_lineplot(
    data = summarised_data,
    x_var = time_col,
    y_var = "Mean",
    colorby_var = colorby_var,
    ylog_scale = ylog_scale,
    show_sd_min = show_sd_min,
    show_sd_max = show_sd_max,
    show_ci = show_ci,
    facet_by = facet_by,
    show_threshold = show_threshold,
    threshold_value = threshold_value,
    show_dose = show_dose,
    dose_data = data %>% mutate(TIME_DOSE = round(NFRLT - NRRLT, 6)),
    palette = palette,
    tooltip_vars = tt_vars,
    labels_df = labels_df
  )

  return(meanplot)
}
