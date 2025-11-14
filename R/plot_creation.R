#' Create an Individual PK Line Plot
#'
#' Combines data processing and plotting for individual concentration-time profiles.
#'
#' @param data Raw data frame.
#' @param selected_usubjids,selected_analytes,selected_pcspec,profiles_selected Inputs for filters.
#' @param colorby_var The variable(s) to be used for coloring.
#' @param ylog_scale Logical, whether to use a logarithmic scale for the y-axis.
#' @param facet_by Character vector of column names to facet by.
#' @param show_threshold Logical, whether to show threshold line.
#' @param threshold_value Numeric, value for the threshold line.
#' @param show_dose Logical, whether to show dosing indicators.
#' @param dose_data The raw data frame (or a derivative) for calculating dose times.
#' @param palette A named color palette.
#' @returns A `ggplot` object.
#'
create_indplot <- function(data,
                           selected_usubjids,
                           selected_analytes,
                           selected_pcspec,
                           colorby_var = "USUBJID",
                           ylog_scale = FALSE,
                           profiles_selected = NULL,
                           facet_by = NULL,
                           show_threshold = FALSE,
                           threshold_value = 0,
                           show_dose = FALSE,
                           dose_data = NULL,
                           palette = NULL) {

  processed_data <- data %>%
    filter(
      USUBJID %in% selected_usubjids,
      PARAM %in% selected_analytes,
      PCSPEC %in% selected_pcspec,
      !is.na(AVAL)
    )

  if (ylog_scale == TRUE) {
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

  processed_data <- processed_data %>%
    mutate(
      time_var = if (!is.null(profiles_selected)) ARRLT else AFRLT,
      color_var = interaction(!!!syms(colorby_var), sep = ", ")
    )

  validate(need(nrow(processed_data) > 0, "No data available for the selected filters."))

  lineplot <- g_lineplot(
    data = processed_data,
    x_var = "time_var",
    y_var = "AVAL",
    group_var = "USUBJID",
    colorby_var = colorby_var,
    facet_by = facet_by,
    ylog_scale = ylog_scale,
    show_threshold = show_threshold,
    threshold_value = threshold_value,
    show_dose = show_dose,
    dose_data = data %>% mutate(TIME_DOSE = round(AFRLT - ARRLT, 6)),
    palette = palette
  )

  return(lineplot)
}

#' Create a Mean PK Line Plot
#'
#' Combines data processing and plotting for mean concentration-time profiles.
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
                            show_ci =  FALSE) {

  processed <- data %>%
    filter(
      PARAM %in% selected_analytes,
      PCSPEC %in% selected_pcspec,
      !is.na(AVAL)
    )

  if (!is.null(profiles_selected)) {
    processed <- processed %>% filter(ATPTREF %in% profiles_selected)
  }

  summarised_data <- processed %>%
    mutate(
      time_var = if (!is.null(profiles_selected)) NRRLT else NFRLT,
      color_var = interaction(!!!syms(colorby_var), sep = ", ", drop = TRUE)
    ) %>%
    group_by(color_var, time_var, !!!syms(facet_by), RRLTU, AVALU) %>%
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

  if (ylog_scale == TRUE) {
    summarised_data <- summarised_data %>%
      filter(Mean > 0)
  }

  validate(need(nrow(summarised_data) > 0, "No data with >= 3 points to calculate mean."))

  meanplot <- g_lineplot(
    data = summarised_data,
    x_var = "time_var",
    y_var = "Mean",
    group_var = "color_var",
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
    palette = palette
  )

  return(meanplot)
}
