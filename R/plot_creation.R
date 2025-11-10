#' Create an Individual PK Line Plot
#'
#' Combines data processing and plotting for individual concentration-time profiles.
#'
#' @param data Raw data frame.
#' @param selected_usubjids,selected_analytes,selected_pcspec,cycle Inputs for filtering.
#' @param colorby_var The variable(s) to be used for coloring.
#' @param time_scale String, either "All Time" or "By Dose Profile".
#' @param yaxis_scale String, either "log" or "lin".
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
                           time_scale = "All Time",
                           cycle = NULL,
                           facet_by = NULL,
                           yaxis_scale = "Lin",
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
      if ("EVID" %in% names(data)) EVID == 0 else TRUE,
      !is.na(AVAL)
    )
  
  if (yaxis_scale == "log") {
    processed_data <- processed_data %>% filter(AVAL > 0)
  }
  
  if (time_scale == "By Dose Profile") {
    if ("ARRLT" %in% names(processed_data) && any(processed_data$ARRLT < 0 & processed_data$AFRLT > 0)) {
      processed_data <- dose_profile_duplicates(
        processed_data,
        groups = c("USUBJID", "PCSPEC", "PARAM", "ATPTREF"),
        dosno = "ATPTREF"
      )
    }
    processed_data <- processed_data %>% filter(ATPTREF %in% cycle)
  }
  
  processed_data <- processed_data %>%
    mutate(
      time_var = if (time_scale == "By Dose Profile") ARRLT else AFRLT,
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
    yaxis_scale = yaxis_scale,
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
#' @param selected_analytes,selected_pcspec,cycle Inputs for filtering.
#' @param colorby_var The variable(s) to group and color by.
#' @param facet_by The variable(s) to facet by.
#' @param yaxis_scale String, either "log" or "lin".
#' @param time_scale String, either "All Time" or "By Dose Profile".
#' @param show_sd_min,show_sd_max,show_ci Logicals for error bars.
#' @param show_threshold Logical, whether to show threshold line.
#' @param threshold_value Numeric, value for the threshold line.
#' @param show_dose Logical, whether to show dosing indicators.
#' @param dose_data The raw data frame (or a derivative) for calculating dose times.
#' @param palette A named color palette.
#' @returns A `ggplot` object.
#'
create_meanplot <- function(data,
                            selected_analytes,
                            selected_pcspec,
                            colorby_var,
                            time_scale = "All Time",
                            cycle = NULL,
                            facet_by = NULL,
                            yaxis_scale = "Lin",
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
      if ("EVID" %in% names(data)) EVID == 0 else TRUE,
      !is.na(AVAL)
    )
  
  if (time_scale == "By Dose Profile") {
    processed <- processed %>% filter(ATPTREF %in% cycle)
  }
  
  summarised_data <- processed %>%
    mutate(
      time_var = if (time_scale == "By Dose Profile") NRRLT else NFRLT,
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
  
  if (yaxis_scale == "log") {
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
    yaxis_scale = yaxis_scale,
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