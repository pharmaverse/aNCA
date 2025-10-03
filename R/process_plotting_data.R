#' Process Data for Individual PK Line Plots
#'
#' Filters and prepares raw data for creating individual concentration-time plots.
#'
#' @param data Raw data frame.
#' @param selected_usubjids,selected_analytes,selected_pcspec,cycle Inputs for filtering.
#' @param colorby_var The variable(s) to be used for coloring.
#' @param time_scale String, either "All Time" or "By Dose Profile".
#' @param yaxis_scale String, either "log" or "lin".
#' @returns A processed and filtered data.frame ready for plotting.
#' @export
process_data_individual <- function(data, selected_usubjids, selected_analytes, selected_pcspec,
                                    colorby_var, time_scale, yaxis_scale, cycle) {
  processed <- data %>%
    filter(
      USUBJID %in% selected_usubjids,
      PARAM %in% selected_analytes,
      PCSPEC %in% selected_pcspec,
      if ("EVID" %in% names(data)) EVID == 0 else TRUE,
      !is.na(AVAL)
    )
  
  if (yaxis_scale == "log") {
    processed <- processed %>% filter(AVAL > 0)
  }
  
  if (time_scale == "By Dose Profile") {
    browser()
    if ("ARRLT" %in% names(processed) && any(processed$ARRLT < 0 & processed$AFRLT > 0)) {
      processed <- dose_profile_duplicates(
        processed,
        groups = c("USUBJID", "PCSPEC", "PARAM", "NCA_PROFILE"),
        dosno = "NCA_PROFILE"
      )
    }
    processed <- processed %>% filter(NCA_PROFILE %in% cycle)
  }
  
  processed %>%
    mutate(
      time_var = if (time_scale == "By Dose Profile") ARRLT else AFRLT,
      color_var = interaction(!!!syms(colorby_var), sep = ", ")
    )
}


#' Process Data for Mean PK Line Plots
#'
#' Filters and summarizes raw data for creating mean concentration-time plots.
#'
#' @param data Raw data frame.
#' @param selected_analytes,selected_pcspec,cycle Inputs for filtering.
#' @param colorby_var The variable(s) to group and color by.
#' @param yaxis_scale String, either "log" or "lin".
#' @param time_scale String, either "All Time" or "By Dose Profile"
#' @returns A summarized data.frame with mean, SD, and CI values.
#' @export
process_data_mean <- function(data, selected_analytes, selected_pcspec, cycle,
                              colorby_var, yaxis_scale, time_scale) {
  # Pre-filter the data
  processed <- data %>%
    filter(
      PARAM %in% selected_analytes,
      PCSPEC %in% selected_pcspec,
      if ("EVID" %in% names(data)) EVID == 0 else TRUE,
      NRRLT > 0
    )
  
  # Conditionally filter by cycle for "By Dose Profile" timescale
  if (time_scale == "By Dose Profile") {
    if ("ARRLT" %in% names(processed) && any(processed$ARRLT < 0 & processed$AFRLT > 0)) {
      processed <- dose_profile_duplicates(
        processed,
        groups = c("USUBJID", "PCSPEC", "PARAM", "NCA_PROFILE"),
        dosno = "NCA_PROFILE"
      )
    }
    processed <- processed %>% filter(NCA_PROFILE %in% cycle)
  }
  
  summarised_data <- processed %>%
    mutate(
      time_var = if (time_scale == "By Dose Profile") ARRLT else AFRLT,
      color_var = interaction(!!!syms(colorby_var), sep = ", ", drop = TRUE)) %>%
    group_by(color_var, time_var) %>%
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
      filter(Mean > 0) %>%
      mutate(
        log10_Mean = log10(Mean),
        log10_SD = SD / (Mean * log(10)),
        SD_min = 10^(log10_Mean - log10_SD),
        SD_max = 10^(log10_Mean + log10_SD),
        log10_SE = SE / (Mean * log(10)),
        log10_CI = 1.96 * log10_SE,
        CI_lower = 10^(log10_Mean - log10_CI),
        CI_upper = 10^(log10_Mean + log10_CI)
      )
  }
  
  return(summarised_data)
}
