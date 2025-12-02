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
#' @param ylog_scale Logical, whether to use a logarithmic scale for the y-axis.
#' @param color_by,facet_by Optional grouping variables.
#' @returns A list with summarised_data and time_col.
#'
process_data_mean <- function(data,
                            selected_analytes,
                            selected_pcspec,
                            profiles_selected = NULL,
                            ylog_scale = FALSE,
                            color_by = NULL,
                            facet_by = NULL){

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
    group_by(!!!syms(color_by), !!sym(time_col), !!!syms(facet_by), RRLTU, AVALU) %>%
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


  return(list(
    summarised_data = summarised_data,
    time_col = time_col
  )
  )
}
