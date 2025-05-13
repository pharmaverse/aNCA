#' Generate a General Line Plot for ADNCA Dataset
#'
#' This function generates a line plot for an ADNCA dataset based on user-selected analytes,
#' subjects, and other parameters. The plot can be customized to display data on a linear or
#' logarithmic scale and can be filtered by cycle.
#'
#' @param data              A data frame containing the ADNCA dataset.
#' @param selected_analytes A character vector of selected analytes to be included in the plot.
#' @param selected_pcspec   A character vector of selected matrix to be included in the plot.
#' @param selected_usubjids A character vector of selected unique subject identifiers (USUBJIDs)
#'                          to be included in the plot.
#' @param colorby_var       A character string specifying the variable by which to color
#'                          the lines in the plot.
#' @param time_scale        A character string specifying the time scale.
#'                          Options are "By Cycle" or other values.
#' @param yaxis_scale       A character string specifying the x-axis scale.
#'                          Options are "Log" or other values.
#' @param cycle             A character string or numeric value specifying the cycle to filter by
#'                          when `time_scale` is "By Cycle". Default is NULL.
#'
#' @return A ggplot object representing the line plot of pharmacokinetic concentration over time.
#'
#' @details
#' The function performs the following steps:a
#' \itemize{
#'   \item Filters the data based on the selected analytes, matrices, and subjects.
#'   \item Selects relevant columns and removes rows with missing concentration values.
#'   \item Converts 'USUBJID', 'NCA_PROFILE', and 'DOSEA' to factors.
#'   \item Filters the data by cycle if `time_scale` is "By Cycle"
#'          while creating duplicates for predose samples if needed.
#'   \item Adjusts concentration values for logarithmic scale if `yaxis_scale` is "Log".
#'   \item Generates a line plot using the `g_ipp` function with the specified parameters.
#'   \item Adjusts the y-axis to logarithmic scale if `yaxis_scale` is "Log".
#' }
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   plot <- general_lineplot(data = adnca_data,
#'                            selected_analytes = c("Analyte1", "Analyte2"),
#'                            selected_pcspec = c("Spec1", "Spec2"),
#'                            selected_usubjids = c("Subject1", "Subject2"),
#'                            colorby_var = "NCA_PROFILE",
#'                            time_scale = "By Cycle",
#'                            yaxis_scale = "Log",
#'                            cycle = "1")
#'   print(plot)
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom tern g_ipp
#' @export
general_lineplot <- function(
  data, selected_analytes, selected_pcspec, selected_usubjids,
  colorby_var, time_scale, yaxis_scale, cycle = NULL
) {

  # preprocess data according to user selection
  preprocessed_data <- data %>%
    filter(
      USUBJID %in% selected_usubjids,
      PARAM %in% selected_analytes,
      PCSPEC %in% selected_pcspec,
      if ("EVID" %in% names(data)) EVID == 0 else TRUE
    ) %>%
    filter(!is.na(AVAL)) %>%
    mutate(
      USUBJID = factor(USUBJID),
      NCA_PROFILE = factor(NCA_PROFILE),
      DOSEA = factor(DOSEA),
      id_var = interaction(!!!syms(colorby_var), sep = ", ")
    )
  # Check if the data is empty
  if (nrow(preprocessed_data) == 0) {
    return(ggplot() + ggtitle("No data available for selected parameters"))
  }

  # If there are predose records duplicate them in the previous line so they are considered

  if ("ARRLT" %in% names(preprocessed_data) &&
        any(preprocessed_data$ARRLT < 0 & preprocessed_data$AFRLT > 0)) {

    # alculate average cycle time per NCA_PROFILE (as character)
    dose_times <- preprocessed_data %>%
      filter(ARRLT > 0, AFRLT > 0) %>%
      mutate(AFRLT.dose = AFRLT - ARRLT) %>%
      group_by(NCA_PROFILE) %>%
      summarise(dose_time = mean(AFRLT.dose, na.rm = TRUE), .groups = "drop")

    # Duplicate pre-dose records and assign to the previous profile
    # Convert NCA_PROFILE to factor to maintain order (if needed)
    preprocessed_data <- preprocessed_data %>%
      mutate(NCA_PROFILE = as.character(NCA_PROFILE)) # Ensure character

    # Get unique ordered profiles
    profile_levels <- unique(preprocessed_data$NCA_PROFILE)
    profile_shift_map <- setNames(
      c(NA, head(profile_levels, -1)), # Previous profile
      profile_levels
    )

    # Identify pre-dose rows and assign to previous profile
    predose_dup <- preprocessed_data %>%
      filter(ARRLT < 0, AFRLT > 0) %>%
      mutate(
        new_NCA_PROFILE = profile_shift_map[NCA_PROFILE],
        dose_time = dose_times$dose_time[match(new_NCA_PROFILE, dose_times$NCA_PROFILE)],
        ARRLT = ARRLT + dose_time
      ) %>%
      filter(!is.na(new_NCA_PROFILE)) %>%
      mutate(NCA_PROFILE = new_NCA_PROFILE) %>%
      select(-new_NCA_PROFILE, -dose_time)

    # Combine the original and duplicated rows
    preprocessed_data <- bind_rows(preprocessed_data, predose_dup)

  }

  # Adjust the data selection according to input
  if (time_scale == "By Dose Profile") {
    preprocessed_data <- preprocessed_data %>%
      filter(NCA_PROFILE %in% cycle)
  }

  if (yaxis_scale == "Log") {
    preprocessed_data <- preprocessed_data %>%
      filter(AVAL > 0)
  }

  time <- if (time_scale == "By Dose Profile") "ARRLT" else "AFRLT"

  plt <- tern::g_ipp(
    df = preprocessed_data,
    xvar = time,
    yvar = "AVAL",
    xlab = paste0("Time [", unique(preprocessed_data$RRLTU), "]"),
    ylab = paste0("Concentration [", unique(preprocessed_data$AVALU), "]"),
    id_var = "id_var",
    title = "Plot of PK Concentration - Time Profile",
    subtitle = paste0(
      "Subjects: ",
      paste(unique(preprocessed_data$USUBJID), collapse = ", "),
      "\nAnalyte(s): ",
      paste(unique(preprocessed_data$PARAM), collapse = ", ")
    ),
    caption = NULL,
    add_baseline_hline = FALSE,
    yvar_baseline = "AVAL",
    col = NULL
  ) +
    labs(color = paste(colorby_var, collapse = ", "))

  if (yaxis_scale == "Log") {
    plt <- plt +
      scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                    label = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)) +
      labs(y = paste0("Log 10 - ", plt$labels$y))
  }
  return(plt)
}
