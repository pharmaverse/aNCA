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
#' @param show_threshold    A boolean specifying whether to show a threshold line or not.
#'                          Default is FALSE.
#' @param threshold_value   A numeric value to set the y value of the threshold line.
#'                          Default is 0.
#' @param show_dose         A boolean specifying whether to show dose times as vertical lines.
#'
#' @returns A ggplot object representing the line plot of pharmacokinetic concentration over time.
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
#'   \item Adds a horizontal line for the threshold value if `show_threshold` is TRUE.
#'   \item Adds vertical lines for dose times if `show_dose` is TRUE
#'   and the number of subjects is less than 5.
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
#'                            cycle = "1",
#'                            show_threshold = TRUE,
#'                            threshold_value = 1)
#'   print(plot)
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom tern g_ipp
#' @importFrom utils head
#' @export
general_lineplot <- function(
  data, selected_analytes, selected_pcspec, selected_usubjids,
  colorby_var, time_scale, yaxis_scale, show_threshold = FALSE,
  threshold_value = 0, show_dose = FALSE, cycle = NULL
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


  # Adjust the data selection according to input
  if (time_scale == "By Dose Profile") {
    # If there are predose records duplicate them in the previous line so they are considered

    if ("ARRLT" %in% names(preprocessed_data) &&
          any(preprocessed_data$ARRLT < 0 & preprocessed_data$AFRLT > 0)) {

      preprocessed_data <- dose_profile_duplicates(preprocessed_data,
                                                   groups = c("USUBJID", "PCSPEC",
                                                              "PARAM", "NCA_PROFILE"),
                                                   dosno = "NCA_PROFILE")

    }

    preprocessed_data <- preprocessed_data %>%
      mutate(id_var = interaction(!!!syms(colorby_var), sep = ", ")) %>%
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
    col = colorby_var
  ) +
    labs(color = paste(colorby_var, collapse = ", "))

  # Add optional layers based on user input
  plt <- add_optional_layers(
    plt = plt,
    yaxis_scale = yaxis_scale,
    show_threshold = show_threshold,
    threshold_value = threshold_value,
    show_dose = show_dose,
    data = preprocessed_data,
    time_scale = time_scale
  )

  return(plt)
}

#' Helper function to handle optional layers
#' @param plt The ggplot object to modify
#' @param yaxis_scale The scale of the y-axis ("Log" or "Linear")
#' @param show_threshold Whether to show a threshold line
#' @param threshold_value The value of the threshold line
#' @param show_dose Whether to show dose times as vertical lines
#' @param data The data used for plotting
#' @param time_scale The time scale used for plotting
#' #' @returns The modified ggplot object with optional layers added
#'
add_optional_layers <- function(plt, yaxis_scale, show_threshold,
                                threshold_value, show_dose,
                                data, time_scale) {
  # Adjust the y-axis scale if specified
  if (yaxis_scale == "Log") {
    plt <- plt +
      scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                    label = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)) +
      labs(y = paste0("Log 10 - ", plt$labels$y))
  }

  # Add a horizontal line for the threshold value if specified
  if (show_threshold) {
    plt <- plt +
      geom_hline(yintercept = threshold_value, linetype = "dotted", color = "red")
  }

  # Add vertical lines for dose times if specified and conditions are met
  if (show_dose &&
        time_scale != "By Dose Profile") {
    dose_info <- data %>%
      distinct(TIME_DOSE, DOSEA) %>%
      filter(!is.na(TIME_DOSE))
    
    plt <- plt +
      geom_vline(data = dose_info,
                 aes(xintercept = TIME_DOSE),
                 linetype = "dotted",
                 size = 0.8,
                 color = "green")
  }

  return(plt)
}
