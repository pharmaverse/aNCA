#' Generate a General Line Plot for ADNCA Dataset
#'
#' This function generates a line plot for an ADNCA dataset based on user-selected analytes,
#' subjects, and other parameters. The plot can be customized to display data on a linear or
#' logarithmic scale and can be filtered by cycle.
#'
#' @param data              A data frame containing the ADNCA dataset.
#' @param selected_analytes A character vector of selected analytes to be included in the plot.
#' @param selected_usubjids A character vector of selected unique subject identifiers (USUBJIDs)
#'                          to be included in the plot.
#' @param colorby_var       A character string specifying the variable by which to color
#'                          the lines in the plot.
#' @param time_scale        A character string specifying the time scale.
#'                          Options are "By Cycle" or other values.
#' @param xaxis_scale       A character string specifying the x-axis scale.
#'                          Options are "Log" or other values.
#' @param cycle             A character string or numeric value specifying the cycle to filter by
#'                          when `time_scale` is "By Cycle". Default is NULL.
#'
#' @return A ggplot object representing the line plot of pharmacokinetic concentration over time.
#'
#' @details
#' The function performs the following steps:a
#' \itemize{
#'   \item Filters the data based on the selected analytes and subjects.
#'   \item Selects relevant columns and removes rows with missing concentration values.
#'   \item Converts 'USUBJID', 'DOSNO', and 'DOSEA' to factors.
#'   \item Filters the data by cycle if `time_scale` is "By Cycle".
#'   \item Adjusts concentration values for logarithmic scale if `xaxis_scale` is "Log".
#'   \item Generates a line plot using the `g_ipp` function with the specified parameters.
#'   \item Adjusts the y-axis to logarithmic scale if `xaxis_scale` is "Log".
#' }
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   plot <- general_lineplot(data = adnca_data,
#'                            selected_analytes = c("Analyte1", "Analyte2"),
#'                            selected_usubjids = c("Subject1", "Subject2"),
#'                            colorby_var = "DOSNO",
#'                            time_scale = "By Cycle",
#'                            xaxis_scale = "Log",
#'                            cycle = "1")
#'   print(plot)
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import nestcolor
#' @importFrom tern g_ipp
#' @export
general_lineplot <- function(
  data, selected_analytes, selected_usubjids, colorby_var, time_scale, xaxis_scale, cycle = NULL
) {
  # preprocess data according to user selection
  preprocessed_data <- data %>%
    filter(
      USUBJID %in% selected_usubjids,
      ANALYTE %in% selected_analytes,
      if ("EVID" %in% names(data)) EVID == 0 else TRUE
    ) %>%
    # filter only the ones where time conc are na, use columns names
    select(
      ARRLT, PCSPEC, AVAL, DOSEA, DOSNO, AFRLT, NRRLT, USUBJID, ANALYTE, STUDYID, AVALU, RRLTU
    ) %>%
    filter(!is.na(AVAL)) %>%
    mutate(
      USUBJID = factor(USUBJID),
      DOSNO = factor(DOSNO),
      DOSEA = factor(DOSEA),
      id_var = interaction(!!!syms(colorby_var), sep = ", ")
    )

  # If there are predose records duplicate them in the previous line so they are considered
  if ("ARRLT" %in% names(preprocessed_data) &&
        any(preprocessed_data$ARRLT < 0 & preprocessed_data$AFRLT > 0)) {

    cycle_times <- preprocessed_data  %>%
      filter(preprocessed_data$ARRLT > 0, preprocessed_data$AFRLT > 0) %>%
      mutate(AFRLT.dose = AFRLT - ARRLT) %>%
      group_by(DOSNO) %>%
      summarise(AFRLT.dose = mean(AFRLT.dose, na.rm = TRUE)) %>%
      pull(AFRLT.dose, DOSNO)

    predose_records <- preprocessed_data %>%
      filter(ARRLT < 0, AFRLT > 0) %>%
      mutate(DOSNO = ifelse(
        AFRLT < as.numeric(cycle_times[as.character(DOSNO)]),
        as.numeric(DOSNO) - 1,
        as.numeric(DOSNO) + 1
      ))

    preprocessed_data <- rbind(predose_records, preprocessed_data)

  }


  # Adjust the data selection according to input
  if (time_scale == "By Cycle") {
    preprocessed_data <- preprocessed_data %>%
      filter(DOSNO %in% cycle)
  }

  if (xaxis_scale == "Log") {
    preprocessed_data <- preprocessed_data %>%
      mutate(AVAL = ifelse(AVAL == 0, 0.001, AVAL))
  }

  time <- if (time_scale == "By Cycle") {
    "ARRLT"
  } else {
    "AFRLT"
  }

  plt <- g_ipp(
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
      "\nAnalyte: ",
      unique(preprocessed_data$ANALYTE)
    ),
    caption = NULL,
    add_baseline_hline = FALSE,
    yvar_baseline = "AVAL",
    ggtheme = nestcolor::theme_nest(),
    col = NULL
  ) +
    labs(color = paste(colorby_var, collapse = ", "))

  if (xaxis_scale == "Log") {
    plt <- plt +
      scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1, 10), label = c(0.001, 0.01, 0.1, 1, 10)) +
      annotation_logticks(sides = "l")
  }

  return(plt)
}
