#' Generate a Mean Concentration Plot for ADNCA Dataset
#'
#' This function generates a mean concentration plot for an ADNCA dataset based on user-selected study IDs, analytes, and cycles. The plot can be customized to display data on a linear or logarithmic scale and can optionally include standard deviation error bars.
#'
#' @param data A data frame containing the ADNCA dataset.
#' @param selected_studyids A character vector of selected study IDs to be included in the plot.
#' @param selected_analytes A character vector of selected analytes to be included in the plot.
#' @param selected_cycles A character vector or numeric vector of selected cycles to be included in the plot.
#' @param id_variable A character string specifying the variable by which to color the lines in the plot. Default is "DOSEA".
#' @param plot_ylog A logical value indicating whether to use a logarithmic scale for the y-axis. Default is FALSE.
#' @param plot_sd A logical value indicating whether to include standard deviation error bars. Default is FALSE.
#'
#' @return A ggplot object representing the mean concentration plot.
#'
#'
#' @import dplyr
#' @import ggplot2
#' @export
#'

general_meanplot <- function(data,
                             selected_studyids,
                             selected_analytes,
                             selected_cycles,
                             id_variable = "DOSEA",
                             plot_ylog = FALSE,
                             plot_sd = FALSE) {

  # preprocess the data by summarising
  preprocessed_data <- data %>%
    filter(STUDYID == selected_studyids,
           ANALYTE == selected_analytes,
           DOSNO == selected_cycles,
           if ('EVID' %in% names(data)) EVID == 0 else T) %>%
    # rename(id_variable = id_variable) %>%
    mutate(id_variable = as.factor(!!sym(id_variable))) %>%
    group_by(id_variable, NRRLT) %>%
    summarise(Mean = geometric.mean(AVAL, na.rm = T),
              SD = sd(AVAL, na.rm = T),
              N = n()) %>%
    filter(N >= 3)

  # filter for log scaling
  if (plot_ylog) {
    preprocessed_data <- preprocessed_data %>% filter(NRRLT >0, Mean != 0)
  }

  # create the plot labels
  labels_data <- data %>%
    filter(STUDYID == selected_studyids,
           ANALYTE == selected_analytes,
           DOSNO == selected_cycles)

  time_label = paste0('Nominal Time [', unique(labels_data$RRLTU), "]")
  conc_units = paste0(unique(labels_data$AVALU))
  dose_units = paste0(unique(labels_data$DOSEU))
  conc_label = paste0('Mean', unique(labels_data$ANALYTE), 'Concentration [', conc_units, "/", dose_units, "]")

  # plot the preprocess data
  p <- ggplot(data = preprocessed_data, aes(x = NRRLT, y = Mean), group = id_variable)+
    geom_line(aes(colour = id_variable))+
    geom_point(aes(colour = id_variable))+
    labs(title = (paste(unique(labels_data$STUDYID),  unique(labels_data$ANALYTE), "Profile: ", unique(labels_data$DOSNO))),
         x = time_label,
         y = conc_label,
         color = id_variable)+
    theme_bw()+
    theme(axis.title = element_text())

  # add log scale
  if (plot_ylog) {
    p <- p + scale_y_log10()
  }
  # add sd
  if (plot_sd) {
    p <- p + geom_errorbar(aes(ymin = (Mean - SD), ymax = (Mean + SD), color = id_variable), width = 0.4)
  }

  return(p)

}

#' Helper Function: Calculate the Geometric Mean
#'
#' @param x A numeric vector.
#' @param na.rm A logical value indicating whether NA values should be removed.
#' @return The geometric mean of the input vector.
#' @export


geometric.mean <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  exp(mean(log(x)))
}
