#' Generate a Mean Concentration Plot for ADNCA Dataset
#'
#' This function generates a mean concentration plot for an ADNCA dataset based on user-selected
#' study IDs, analytes, and cycles. The plot can be customized to display data on a linear or
#' logarithmic scale and can optionally include standard deviation error bars.
#'
#' @param data              A data frame containing the ADNCA dataset.
#' @param selected_studyids A character vector of selected study IDs to be included in the plot.
#' @param selected_analytes A character vector of selected analytes to be included in the plot.
#' @param selected_pcspecs  A character vector of selected matrices to be included in the plot.
#' @param selected_cycles   A character vector or numeric vector of selected cycles to be
#'                          included in the plot.
#' @param id_variable       A character string specifying the variable by which to color the lines
#'                          in the plot. Default is "DOSEA".
#' @param groupby_var       A character string specifying the variable by which to group the data.
#' @param plot_ylog         A logical value indicating whether to use a logarithmic scale for
#'                          the y-axis. Default is FALSE.
#' @param plot_sd_min       A logical value to add a SD error bar below the mean. Default is FALSE.
#' @param plot_sd_max       A logical value to add a SD error bar above the mean. Default is FALSE.
#' @param plot_ci           A logical value indicating whether to include confidence interval 95%
#'                          ribbon. Default is FALSE.
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
                             selected_pcspecs,
                             selected_cycles,
                             id_variable = "DOSEA",
                             groupby_var = c("STUDYID", "PARAM", "PCSPEC", "NCA_PROFILE"),
                             plot_ylog = FALSE,
                             plot_sd_min = FALSE,
                             plot_sd_max = FALSE,
                             plot_ci = FALSE) {

  # preprocess the data by summarising
  preprocessed_data <- data %>%
    filter(
      STUDYID %in% selected_studyids,
      PARAM %in% selected_analytes,
      PCSPEC %in% selected_pcspecs,
      NCA_PROFILE %in% selected_cycles,
      if ("EVID" %in% names(data)) EVID == 0 else TRUE,
      NRRLT > 0
    )

  time_unit <- unique(preprocessed_data$RRLTU)
  conc_unit <- unique(preprocessed_data$AVALU)

  summarised_data <- preprocessed_data %>%
    mutate(id_variable_col = interaction(!!!syms(id_variable), sep = ", ",  drop = TRUE)) %>%
    # Create a groups variables for the labels
    mutate(groups = paste0(
      paste(!!!syms(groupby_var), sep = ", "), " [", AVALU, "]"
    )) %>%
    group_by(id_variable_col, NRRLT, groups) %>%
    summarise(
              Mean = round(mean(AVAL, na.rm = TRUE), 3),
              SD = sd(AVAL, na.rm = TRUE),
              SD_min = Mean - SD,
              SD_max = Mean + SD,
              N = n(),
              SE = SD / sqrt(N),
              CI_lower = Mean - 1.96 * SE,
              CI_upper = Mean + 1.96 * SE,
              .groups = "drop") %>%
    select(NRRLT, Mean, SD, N, CI_lower, CI_upper, id_variable_col, groups, SD_min, SD_max, SE) %>%
    # Filter means/averages calculated with less than 3 points
    filter(N >= 3)

  # filter for log scaling y values that equal 0
  if (plot_ylog) {
    summarised_data <- summarised_data %>%
      filter(Mean > 0) %>%
      mutate(
        log10_Mean = log10(Mean),
        log10_SD = SD / (Mean * log(10)),  # Calculation of SD in log10 scale
        SD_min = 10^(log10_Mean - log10_SD),
        SD_max = 10^(log10_Mean + log10_SD),
        log10_SE = SE / (Mean * log(10)),  # Calculation of SE in log10 scale
        log10_CI = 1.96 * log10_SE,  # CI in the log10 scale
        CI_lower = 10^(log10_Mean - log10_CI),
        CI_upper = 10^(log10_Mean + log10_CI)
      )
  }

  # Check if preprocessed_data is empty
  if (nrow(summarised_data) == 0) {
    empty_plot <- ggplot() + labs(title = "No data available")
    return(empty_plot)
  }

  # plot the preprocess data
  p <- ggplot(data = summarised_data, aes(x = NRRLT, y = Mean)) +
    geom_line(aes(colour = id_variable_col)) +
    geom_point(aes(colour = id_variable_col)) +
    facet_wrap(
      ~groups,
      strip.position = "top",
      scales = "free_y"
    ) +
    labs(
      x = paste0("Nominal Time [", time_unit, "]"),
      y = paste0("Mean concentration [", paste0(conc_unit, collapse = ", "), "]"),
      color = paste0(id_variable, collapse = ", ")
    ) +
    theme_bw() +
    theme(legend.position = "right",
          panel.spacing = unit(1, "lines"),
          strip.text = element_text(size = 8),
          strip.background = element_rect(fill = "grey90", color = "grey50"),
          plot.margin = margin(10, 10, 10, 10, "pt"))

  # add sd error bars
  if (plot_sd_min || plot_sd_max) {
    if (plot_ylog) {
      ymin <- if (plot_sd_min) summarised_data$SD_min else summarised_data$log10_Mean
      ymax <- if (plot_sd_max) summarised_data$SD_max else summarised_data$log10_Mean
    } else {
      ymin <- if (plot_sd_min) summarised_data$SD_min else summarised_data$Mean
      ymax <- if (plot_sd_max) summarised_data$SD_max else summarised_data$Mean
    }
    p <- p +
      geom_errorbar(aes(ymin = ymin, ymax = ymax, color = id_variable_col), width = 0.4)
  }

  # add ci
  if (plot_ci) {
    p <- p +
      geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, color = id_variable_col), alpha = 0.3)
  }

  # add log scale
  if (plot_ylog) {
    p <- p +
      scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                    label = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)) +
      labs(y = paste0("Log 10 - ", p$labels$y))
  }

  # Convert ggplot to plotly
  p
}
