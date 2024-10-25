#' Generate a Mean Concentration Plot for ADNCA Dataset
#'
#' This function generates a mean concentration plot for an ADNCA dataset based on user-selected
#' study IDs, analytes, and cycles. The plot can be customized to display data on a linear or
#' logarithmic scale and can optionally include standard deviation error bars.
#'
#' @param data              A data frame containing the ADNCA dataset.
#' @param selected_studyids A character vector of selected study IDs to be included in the plot.
#' @param selected_analytes A character vector of selected analytes to be included in the plot.
#' @param selected_cycles   A character vector or numeric vector of selected cycles to be
#'                          included in the plot.
#' @param id_variable       A character string specifying the variable by which to color the lines
#'                          in the plot. Default is "DOSEA".
#' @param plot_ylog         A logical value indicating whether to use a logarithmic scale for
#'                          the y-axis. Default is FALSE.
#' @param plot_sd           A logical value indicating whether to include standard deviation
#'                          error bars. Default is FALSE.
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
                             plot_sd = FALSE,
                             plot_cv = FALSE) {


  # preprocess the data by summarising
  preprocessed_data <- data %>%
    filter(
      STUDYID %in% selected_studyids,
      ANALYTE %in% selected_analytes,
      DOSNO %in% selected_cycles,
      if ("EVID" %in% names(data)) EVID == 0 else TRUE,
      NRRLT > 0
    ) %>%
    # rename(id_variable = id_variable) %>%
    mutate(id_variable = as.factor(!!sym(id_variable))) %>%
    # Create a groups variables for the labels
    mutate(groups = paste(STUDYID, ANALYTE, DOSNO, sep = ", ")) %>%
    group_by(id_variable, NRRLT) %>%
    mutate(
      Mean = round(geometric_mean(AVAL, na.rm = TRUE), 3),
      SD = sd(AVAL, na.rm = TRUE),
      CV = (SD / Mean) * 100,
      N = n()
    ) %>%
    select(where(~n_distinct(.) == 1), Mean, SD, CV, N) %>%
    slice(1) %>%
    # Filter means/averages calculated with less than 3 points
    filter(N >= 3)
  
  # filter for log scaling y values that equal 0
  if (plot_ylog) {
    preprocessed_data <- preprocessed_data %>% filter(Mean != 0)
  }
  
  # Check if preprocessed_data is empty
  if (nrow(preprocessed_data) == 0) {
    empty_plot <- ggplot() + labs(title = "No data available")
    return(ggplotly(empty_plot))
  }

  # plot the preprocess data
  p <- ggplot(data = preprocessed_data, aes(x = NRRLT, y = Mean), group = id_variable) +
    geom_line(aes(colour = id_variable)) +
    geom_point(aes(colour = id_variable)) +
    facet_wrap(~groups,
               strip.position = "top") +
    labs(
      x = paste0("Nominal Time [", preprocessed_data$RRLTU[1], "]"),
      y = paste0(
        "Mean concentration", " [", paste(unique(preprocessed_data$AVALU), collapse = ","), "]"
      ),
      color = id_variable
    ) +
    theme_bw() +
    theme(legend.position = "right",
          panel.spacing = unit(1, "lines"),
          strip.text = element_text(size = 8),
          strip.background = element_rect(fill = "grey90", color = "grey50"),
          plot.margin = margin(10, 10, 10, 10, "pt"))

  # add log scale
  if (plot_ylog) {
    p <- p + scale_y_log10()
  }
  # add sd
  if (plot_sd) {
    p <- p +
      geom_errorbar(aes(ymin = (Mean - SD), ymax = (Mean + SD), color = id_variable), width = 0.4)
  }
  # add sd
  if (plot_cv) {
    p <- p +
      geom_errorbar(aes(ymin = (Mean - CV), ymax = (Mean + CV), color = id_variable), width = 0.4)
  }
  # Convert ggplot to plotly
  p_plotly <- ggplotly(p)
  
  return(p_plotly)
}

#' Helper Function: Calculate the Geometric Mean
#'
#' @param x A numeric vector.
#' @param na.rm A logical value indicating whether NA values should be removed.
#' @return The geometric mean of the input vector.
#' @export
geometric_mean <- function(x, na.rm = FALSE) { # nolint
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  exp(mean(log(x)))
}

#' Derived function from labeller label_both: Labels in one line
#'
#' @param x Column names as character vector
#' @return Corresponding labels of the facet plots split by the specified colimns (labels)
#' @export
