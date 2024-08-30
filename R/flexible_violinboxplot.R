#' Flexible Violin/Box Plot
#'
#' This function generates a  violin or box plot based on the provided data, parameter, and dose information.
#'
#' @param result_data A list containing the data to be plotted. It should have a `data` element with dose information and a `formatted` element with the data to be plotted.
#' @param parameter A string specifying the parameter to be plotted.
#' @param doses_included A vector of doses to be included in the plot.
#' @param dosenumber_included A vector of dose numbers to be included in the plot.
#' @param box A logical value indicating whether to plot a box plot (`TRUE`) or a violin plot (`FALSE`). Default is `TRUE`.
#'
#' @return A ggplot object representing the violin or box plot.
#' @import dplyr
#' @import ggplot2
#' @import forcats
#' @export


flexible_violinboxplot <- function(result_data, 
                                   parameter, 
                                   doses_included,
                                   dosenumber_included,
                                   box = TRUE) {
  
  
  # preprocess data to plot
  box_data <- result_data %>%
    mutate(DOSEA_factor = as.factor(DOSEA)) %>% 
    filter(PARAM == parameter) %>% 
    # adapt DOSNO to be discrete with the fct_relevel library
    mutate(DOSNO = as.factor(DOSNO)) %>%
    filter(DOSEA %in% doses_included, 
           DOSNO %in% dosenumber_included)
  
  # # xlabel of violin/boxplot
  dose_label = paste0("Dose [",  paste0(unique(box_data$DOSEU)), ']')
  # ylabel of violin/boxplot
  col_name <- if (box_data$AVALU[1] == "unitless") {
    parameter} else {
      paste(parameter," [", box_data$AVALU[1], "]")
    }
  # decide whether to layer violin or boxplot
  p <- if (box) {
    ggplot(data = box_data, aes(x = DOSEA_factor, y = AVAL, color = DOSNO)) + geom_boxplot()
      
  } else {
    ggplot(data = box_data, aes(x = DOSEA_factor, y = AVAL, color = DOSNO)) + geom_violin()
  }
  
  # add jitter, facet_wrap, and labels
  p +
    geom_point(position=position_jitterdodge()) +
    # geom_smooth(method = "lm", color = "black") + # This is conceptually wrong, we need to do an ANOVA here
    facet_wrap(~STUDYID) +
    labs(x = dose_label, y = col_name, color = "Dose Number") +
    theme(legend.position = "right",
          panel.spacing = unit(3, "lines"),
          strip.text = element_text(size = 10))
  
  
}