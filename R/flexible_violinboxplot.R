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
    mutate(DOSEA = as.factor(DOSEA),
           DOSNO = as.factor(DOSNO)) %>% 
    filter(PPTESTCD == parameter,
           DOSEA %in% doses_included,
           DOSNO %in% dosenumber_included) 
  
  # # xlabel of violin/boxplot
  dose_label = if ('DOSEU' %in% names(box_data)) {
                paste0("Dose [", unique(box_data$DOSEU)[1], "]")
                } else {"Dose"}
  
  # ylabel of violin/boxplot
  pptestcd_label <- if (box_data$PPORRESU[1] == "unitless" | is.na(box_data$PPORRESU[1]) | is.null(box_data$PPORRESU)) {
    parameter} else {
      paste(parameter," [", box_data$PPORRESU[1], "]")
    }
  # decide whether to layer violin or boxplot
  p <- if (box) {
    ggplot(data = box_data, aes(x = DOSEA, y = PPORRES, color = DOSNO)) + geom_boxplot()
    
  } else {
    ggplot(data = box_data, aes(x = DOSEA, y = PPORRES, color = DOSNO)) + geom_violin()
  }
  
  # add jitter, facet_wrap, and labels
  p +
    geom_point(position=position_jitterdodge()) +
    # geom_smooth(method = "lm", color = "black") + # Let's consider it for the future
    facet_wrap(~STUDYID) +
    labs(x = dose_label, y = pptestcd_label, color = "Dose Number") +
    theme(legend.position = "right",
          panel.spacing = unit(3, "lines"),
          strip.text = element_text(size = 10))
  
  
}