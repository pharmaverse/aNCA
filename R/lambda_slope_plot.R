#' Generate a Lambda Slope Plot
#'
#' This function generates a lambda slope plot using pharmacokinetic data. It calculates relevant lambda parameters and visualizes the data points used for lambda calculation, along with a linear regression line and additional plot annotations.
#'
#' @param PKNCAres_df Data frame containing the results of the pharmacokinetic non-compartmental analysis (default is `PKNCA::pk.nca(.)$result`).
#' @param PKNCAconc_df Data frame containing the concentration data (default is `mydata$conc$data`).
#' @param dosno Numeric value representing the dose number (default is `profile`).
#' @param usubjid Character value representing the unique subject identifier (default is `patient`).
#' @param R2ADJTHRESHOL Numeric value representing the R-squared adjusted threshold for determining the subtitle color (default is 0.7).
#'
#' @return A plotly object representing the lambda slope plot.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Filters and arranges the input data to obtain relevant lambda calculation information.
#'   \item Identifies the data points used for lambda calculation.
#'   \item Calculates the fitness, intercept, and time span of the half-life estimate.
#'   \item Determines the subtitle color based on the R-squared adjusted value and half-life estimate.
#'   \item Generates a ggplot object with the relevant data points, linear regression line, and annotations.
#'   \item Converts the ggplot object to a plotly object for interactive visualization.
#' }
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   plot <- lambda_slope_plot(PKNCAres_df = myres$result, 
#'                             PKNCAconc_df = mydata$conc$data, 
#'                             dosno = 1, 
#'                             usubjid = "subject_1", 
#'                             R2ADJTHRESHOL = 0.7)
#'   plot
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @export

lambda_slope_plot = function(PKNCAres_df = myres$result, 
                             PKNCAconc_df = mydata$conc$data, 
                             dosno=profile, 
                             usubjid=patient, 
                             R2ADJTHRESHOL=0.7){
  
  # Obtain all information relevant regarding lambda calculation
  lambda_res = PKNCAres_df %>%
    filter(DOSNO==dosno, USUBJID==usubjid, end==Inf)  %>% 
    arrange(USUBJID, DOSNO, start, desc(end))  %>%
    filter(!duplicated(paste0(USUBJID, DOSNO, PPTESTCD))) 
  
  # Obtain the number of data points used to calculate lambda
  lambda.z.n.points = as.numeric(lambda_res$PPORRES[lambda_res$PPTESTCD=='lambda.z.n.points'])
  if (is.na(lambda.z.n.points)) lambda.z.n.points = 0
  
  # Identify in the data the points used to calculate lambda
  lambda.z.ix.rows = PKNCAconc_df   %>% 
    filter(DOSNO==dosno, USUBJID==usubjid, !exclude_half.life,
           TIME>=sum(subset(lambda_res, lambda_res$PPTESTCD=='lambda.z.time.first', select=c('start', 'PPORRES'))) 
    )  %>% 
    arrange(IX) %>%
    slice(0:lambda.z.n.points)

  # Calculate the base and adjusted fitness, half life and time span estimated
  R2_value <- signif(as.numeric(lambda_res$PPORRES[lambda_res$PPTESTCD=='r.squared']), 3)
  R2ADJ_value <- signif(as.numeric(lambda_res$PPORRES[lambda_res$PPTESTCD=='adj.r.squared']), 3)
  half_life_value <- signif(log(2)/as.numeric(lambda_res$PPORRES[lambda_res$PPTESTCD=='lambda.z']), 3)
  time_span <- signif(abs(lambda.z.ix.rows$TIME[nrow(lambda.z.ix.rows)] - lambda.z.ix.rows$TIME[1]), 3)
  
  # Determine the color based on the conditions
  subtitle_color <- ifelse(R2ADJ_value < R2ADJTHRESHOL | half_life_value > (time_span/2), "red", "black")
  subtitle_text <- paste0("R^2: ", signif(as.numeric(lambda_res$PPORRES[lambda_res$PPTESTCD=='r.squared']), 3), 
                          "    R^2_{adj}: ", R2ADJ_value, 
                          "  ln(2)/lambda = ", half_life_value, "h    ",
                          "(T_", lambda.z.ix.rows$IX[nrow(lambda.z.ix.rows)], " - T_", lambda.z.ix.rows$IX[1], ")/2 = ", time_span/2, 'h'
  )
  
  subtitle_text <- paste0(
    # "R<sup>2</sup>: ", R2_value,
    "    R<sup>2</sup><sub>adj</sub>: ", R2ADJ_value, 
    "    ln(2)/\u03BB = ", half_life_value, "h",
    "    (T<sub>", lambda.z.ix.rows$IX[2], "</sub> - T<sub>", lambda.z.ix.rows$IX[1], "</sub>)/2 = ", time_span/2, "h"
  )
  
  
  # If Cmax is included in lambda calculation, inform the user in the plot
  if(lambda_res$PPORRES[lambda_res$PPTESTCD=='cmax'] <=  max(lambda.z.ix.rows$AVAL)){
    subtitle_color = 'red'
    subtitle_text = paste0('Cmax should not be included in lambda calculation', '\n', subtitle_text)
  }
  
  
  # Include in the data the aesthetics for the plot
  plot_data = PKNCAconc_df  %>%
    filter(DOSNO==dosno, USUBJID==usubjid) %>% 
    arrange(IX)  %>% 
    mutate(IX_shape =ifelse(is.excluded.hl, 'excluded', 'included'),
           IX_stroke = ifelse(is.excluded.hl, 4, 1),
           IX_color =case_when(
             is.excluded.hl ~ 'excluded',
             IX %in% lambda.z.ix.rows$IX ~ 'hl.included',
             TRUE ~ 'hl.excluded') 
    )   %>% 
    as.data.frame()
  
  
  # Generate the base scatter ggplot 
  p = plot_data  %>% 
    ggplot(aes(x = TIME, y = AVAL)) +
    #geom_segment_layer+
    geom_line(color = 'gray70', linetype='solid', linewidth=1)+
    geom_smooth(data=subset(plot_data, IX_color=='hl.included'),method = 'lm', se = F, color = 'green3', linetype='solid', linewidth=1) +
    geom_point(data = plot_data, aes(shape = IX_shape, color= IX_color, stroke=IX_stroke), size = 5) +
    labs(
      title = paste0('USUBJID: ', usubjid, ', DOSNO: ', dosno),
      y = paste0('Log10 Concentration (', PKNCAconc_df $PCSTRESU[1], ')'),
      x = paste0('Actual time post dose (', PKNCAconc_df $RRLTU[1], ')')
    ) +
    theme_bw() +
    
    theme(
      plot.title = element_text(hjust=0, face = "bold", size = 20, family = 'serif'),
      plot.subtitle = element_text(hjust=0, color = subtitle_color, size = 18, family = 'serif'),
      legend.position="none",
      axis.text = element_text(size = 15),
      axis.title.x = element_text(size = 15, family = "serif", margin=margin(t=0)),
      axis.title.y = element_text(size = 15, family = "serif", margin=margin(r=10)),
      panel.border = element_rect(colour = "gray35", fill=NA, size=1),
      panel.grid.major = element_line(colour = "gray90"),
      plot.margin = margin(t = 5, r = 5, b = 35, l = 5)  # Add margin space below the X-axis title
    ) +
    scale_shape_manual(values = c("included" = 16, "excluded" = 3)) +
    scale_color_manual(values = c("hl.included" = "green4", "hl.excluded" = "black", "excluded" ="red3"))+
    scale_y_log10()
  
  # Make a plotly interactive plot
  #library(plotly)
  pl <- ggplotly(p) %>% 
    layout(
      annotations = list(
        text = subtitle_text,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "top",
        font = list(size = 15, color = subtitle_color, family='times'),
        x  = 0,
        y = 1.105
      ),
      hoverlabel = list(font= list(family='times', size=20)),
      width=800, height=600
    )  %>% 
    config(mathjax='cdn')  %>%
    style(hovertext=~paste0('Data Point: ',IX), hoverinfo='none', traces=1)  %>% 
    style(hovertext=~paste0('Data Point: ',IX), hoverinfo='none', traces=2)  %>% 
    style(hovertext=~paste0('Data Point: ',IX), hoverinfo='none', traces=3)  %>% 
    style(hovertext=~paste0('Data Point: ',IX), hoverinfo='none', traces=4)  %>% 
    style(hovertext=~paste0('Data Point: ',IX), hoverinfo='none', traces=5)  %>% 
    # Make this trace the only one 
    add_trace(data = plot_data  %>% filter(DOSNO==dosno, USUBJID==usubjid),
              x = ~TIME, y = ~log10(AVAL),
              customdata = ~paste0(USUBJID, '_', DOSNO, '_', IX),
              text = ~paste0('Data Point: ',IX, '\n', '(', signif(TIME,2), ' , ', signif(AVAL,2), ')'),
              type = 'scatter',
              mode = 'markers',
              name = 'Data Points',
              hoverinfo = 'text',
              marker = list(color = case_when(
                plot_data$is.excluded.hl ~ 'red',
                plot_data$IX %in% lambda.z.ix.rows$IX ~ 'green',
                TRUE ~ 'black'
              ), size = 50, opacity = 0),  # Make points semi-transparent
              showlegend = FALSE  # Don't show this trace in the legend
    )
  
  return(pl)
  
}

