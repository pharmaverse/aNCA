dose_plot <- function(data) {
  
  time <- "AFRLT"
  
  plt <- tern::g_ipp(
    df = data,
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
  
  if (show_threshold) {
    plt <- plt +
      ggplot2::geom_hline(yintercept = threshold_value, linetype = "dotted", color = "red")
  }
  
  return(plt)
}
