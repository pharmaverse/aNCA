
# Utility functions for creating PowerPoint presentations with tables and plots using officer and flextable

library(officer)
library(flextable)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)

#' Create a new PowerPoint document from a template and add a title slide
#' @param path File path to save the presentation
#' @param title Title for the presentation
#' @param template Path to PowerPoint template file
#' @return rpptx object
create_pptx_doc <- function(path, title, template) {
  pptx <- read_pptx(template)
  add_pptx_sl_title(pptx, title)
}

#' Add a title slide to the rpptx document
#' @param pptx rpptx object
#' @param title Title text
#' @return rpptx object with title slide added
add_pptx_sl_title <- function(pptx, title) {
  pptx <- add_slide(pptx, layout = "Title Slide", master = "Office Theme")
  ph_with(pptx, value = title, location = ph_location_type(type = "ctrTitle"))
}

#' Add a slide with both a plot and a table
#' @param pptx rpptx object
#' @param df Data frame to show as table
#' @param plot ggplot object to show as plot
#' @return rpptx object with slide added
add_pptx_sl_plottable <- function(pptx, df, plot) {
  pptx <- add_slide(pptx, layout = "Content with Caption")
  pptx <- ph_with(pptx, value = plot, location = "Content Placeholder 1")
  ph_with(pptx, value = flextable::flextable(df, cwidth = 1), location = "Table Placeholder 1")
}

#' Add a slide with a table only
#' @param pptx rpptx object
#' @param df Data frame to show as table
#' @return rpptx object with slide added
add_pptx_sl_table <- function(pptx, df) {
  pptx <- add_slide(pptx, layout = "Title Only")
  ph_with(pptx, value = flextable::flextable(df, cwidth = 1), location = "Table Placeholder 1")
}

#' Add a slide with a plot only
#' @param pptx rpptx object
#' @param plot ggplot object to show as plot
#' @return rpptx object with slide added
add_pptx_sl_plot <- function(pptx, plot) {
  pptx <- add_slide(pptx, layout = "Picture with Caption")
  ph_with(pptx, value = plot, location = "Picture Placeholder 2")
}

#' Create a PowerPoint presentation with dose escalation results, including main and extra figures
#' Adds slides for summary tables, mean plots, line plots, and individual subject results
#' @param res_dose_slides List of results for each dose group
#' @param path File path to save the presentation
#' @param title Title for the presentation
#' @param template Path to PowerPoint template file
#' @return TRUE (invisible)
create_pptx_dose_slides <- function(res_dose_slides, path, title, template) {
  pptx <- create_pptx_doc(path, title, template)

  # Prepare main presentation figures
  for (i in seq_len(length(res_dose_slides))) {
    pptx <- add_pptx_sl_table(pptx, res_dose_slides[[i]]$info)
    pptx <- add_pptx_sl_plottable(
      pptx,
      df = res_dose_slides[[i]]$statistics,
      plot = res_dose_slides[[i]]$meanplot
    )
    pptx <- add_pptx_sl_plot(pptx, res_dose_slides[[i]]$linplot)
  }

  # Include extra presentation figures
  add_pptx_sl_title(pptx, "Extra Figures")
  for (i in seq_len(length(res_dose_slides))) {
    pptx <- add_pptx_sl_table(pptx, res_dose_slides[[i]]$info)
    for (subj in names(res_dose_slides[[i]]$ind_params)) {
      pptx <- add_pptx_sl_plottable(
        pptx,
        df = res_dose_slides[[i]]$ind_params[[subj]],
        plot = res_dose_slides[[i]]$ind_plots[[subj]]
      )
    }
  }
  print(pptx, target = path)
  invisible(TRUE)
}
