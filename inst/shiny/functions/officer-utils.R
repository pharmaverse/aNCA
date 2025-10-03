# Using officer create a power point presentation with 2 tables and 1 plot

library(officer)
library(flextable)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)

create_pptx_doc <- function(path, title, template = NULL) {
  if (is.null(template)) {
    pptx <- read_pptx("template.pptx")
  } else {
    pptx <- read_pptx(template)
  }
  pptx <- add_slide(pptx, layout = "Title Slide", master = "Office Theme")
  pptx <- ph_with(pptx, value = title, location = ph_location_type(type = "ctrTitle"))
  pptx
}

add_pptx_sl_PlotTable <- function(pptx, df, plot) {
  pptx <- add_slide(pptx, layout = "Content with Caption")
  pptx <- ph_with(pptx, value = plot, location = "Content Placeholder 1")
  pptx <- ph_with(pptx, value = flextable::flextable(df, cwidth = 1), location = "Table Placeholder 1")
  pptx
}

add_pptx_sl_Table <- function(pptx, df) {
  pptx <- add_slide(pptx, layout = "Title Only")
  pptx <- ph_with(pptx, value = flextable::flextable(df, cwidth = 1), location = "Table Placeholder 1")
  pptx
}

add_pptx_sl_Plot <- function(pptx, plot) {
  pptx <- add_slide(pptx, layout = "Picture with Caption")
  pptx <- ph_with(pptx, value = plot, location = "Picture Placeholder 2")
  pptx
}

create_pptx_dose_slides <- function(res_dose_slides, path, title, template = NULL, slide_style = "TableTablePlot") {
  pptx <- create_pptx_doc(path, title, template)
  for (i in seq_len(length(res_dose_slides))) {
    pptx <- add_pptx_sl_Table(pptx, res_dose_slides[[i]]$info)
    pptx <- add_pptx_sl_PlotTable(
      pptx,
      df = res_dose_slides[[i]]$statistics,
      plot = res_dose_slides[[i]]$meanplot
    )
    pptx <- add_pptx_sl_Plot(pptx, res_dose_slides[[i]]$linplot)
  }
  print(pptx, target = path)
  invisible(TRUE)
}
