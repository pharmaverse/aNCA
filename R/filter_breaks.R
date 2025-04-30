
#' Filter Breaks for X-Axis
#'
#' Filters X-axis for consecutive breaks with at least the specified distance.
#'
#' @param breaks          A numeric vector of x-axis breaks.
#' @param plot            A ggplot object used to extract plot dimensions and scales.
#' @param min_cm_distance A numeric of the minimum distance between breaks.
#' @param axis            Axis to filter on, either "x" or "y".
#' @return A numeric vector of filtered x-axis breaks.
#' @importFrom ggplot2 ggplot_build ggplot_gtable
#' @importFrom grid convertUnit
#' @importFrom stats na.omit
#' @author Gerardo Rodriguez
#' @export
filter_breaks <- function(breaks = NA,
                          plot = plot,
                          min_cm_distance = 0.5,
                          axis = "x") {

  breaks <- unique(na.omit(sort(breaks)))
  plot_build <- ggplot_build(plot)
  plot_table <- ggplot_gtable(plot_build)

  # Extract axis scale information
  if (axis == "x") {
    scale_range <- plot_build$layout$panel_params[[1]]$x.range
  } else if (axis == "y") {
    scale_range <- plot_build$layout$panel_params[[1]]$y.range
  } else {
    stop("Error: Invalid axis specified. Use 'x' or 'y'.")
  }

  # Identify the panel grob
  panel_index <- which(sapply(plot_table$grobs,
                              function(x) grepl("panel", x$name)))

  if (length(panel_index) == 0) {
    stop("Error: Panel grob not found.")
  }
  panel <- plot_table$grobs[[panel_index]]

  # Extract the panel border grob to get the width or height
  panel_border <- panel$children[[
    which(sapply(panel$children, function(x) {
      grepl("panel.border", x$name)
    }))
  ]]

  # Convert panel width or height to cm
  if (axis == "x") {
    panel_size_cm <- grid::convertUnit(panel_border$width,
                                       unitTo =  "cm",
                                       valueOnly = TRUE)
  } else {
    panel_size_cm <- grid::convertUnit(panel_border$height,
                                       unitTo =  "cm",
                                       valueOnly = TRUE)
  }

  # Calculate the distance between breaks in cm
  break_distances <- diff(breaks) / diff(scale_range) * panel_size_cm

  # Filter only breaks that satisfy the minimum distance
  filt_breaks <- breaks[1]

  for (i in 2:length(breaks)) {

    # Take latest selected break and calculate its distance
    b0 <- filt_breaks[length(filt_breaks)]
    bdist <- (breaks[i] - b0) / diff(scale_range) * panel_size_cm

    if (bdist >= min_cm_distance) {
      filt_breaks <- c(filt_breaks, breaks[i])
    }
  }
  return(filt_breaks)
}
