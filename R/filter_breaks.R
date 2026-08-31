#' Filter Breaks for X-Axis
#'
#' Filters an axis for consecutive breaks that are far enough apart to be drawn without their
#' labels colliding. A break is kept when its distance from the last kept break is at least
#' `min_cm_distance` *and* at least the space the two labels occupy along the axis, so a wide
#' label such as `119.917` thins the axis further than a short one such as `12` does.
#'
#' @param breaks          A numeric vector of x-axis breaks.
#' @param plot            A ggplot object used to extract plot dimensions and scales.
#' @param min_cm_distance A numeric of the minimum distance between breaks.
#' @param axis            Axis to filter on, either "x" or "y".
#' @param labels          A function turning `breaks` into the labels that will be drawn, used
#'                        to measure how much room each one needs. Defaults to the same
#'                        labels the concentration plots draw.
#' @returns A numeric vector of filtered x-axis breaks.
#' @importFrom ggplot2 ggplot_build ggplot_gtable
#' @importFrom grid convertUnit
#' @importFrom stats na.omit
#' @author Gerardo Rodriguez
#' @export
filter_breaks <- function(breaks = NA,
                          plot = plot,
                          min_cm_distance = 0.5,
                          axis = "x",
                          labels = format_axis_labels) {

  breaks <- unique(na.omit(sort(breaks)))
  plot_build <- ggplot_build(plot)
  plot_table <- ggplot_gtable(plot_build)

  if (length(breaks) <= 1) {
    return(breaks)
  }

  # Identify the panel grob
  panel_index <- which(sapply(plot_table$grobs,
                              function(x) grepl("panel", x$name)))

  if (length(panel_index) == 0) {
    stop("Error: Panel grob not found.")
  }
  # A faceted plot has one grob per panel; they are laid out alike, so check the first
  panel <- plot_table$grobs[[panel_index[1]]]

  # Find the panel border grob index
  border_index <- panel$children %>%
    sapply(\(x) x$name) %>%
    grepl("panel.border", .) %>%
    which()

  # Extract the panel border grob. Its width is unit(1, "npc"), so it only tells us the panel
  # is drawable; how much room the panel actually gets comes from the gtable layout below
  panel_border <- panel$children[[border_index]]

  if (is.null(panel_border$width) || is.null(panel_border$height)) {
    stop("Error: Panel border lacks a 'width' or 'height' property.")
  }
  # Extract axis scale information
  if (axis == "x") {
    scale_range <- plot_build$layout$panel_params[[1]]$x.range
  } else if (axis == "y") {
    scale_range <- plot_build$layout$panel_params[[1]]$y.range
  } else {
    stop("Error: Invalid axis specified. Use 'x' or 'y'.")
  }
  panel_size_cm <- .panel_size_cm(plot_table, axis)

  # Room each label needs along the axis, so wide labels are not allowed to collide even
  # where the ticks themselves clear min_cm_distance
  if (!is.function(labels)) {
    stop("Error: 'labels' must be a function applied to the breaks.")
  }
  rendered <- labels(breaks)

  if (length(rendered) != length(breaks)) {
    stop("Error: 'labels' must return one label per break.")
  }
  label_sizes_cm <- .label_extents_cm(rendered, plot, axis)

  # Filter only breaks that satisfy the minimum distance
  filt_breaks <- breaks[1]
  last_kept <- 1

  for (i in 2:length(breaks)) {

    # Take latest selected break and calculate its distance
    b0 <- filt_breaks[length(filt_breaks)]
    bdist <- (breaks[i] - b0) / diff(scale_range) * panel_size_cm

    # Adjacent labels are centered on their ticks, so each contributes half its size
    needed <- max(min_cm_distance, (label_sizes_cm[last_kept] + label_sizes_cm[i]) / 2)

    if (bdist >= needed) {
      filt_breaks <- c(filt_breaks, breaks[i])
      last_kept <- i
    }
  }
  filt_breaks
}

#' Room a single panel gets along one axis
#'
#' Reads the laid-out size of a panel from the plot's `gtable`. The panel border grob cannot be
#' used for this: its width and height are `unit(1, "npc")`, which converts to the size of the
#' whole device, ignoring both the space the axes and titles take and the fact that a faceted
#' plot splits what is left between several panels.
#'
#' @param plot_table A `gtable`, as returned by [ggplot2::ggplot_gtable()].
#' @param axis       Axis to measure along, either "x" or "y".
#'
#' @returns The size of one panel in centimeters.
#' @keywords internal
.panel_size_cm <- function(plot_table, axis) {

  horizontal <- axis == "x"
  sizes <- if (horizontal) plot_table$widths else plot_table$heights
  convert <- if (horizontal) grid::convertWidth else grid::convertHeight

  # Panels are the cells sized in "null" units; axes, titles and margins take a fixed amount
  # off the device first and the panels share whatever is left
  flexible <- grid::unitType(sizes) == "null"
  fixed_cm <- sum(convert(sizes[!flexible], "cm", valueOnly = TRUE))
  free_cm <- max(0, convert(grid::unit(1, "npc"), "cm", valueOnly = TRUE) - fixed_cm)

  cells <- plot_table$layout[grepl("^panel", plot_table$layout$name), , drop = FALSE]
  cell <- if (horizontal) cells$l[1] else cells$t[1]

  free_cm * as.numeric(sizes[cell]) / sum(as.numeric(sizes[flexible]))
}

#' Room axis labels take up along their own axis
#'
#' Measures each rendered label with the plot's `axis.text` styling and returns the extent it
#' occupies in the direction breaks are spaced along: width for an x axis, height for a y
#' axis. Rotated labels contribute a mix of the two.
#'
#' The extent includes a space either side of the label. Without it two labels are allowed to
#' sit edge to edge, which reads as one run-on label rather than two.
#'
#' @param labels A character vector of rendered axis labels.
#' @param plot   The ggplot the labels belong to, used for the axis text theme element.
#' @param axis   Axis the labels sit on, either "x" or "y".
#'
#' @returns A numeric vector of extents in centimeters, one per label.
#' @importFrom ggplot2 calc_element theme_get
#' @keywords internal
.label_extents_cm <- function(labels, plot, axis) {

  # theme_get() supplies the parents calc_element() needs when the plot has a partial theme
  theme <- plot$theme
  if (!isTRUE(attr(theme, "complete"))) {
    theme <- theme_get() + theme
  }
  element <- calc_element(paste0("axis.text.", axis), theme)

  # A blanked axis draws no labels, so nothing can collide
  if (inherits(element, "element_blank")) {
    return(rep(0, length(labels)))
  }

  text_gp <- grid::gpar(
    fontsize = element$size,
    fontfamily = element$family,
    fontface = element$face
  )
  angle <- if (is.null(element$angle)) 0 else element$angle * pi / 180

  vapply(labels, function(label) {
    grob <- grid::textGrob(paste0(" ", label, " "), gp = text_gp)
    width <- grid::convertWidth(grid::grobWidth(grob), "cm", valueOnly = TRUE)
    height <- grid::convertHeight(grid::grobHeight(grob), "cm", valueOnly = TRUE)

    if (axis == "x") {
      abs(cos(angle)) * width + abs(sin(angle)) * height
    } else {
      abs(sin(angle)) * width + abs(cos(angle)) * height
    }
  }, numeric(1), USE.NAMES = FALSE)
}
