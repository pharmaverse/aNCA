#' Convert a ggplot TLG to Plotly without losing its text labels.
#'
#' `ggplotly()` drops subtitles and captions. Add all three labels explicitly
#' as annotations, preserving existing facet and axis annotations. Reserve extra
#' height and margins so multiline labels do not reduce the plotting area.
#'
#' @param item A ggplot, Plotly widget or character error message.
#' @returns A Plotly widget, or the unchanged non-ggplot input.
#' @noRd
.tlg_ggplotly <- function(item) {
  if (!inherits(item, "ggplot")) return(item)
  labels <- vapply(c("title", "subtitle", "caption"), function(key) {
    text <- item$labels[[key]]
    if (is.null(text)) return("")
    lines <- strsplit(as.character(text), "\n", fixed = TRUE)[[1]]
    width <- if (key == "title") 60 else 80
    paste(vapply(lines, function(line) {
      paste(strwrap(line, width = width), collapse = "\n")
    }, character(1)), collapse = "\n")
  }, character(1))
  line_counts <- vapply(labels, function(text) {
    if (!nzchar(text)) return(0L)
    length(strsplit(text, "\n", fixed = TRUE)[[1]])
  }, integer(1))
  label_heights <- line_counts * c(24, 17, 16)
  extra_top <- if (any(line_counts[1:2] > 0)) sum(label_heights[1:2]) + 12 else 0
  extra_bottom <- if (line_counts[[3]] > 0) label_heights[[3]] + 12 else 0

  plot <- item + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
  widget <- plotly::ggplotly(plot, height = 500)
  margins <- widget$x$layout$margin
  caption_offset <- margins$b
  if (nzchar(labels[["caption"]])) {
    # Plotly's initial margin omits rotated tick-label and legend heights.
    # Measure the rows below the last ggplot panel before placing the footnote.
    grob <- ggplot2::ggplotGrob(plot)
    last_panel <- max(grob$layout$b[grepl("^panel", grob$layout$name)])
    bottom_height <- sum(grob$heights[seq.int(last_panel + 1L, length(grob$heights))])
    bottom_px <- grid::convertHeight(bottom_height, "inches", valueOnly = TRUE) * 96
    caption_offset <- max(margins$b, bottom_px)
    extra_bottom <- extra_bottom + caption_offset - margins$b
  }
  shifts <- c(margins$t + label_heights[[2]] + 6, margins$t, -caption_offset - 6)
  label_annotations <- lapply(which(nzchar(labels)), function(i) {
    list(
      text = gsub("\n", "<br>", htmltools::htmlEscape(labels[[i]]), fixed = TRUE),
      x = 0, y = if (i == 3L) 0 else 1,
      xref = "paper", yref = "paper", xanchor = "left",
      yanchor = if (i == 3L) "top" else "bottom", yshift = shifts[[i]],
      align = "left", showarrow = FALSE,
      font = list(size = c(18, 12, 11)[[i]], color = "black")
    )
  })
  margins$t <- margins$t + extra_top
  margins$b <- margins$b + extra_bottom
  widget$height <- 500 + extra_top + extra_bottom
  widget$x$layout$height <- widget$height
  plotly::layout(
    widget, margin = margins,
    annotations = c(widget$x$layout$annotations, label_annotations)
  )
}
