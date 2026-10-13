#' Convert a ggplot TLG to Plotly without losing its text labels.
#'
#' `ggplotly()` drops subtitles and captions. Add all three labels explicitly
#' as annotations, preserving existing facet and axis annotations. Reserve extra
#' height and margins so multiline labels do not reduce the plotting area.
#'
#' @param item A ggplot, Plotly widget or character error message.
#' @returns A labeled Plotly widget, or unchanged input without a source ggplot.
#' @noRd
.tlg_ggplotly <- function(item) {
  source <- if (inherits(item, "ggplot")) item else attr(item, "ggplot")
  if (!inherits(source, "ggplot")) return(item)
  labels <- .tlg_plot_labels(source)
  line_counts <- vapply(labels, function(text) {
    if (!nzchar(text)) return(0L)
    length(strsplit(text, "\n", fixed = TRUE)[[1]])
  }, integer(1))
  label_heights <- line_counts * c(24, 17, 16)
  extra_top <- if (any(line_counts[1:2] > 0)) sum(label_heights[1:2]) + 12 else 0
  extra_bottom <- if (line_counts[[3]] > 0) label_heights[[3]] + 12 else 0

  plot <- source + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
  bare <- plotly::plotly_build(plotly::ggplotly(plot, height = 500))
  widget <- .tlg_annotation_widget(item, bare, source$labels$caption)
  margins <- bare$x$layout$margin
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
      name = paste0("tlg-", c("title", "subtitle", "caption")[[i]]),
      text = gsub("\n", "<br>", htmltools::htmlEscape(labels[[i]]), fixed = TRUE),
      x = 0, y = if (i == 3L) 0 else 1,
      xref = "paper", yref = "paper", xanchor = "left",
      yanchor = if (i == 3L) "top" else "bottom", yshift = shifts[[i]],
      align = "left", showarrow = FALSE,
      font = list(size = c(18, 12, 11)[[i]], color = "black")
    )
  })
  resize_labels <- list(
    text = unname(lapply(c("title", "subtitle", "caption"), function(key) {
      as.character(source$labels[[key]] %||% "")
    })),
    top = margins$t, bottom = margins$b, captionOffset = caption_offset
  )
  margins$t <- margins$t + extra_top
  margins$b <- margins$b + extra_bottom
  widget$height <- 500 + extra_top + extra_bottom
  widget$x$layout$height <- widget$height
  widget$x$layout$margin <- margins
  widget$x$layout$annotations <- c(widget$x$layout$annotations, label_annotations)
  htmlwidgets::onRender(widget, paste(readLines(
    system.file("shiny/www/tlg_plot_labels.js", package = "aNCA"), warn = FALSE
  ), collapse = "\n"), data = resize_labels)
}

#' Keep supported text labels from a settings payload, keyed by catalog name.
#'
#' Unknown catalog entries/options and malformed values cannot replace defaults.
#' Empty strings and annotation tokens are kept verbatim. Older settings omit this key.
#' @param saved Named list from the top-level `tlg_labels` settings key.
#' @param definitions Current catalog definitions.
#' @returns A named list of valid text labels.
#' @noRd
.normalize_tlg_labels <- function(saved, definitions) {
  if (is.null(saved) || identical(saved, list())) return(list())
  if (!.tlg_label_map_valid(saved)) {
    warning("Ignored invalid TLG labels: expected unique catalog names.", call. = FALSE)
    return(list())
  }
  keys <- intersect(names(saved), names(definitions))
  if (length(keys) < length(saved)) {
    warning("Ignored unknown or invalid TLG labels in settings.", call. = FALSE)
  }
  labels <- lapply(keys, function(key) {
    .normalize_tlg_label_entry(saved[[key]], definitions[[key]]$options)
  })
  labels <- setNames(labels, keys)
  labels[lengths(labels) > 0L]
}

#' @noRd
.tlg_label_map_valid <- function(x) {
  is.list(x) && !is.null(names(x)) && !anyDuplicated(names(x)) && all(nzchar(names(x)))
}

#' @noRd
.normalize_tlg_label_entry <- function(entry, options) {
  if (!.tlg_label_map_valid(entry)) {
    warning("Ignored unknown or invalid TLG labels in settings.", call. = FALSE)
    return(NULL)
  }
  text_ids <- names(options)[vapply(options, function(def) {
    is.list(def) && identical(def$type, "text")
  }, logical(1))]
  valid <- vapply(entry, function(value) {
    is.character(value) && length(value) == 1L && !is.na(value)
  }, logical(1)) & names(entry) %in% text_ids
  if (any(!valid)) warning("Ignored unknown or invalid TLG labels in settings.", call. = FALSE)
  entry[valid]
}

#' Collect raw text from registered modules, retaining unopened imported labels.
#' @noRd
.collect_tlg_labels <- function(registry, imported) {
  for (key in ls(envir = registry)) {
    imported[[key]] <- get(key, envir = registry)$labels()
  }
  imported
}

#' Wrap annotation text for a fixed-size export or initial widget layout.
#' @noRd
.tlg_plot_labels <- function(item) {
  vapply(c("title", "subtitle", "caption"), function(key) {
    text <- item$labels[[key]]
    if (is.null(text)) return("")
    lines <- strsplit(as.character(text), "\n", fixed = TRUE)[[1]]
    width <- if (key == "title") 60 else 80
    paste(vapply(lines, function(line) {
      paste(strwrap(line, width = width), collapse = "\n")
    }, character(1)), collapse = "\n")
  }, character(1))
}


#' Replace concentration annotations without rebuilding their interactive traces.
#' @noRd
.tlg_annotation_widget <- function(item, bare, caption) {
  if (!inherits(item, "plotly")) return(bare)
  widget <- plotly::plotly_build(item)
  # The source ggplot has the same facets and labels, but no legacy title spacing.
  # Keep native log axes, filtered ticks, hover text, symbols and all trace values.
  widget$x$layout$title <- list(text = "")
  normalize <- function(text) gsub("<br\\s*/?>", "\n", text %||% "")
  widget$x$layout$annotations <- Filter(function(annotation) {
    !(identical(normalize(annotation$text), normalize(caption)) &&
        identical(annotation$yref, "paper") && isTRUE(annotation$y <= 0))
  }, widget$x$layout$annotations)
  widget$x$layout$legend <- bare$x$layout$legend
  attr(widget, "ggplot") <- attr(item, "ggplot")
  attr(widget, "tlg_group_header") <- attr(item, "tlg_group_header")
  widget
}
