#' NEST pilot: linear mean concentration-time graph
#'
#' Reuses [g_pkcg03_lin()] without changing its estimators, BLQ eligibility
#' rule or summary exclusions. Adds a persistent methods footnote, including
#' when the user supplies a custom footnote. The footnote states the BLQ rule
#' only when a BLQ flag (`AVALC`/`AVALCAT1`) exists, since the rule cannot apply
#' otherwise. The default remains mean +/- SD.
#'
#' @param data ADNCA concentration data.
#' @param footnote Additional annotation text supporting `$VAR` and `!VAR`.
#' @param plotgroup_vars Columns used to split the graphs.
#' @param ... Arguments passed to [g_pkcg03_lin()], including grouping,
#'   axes, summary method and whisker selection.
#' @returns Named list of plotly/ggplot objects, or an explanatory string
#'   when no timepoint meets the existing eligibility rules.
#' @importFrom dplyr as_tibble
#' @importFrom ggplot2 labs
#' @importFrom plotly plotly_build
#' @export
g_pkcg03_lin_nest <- function(data, footnote = NULL,
                              plotgroup_vars = c("ROUTE", "PCSPEC", "PARAM"), ...) {
  # The existing BLQ timepoint rule only applies when a BLQ flag column exists.
  blq_rule <- if (length(intersect(c("AVALC", "AVALCAT1"), names(data)))) {
    "Timepoints require >1 subject and <=50% BLQ."
  } else {
    "Timepoints require >1 subject; no BLQ flag, so no BLQ rule."
  }
  method_note <- c(
    "Summary-excluded records are omitted.",
    blq_rule,
    "AVAL includes upstream imputation; no new imputation."
  )
  if (!nrow(data)) return(list("No data available for the NEST graph."))
  data <- as_tibble(data)
  split_and_apply(data, plotgroup_vars, function(df) {
    plots <- g_pkcg03_lin(
      df, plotgroup_vars = plotgroup_vars,
      footnote = paste(c(footnote, method_note), collapse = "\n"), ...
    )
    if (!length(plots)) return("No timepoints meet the NEST graph reporting criteria.")
    source <- if (inherits(plots[[1]], "ggplot")) plots[[1]] else attr(plots[[1]], "ggplot")
    if (!nrow(source$data)) return("No nonmissing concentrations remain for this NEST graph.")
    .nest_graph_notes(plots[[1]], source)
  })
}

# Keep the methods text below the axis title and legend in both preview and export.
.nest_graph_notes <- function(plot, source) {
  caption <- gsub("<br>", "\n", source$labels$caption, fixed = TRUE)
  lines <- unlist(lapply(strsplit(caption, "\n")[[1]], strwrap, width = 65))
  source <- source + labs(caption = paste(lines, collapse = "\n"))
  if (!inherits(plot, "plotly")) return(source)
  # Build once so the inherited methods annotation is replaced, not appended.
  plot <- plotly_build(plot)
  plot$height <- plot$x$layout$height <- 600 + 14 * length(lines)
  plot$x$layout$margin$b <- 120 + 14 * length(lines)
  plot$x$layout$legend$orientation <- "h"
  plot$x$layout$legend$y <- -0.14
  plot$x$layout$legend$yanchor <- "top"
  plot$x$layout$annotations <- list(list(
    x = 0, y = 0, yshift = -85, text = paste(lines, collapse = "<br>"),
    xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
    showarrow = FALSE, align = "left", font = list(size = 11)
  ))
  .with_ggplot(plot, source)
}
