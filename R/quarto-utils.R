
## Create a new Quarto (.qmd) presentation file with YAML header and setup chunk
#' Create a new Quarto presentation file with YAML header and setup chunk
#'
#' Used internally to initialize a Quarto document for exporting plots and tables.
#'
#' @param quarto_path Path to the Quarto (.qmd) file to create.
#' @param title Title for the presentation.
#' @param libraries Character vector of libraries to load in setup chunk.
#' @param rda_path Path to the RDS file to be loaded in the document.
#' @param template (Optional) Path to a Quarto template to use (default: NULL).
#' @param extra_setup (Optional) Character vector of extra setup lines to include after YAML.
#' @returns Invisibly returns TRUE if the file was created.
create_qmd_doc <- function(
  quarto_path,
  title = "NCA Report",
  libraries = c("plotly", "flextable", "dplyr"),
  rda_path = NULL,
  template = NULL,
  extra_setup = NULL
) {
  yaml_header <- c(
    "---",
    paste0("title: \"", gsub("[\r\n]+", " ", title), "\""),
    "format:",
    "  revealjs:",
    "    toc: true",
    "    toc-depth: 1",
    "    theme: default",
    "    scrollable: true",
    "    smaller: true",
    "execute:",
    "  echo: false",
    "  warning: false",
    "  message: false",
    "---",
    ""
  )
  load_chunk <- c(
    "```{r setup, include=FALSE}",
    if (!is.null(rda_path)) paste0("load(\"", rda_path, "\")") else "",
    paste0("library(", libraries, ")"),
    "```",
    ""
  )
  content <- c(yaml_header, extra_setup, load_chunk)
  writeLines(content, con = quarto_path)
  invisible(TRUE)
}

#' Add a slide to the Quarto document with a plot and two tables (side by side)
#'
#' Used internally for dose escalation reporting.
#'
#' @param quarto_path Path to the Quarto (.qmd) file to append to.
#' @param df1 Expression for first table (left column).
#' @param df2 Expression for second table (right column).
#' @param plot Expression for plot.
#' @param use_plotly Logical, whether to convert plot to plotly.
#' @returns Invisibly returns TRUE if the slide was added.
add_qmd_sl_plottabletable <- function(quarto_path, df1, df2, plot, use_plotly = FALSE) {
  has_tables <- !is.null(df1) || !is.null(df2)
  slide_content <- c(
    "\n---",
    if (!is.null(plot)) add_qmd_plot(plot, use_plotly),
    if (has_tables) c(
      "::: columns",
      "",
      if (!is.null(df1)) c("::: column", add_qmd_table(df1), ":::", ""),
      if (!is.null(df2)) c("::: column", add_qmd_table(df2), ":::", ""),
      ":::",
      ""
    )
  )
  write(slide_content, file = quarto_path, append = TRUE)
  invisible(TRUE)
}

#' Add a slide to the Quarto document with a single plot
#'
#' Used internally for dose escalation reporting.
#'
#' @param quarto_path Path to the Quarto (.qmd) file to append to.
#' @param plot Expression for plot.
#' @param use_plotly Logical, whether to convert plot to plotly.
#' @returns Invisibly returns TRUE if the slide was added.
add_qmd_sl_plot <- function(quarto_path, plot, use_plotly = FALSE) {
  slide_content <- c(
    "\n---",
    "",
    add_qmd_plot(plot, use_plotly),
    ""
  )
  write(slide_content, file = quarto_path, append = TRUE)
  invisible(TRUE)
}

#' Write a section header slide with the group info table to a qmd file
#' @param quarto_path Path to the Quarto (.qmd) file to append to.
#' @param res_dose_slides List of results for each dose group.
#' @param i Integer index of the dose group.
#' @param label Character string used as the # heading text.
#' @keywords internal
#' @noRd
.add_qmd_group_section_header <- function(quarto_path, res_dose_slides, i, label) {
  write(
    c(
      paste0("\n# ", label),
      "",
      add_qmd_table(paste0("res_dose_slides[[", i, "]]$info")),
      ""
    ),
    file = quarto_path, append = TRUE
  )
}

#' Append boxplot slides for one dose group to a qmd file
#' @param quarto_path Path to the Quarto (.qmd) file to append to.
#' @param boxplots_i Named list of boxplot objects for group i.
#' @param i Integer index of the dose group.
#' @param use_plotly Logical, whether to convert plots to plotly.
#' @keywords internal
#' @noRd
.add_qmd_boxplot_slides <- function(quarto_path, boxplots_i, i, use_plotly) {
  for (bp_name in names(boxplots_i)) {
    if (!is.null(boxplots_i[[bp_name]])) {
      add_qmd_sl_plot(
        quarto_path,
        paste0("res_dose_slides[[", i, "]]$boxplot$", bp_name),
        use_plotly
      )
    }
  }
}

#' Append meanplot/statistics/linplot/boxplot slides for all dose groups to a qmd file
#' @param quarto_path Path to the Quarto (.qmd) file to append to.
#' @param res_dose_slides List of results for each dose group.
#' @param in_sections Function(id) returning TRUE when the section id is selected.
#' @param use_plotly Logical, whether to convert plots to plotly.
#' @keywords internal
#' @noRd
.add_qmd_summary_slides <- function(quarto_path, res_dose_slides, in_sections, use_plotly) {
  for (i in seq_along(res_dose_slides)) {
    .add_qmd_group_section_header(quarto_path, res_dose_slides, i, paste0("Group ", i))
    if (in_sections("meanplot") || in_sections("statistics")) {
      add_qmd_sl_plottabletable(
        quarto_path = quarto_path,
        df1 = if (in_sections("statistics")) {
          paste0("res_dose_slides[[", i, "]]$statistics")
        } else {
          NULL
        },
        df2 = NULL,
        plot = if (in_sections("meanplot")) {
          paste0("res_dose_slides[[", i, "]]$meanplot")
        } else {
          NULL
        },
        use_plotly = use_plotly
      )
    }
    if (in_sections("linplot")) {
      add_qmd_sl_plot(quarto_path = quarto_path,
                      plot = paste0("res_dose_slides[[", i, "]]$linplot"),
                      use_plotly = use_plotly)
    }
    boxplots_i <- res_dose_slides[[i]]$boxplot
    if (in_sections("boxplot") && is.list(boxplots_i)) {
      .add_qmd_boxplot_slides(quarto_path, boxplots_i, i, use_plotly)
    }
  }
}

#' Append individual-subject slides for all dose groups to a qmd file
#' @param quarto_path Path to the Quarto (.qmd) file to append to.
#' @param res_dose_slides List of results for each dose group.
#' @param in_sections Function(id) returning TRUE when the section id is selected.
#' @param use_plotly Logical, whether to convert plots to plotly.
#' @keywords internal
#' @noRd
.add_qmd_ind_slides <- function(quarto_path, res_dose_slides, in_sections, use_plotly) {
  for (i in seq_along(res_dose_slides)) {
    if (length(res_dose_slides[[i]]$ind_params) == 0 &&
          length(res_dose_slides[[i]]$ind_plots) == 0) {
      next
    }
    .add_qmd_group_section_header(
      quarto_path, res_dose_slides, i, paste0("Group ", i, " (Individual)")
    )
    for (subj in names(res_dose_slides[[i]]$ind_params)) {
      add_qmd_sl_plottabletable(
        quarto_path = quarto_path,
        df1 = if (in_sections("ind_params")) {
          paste0("res_dose_slides[[", i, "]]$ind_params[['", subj, "']]")
        } else {
          NULL
        },
        df2 = NULL,
        plot = if (in_sections("ind_plots")) {
          paste0("res_dose_slides[[", i, "]]$ind_plots[['", subj, "']]")
        } else {
          NULL
        },
        use_plotly = use_plotly
      )
    }
  }
}

#' Append additional analysis slides to a qmd file
#' @param quarto_path Path to the Quarto (.qmd) file to append to.
#' @param additional_analysis Named list of data frames.
#' @param slide_sections Character vector of selected section IDs, or NULL for all.
#' @keywords internal
#' @noRd
.add_qmd_additional_analysis <- function(quarto_path, additional_analysis, slide_sections) {
  if (is.null(additional_analysis)) return(invisible(NULL))
  keep <- vapply(additional_analysis, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))
  analysis_to_show <- additional_analysis[keep]
  if (!is.null(slide_sections)) {
    analysis_to_show <- analysis_to_show[names(analysis_to_show) %in% slide_sections]
  }
  if (length(analysis_to_show) == 0) return(invisible(NULL))
  write("\n# Additional Analysis Figures", file = quarto_path, append = TRUE)
  for (name in names(analysis_to_show)) {
    slide_title <- tools::toTitleCase(gsub("_", " ", name))
    write(
      c("\n---", "", paste0("## ", slide_title),
        add_qmd_table(paste0("additional_analysis[['", name, "']]")), ""),
      file = quarto_path,
      append = TRUE
    )
  }
}

#' Create all slides for dose escalation results in a Quarto document
#'
#' Used internally to generate main and individual slides for each dose group.
#'
#' @param res_dose_slides List of results for each dose group.
#' @param quarto_path Path to the Quarto (.qmd) file to create.
#' @param title Title for the presentation.
#' @param use_plotly Logical, whether to convert plots to plotly.
#' @returns Invisibly returns TRUE if slides were created.
create_qmd_dose_slides <- function(res_dose_slides, quarto_path, title, use_plotly = TRUE) {
  # Read optional filtering attributes
  slide_sections <- attr(res_dose_slides, "slide_sections")
  additional_analysis <- attr(res_dose_slides, "additional_analysis")

  # Helper: TRUE when id is selected (NULL slide_sections means all selected)
  in_sections <- function(id) is.null(slide_sections) || id %in% slide_sections

  # Save accessible objects with all results
  rda_path <- paste0(dirname(quarto_path), "/results_slides_outputs.rda")
  save(list = c("res_dose_slides", "additional_analysis"), file = rda_path)

  # Generate the main quarto document
  create_qmd_doc(quarto_path = quarto_path, title = title, rda_path = basename(rda_path))

  # Mean plot + statistics block
  has_summary <- in_sections("meanplot") || in_sections("statistics") ||
    in_sections("linplot") || in_sections("boxplot")
  has_individual <- in_sections("ind_plots") || in_sections("ind_params")

  if (has_summary) .add_qmd_summary_slides(quarto_path, res_dose_slides, in_sections, use_plotly)
  if (has_summary && has_individual) write("\n# Extra Figures", file = quarto_path, append = TRUE)
  if (has_individual) .add_qmd_ind_slides(quarto_path, res_dose_slides, in_sections, use_plotly)

  # Additional analysis section
  .add_qmd_additional_analysis(quarto_path, additional_analysis, slide_sections)
}

#' Render dose escalation results to HTML via Quarto
#'
#' Used internally to create and render a .qmd file to HTML.
#'
#' @param res_dose_slides List of results for each dose group.
#' @param path Path to the output HTML file.
#' @param title Title for the presentation.
#' @returns Invisibly returns TRUE if rendering succeeded.
create_html_dose_slides <- function(res_dose_slides, path, title) {
  output_format <- tools::file_ext(path)
  quarto_path <- gsub(paste0("\\.", output_format), ".qmd", path)
  use_plotly <- if (output_format == "html") TRUE else FALSE
  output_format <- if (output_format == "html") "all" else output_format

  create_qmd_dose_slides(
    res_dose_slides,
    quarto_path = quarto_path,
    title = title,
    use_plotly = use_plotly
  )
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop(
      "HTML slides draft requires `quarto` package, please install it with ",
      "`install.packages('quarto')`"
    )
  }
  quarto::quarto_render(input = quarto_path, output_format = output_format)
}

#' Helper to create a Quarto code chunk for a plot
#'
#' Used internally to format a plot chunk for Quarto documents.
#'
#' @param plot_expr Expression for plot.
#' @param use_plotly Logical, whether to convert plot to plotly.
#' @returns Character vector for Quarto code chunk.
add_qmd_plot <- function(plot_expr, use_plotly = FALSE) {
  c(
    "```{r, echo=FALSE}",
    paste0(
      plot_expr,
      if (use_plotly) " %>% plotly::ggplotly()" else ""
    ),
    "```"
  )
}

#' Helper to create a Quarto code chunk for a table
#'
#' Used internally to format a table chunk for Quarto documents.
#'
#' @param table_expr Expression for table.
#' @returns Character vector for Quarto code chunk.
add_qmd_table <- function(table_expr) {
  c(
    "```{r, echo=FALSE}",
    paste0("flextable(", table_expr, ")"),
    "```"
  )
}
