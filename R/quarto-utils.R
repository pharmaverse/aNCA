
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
  libraries = c("plotly", "flextable"),
  rda_path = NULL,
  template = NULL,
  extra_setup = NULL
) {
  yaml_header <- c(
    "---",
    paste0("title: \"", title, "\""),
    "format: revealjs",
    if (!is.null(template)) paste0("reference-doc: ", template) else NULL,
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
  slide_content <- c(
    "\n---",
    add_qmd_plot(plot, use_plotly),
    "::: columns",
    "",
    "::: column",
    add_qmd_table(df1),
    ":::",
    "",
    "::: column",
    add_qmd_table(df2),
    ":::",
    "",
    ":::",
    ""
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
  # Save an accessible object with all results
  rda_path <- paste0(dirname(quarto_path), "/dose_escalation_outputs.rda")
  save(list = as.character(quote(res_dose_slides)), file = rda_path)

  # Generate the main quarto document
  create_qmd_doc(quarto_path = quarto_path, title = title, rda_path = basename(rda_path))
  for (i in seq_len(length(res_dose_slides))) {
    add_qmd_sl_plottabletable(
      quarto_path = quarto_path,
      df1 = paste0("res_dose_slides[[", i, "]]$info"),
      df2 = paste0("res_dose_slides[[", i, "]]$statistics"),
      plot = paste0("res_dose_slides[[", i, "]]$meanplot"),
      use_plotly = use_plotly
    )
    add_qmd_sl_plot(
      quarto_path = quarto_path,
      plot = paste0("res_dose_slides[[", i, "]]$linplot"),
      use_plotly = use_plotly
    )
    add_qmd_sl_plot(
      quarto_path = quarto_path,
      plot = paste0("res_dose_slides[[", i, "]]$boxplot"),
      use_plotly = use_plotly
    )
  }

  # Add the individual information slides
  for (i in seq_len(length(res_dose_slides))) {
    for (subj in names(res_dose_slides[[i]]$ind_params)) {
      add_qmd_sl_plottabletable(
        quarto_path = quarto_path,
        df1 = paste0("res_dose_slides[[", i, "]]$info"),
        df2 = paste0("res_dose_slides[[", i, "]]$ind_params[['", subj, "']]"),
        plot = paste0("res_dose_slides[[", i, "]]$ind_plots[['", subj, "']]"),
        use_plotly = use_plotly
      )
    }
  }
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
  if (requireNamespace("quarto", quietly = TRUE)) {
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
      if (use_plotly) " |> plotly::ggplotly()" else ""
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
