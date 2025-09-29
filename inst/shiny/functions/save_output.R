#' Save aNCA Result to the Output Directory
#'
#' @param output Output object, can be a data frame, plot or a list of them.
#' @param output_path Path to the output directory (should exist or be creatable).
#' @returns Invisibly returns the file path written.
save_output <- function(output, output_path) {

  # Create output directory if it doesn't exist
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

  for (name in names(output)) {
    if (!dir.exists(paste0(output_path, "/", name))) {
      dir.create(paste0(output_path, "/", name), recursive = TRUE)
    }

    if (inherits(output[[name]], "list")) {

      save_output(output = output[[name]], output_path = paste0(output_path, "/", name))

    } else if (inherits(output[[name]], "ggplot")) {
      file_name <- paste0(output_path, "/", name, ".png")
      ggsave(file_name, plot = output[[name]], width = 10, height = 6)

    } else if (inherits(output[[name]], "data.frame")) {
      file_name <- paste0(output_path, "/", name)
      write.csv(output[[name]], file = paste0(file_name, ".csv"), row.names = FALSE)
      saveRDS(output[[name]], file = paste0(file_name, ".rds"))
      tryCatch(
        haven::write_xpt(format_to_xpt_compatible(output[[name]]), paste0(file_name, ".xpt")),
        error = function(e) {
          message("Error writing XPT file for ", name, ": ", e$message)
        }
      )
    } else if (inherits(output[[name]], "plotly")) {
      htmlwidgets::saveWidget(
        output[[name]],
        file = paste0(output_path, "/", name, ".html")
      )
    } else {
      stop(
        "Unsupported output type object in the list: ",
        paste0(class(output[[name]]), collapse = ", ")
      )
    }
  }
}

# Helper function to ensure the data.frame object is XPT compatible
format_to_xpt_compatible <- function(data) {

  # Columns should not have brackets or parenthesis
  names(data) <- make.unique(gsub(pattern = "\\[.*\\]", replacement = "", x = names(data)))
  names(data) <- gsub(pattern = "\\.", replacement = "", x = names(data))
  data
}

#' Append a Slide with Two Plots and a Table to an Existing Quarto Presentation
#'
#' @param quarto_path Path to the Quarto (.qmd) document.
#' @param plot1 A plotly object for the first plot.
#' @param plot2 A plotly object for the second plot.
#' @param df A data.frame to be shown as a table.
#'
#' @return Invisibly returns TRUE if the slide was appended.
add_slide_PlotTablePlot <- function(quarto_path, plot1, plot2, df, use_plotly = FALSE) {
  slide_content <- c(
    "\n---",
    "",
    "::: columns",
    "",
    "::: column",
    "```{r, echo=FALSE}",
    paste0(
      plot1,
      if (use_plotly) {" |> plotly::ggplotly()"} else {""}
    ),
    "flextable(", df, ")",
    "```",
    ":::",
    "",
    "::: column",
    "```{r, echo=FALSE}",
    paste0(
      plot2,
      if (use_plotly) {" |> plotly::ggplotly()"} else {""}
    ),
    "```",
    ":::",
    "",
    ":::",
    ""
  )
  write(slide_content, file = quarto_path, append = TRUE)
  invisible(TRUE)
}

add_slides_TablePlot_Plot <- function(quarto_path, plot1, plot2, df, use_plotly = FALSE) {
  slide_content <- c(
    "\n---",
    "",
    "::: columns",
    "",
    "::: column",
    "```{r, echo=FALSE}",
    "flextable(", df, ")",
    "```",
    ":::",
    "",
    "::: column",
    "```{r, echo=FALSE}",
    paste0(
      plot2,
      if (use_plotly) {" |> plotly::ggplotly()"} else {""}
    ),
    "```",
    ":::",
    "",
    ":::",
    "",
    "\n---",
    "```{r, echo=FALSE}",
    paste0(
      plot1,
      if (use_plotly) {" |> plotly::ggplotly()"} else {""}
    ),
    "```",
    ""
  )
  write(slide_content, file = quarto_path, append = TRUE)
  invisible(TRUE)
}

create_dose_slides_quarto <- function(res_dose_slides, quarto_path, title, rda_path, slide_fun = "add_slide_PlotTablePlot", template, extra_setup, use_plotly = FALSE){
  create_quarto_presentation(quarto_path, title, rda_path)
  slide_fun <- get(slide_fun)
  for (i in seq_len(length(res_dose_slides))) {
    slide_fun(
      quarto_path,
      plot1 = paste0("res_dose_slides[[", i, "]]$linplot"),
      plot2 = paste0("res_dose_slides[[", i, "]]$meanplot"),
      df = paste0("res_dose_slides[[", i, "]]$statistics"),
      use_plotly = use_plotly
    )
  }
}
# create_dose_slides_presentation(res_dose_slides = res_dose_slides, path = "test_dose_slides.pptx", title = "Dose Escalation Slides (Unnamed project)", template = NULL)
# create_dose_slides_presentation(res_dose_slides = res_dose_slides, path = "test_dose_slides.html", title = "Dose Escalation Slides (Unnamed project)", template = NULL)
create_dose_slides_presentation <- function(res_dose_slides, path, title, template, extra_setup){
  output_format <- tools::file_ext(path)
  quarto_path <- gsub(paste0("\\.", output_format), ".qmd", path)
  if (output_format == "html") output_format <- "revealjs"
  rda_obj_path <- paste0(dirname(quarto_path), "/all_outputs.rda")
  use_plotly <- if (output_format == "pptx") FALSE else TRUE
  slide_fun <- if (output_format == "pptx") "add_slides_TablePlot_Plot" else "add_slide_PlotTablePlot"
  save(list = as.character(quote(res_dose_slides)), file = rda_obj_path)
  create_dose_slides_quarto(res_dose_slides, quarto_path = quarto_path, title, slide_fun = slide_fun, rda_path = basename(rda_obj_path), use_plotly = use_plotly)
  quarto::quarto_render(input = quarto_path, output_format = output_format)
}

#' Create a New Quarto Presentation Document for Exporting Plots
#'
#' This function creates a new Quarto (.qmd) presentation file from scratch, writing the YAML header,
#' title, format, and an initial code chunk to load an RDS object (containing all plots/data).
#' Optionally, you can specify a template or add extra setup code.
#'
#' @param quarto_path Path to the Quarto (.qmd) file to create.
#' @param title Title for the presentation.
#' @param rda_path Path to the RDS file to be loaded in the document.
#' @param template (Optional) Path to a Quarto template to use (default: NULL).
#' @param extra_setup (Optional) Character vector of extra setup lines to include after YAML.
#' @return Invisibly returns TRUE if the file was created.
create_quarto_presentation <- function(quarto_path, title = "NCA Report", rda_path = NULL, template = NULL, extra_setup = NULL) {
  yaml_header <- c(
    "---",
    paste0("title: \"", title, "\""),
    "format: revealjs",
    if (!is.null(template)) paste0("template: ", template) else NULL,
    "execute:",
    "  echo: true",
    "  warning: false",
    "  message: false",
    "---",
    ""
  )
  load_chunk <- c(
    "```{r setup, include=FALSE}",
    if (!is.null(rda_path)) paste0("load(\"", rda_path, "\")") else "",
    "library(plotly)",
    "library(knitr)",
    "library(kableExtra)",
    "library(flextable)",
    "```",
    ""
  )
  content <- c(yaml_header, extra_setup, load_chunk)
  writeLines(content, con = quarto_path)
  invisible(TRUE)
}

linplot_meanplot_stats_by_group <- function(o_nca, group_by_vars, statistics = "Mean", facet_vars = "DOSEA", stats_parameters = c("CMAX", "TMAX", "VSS", "CLSTP")) {

  groups <- unique(o_nca$data$intervals[, group_by_vars])
  output_list <- list()
  # Loop over each of the groups
  for (i in seq_len(nrow(groups))) {
    group_i <- groups[i, , drop = FALSE]
    d_conc_i <- merge(o_nca$data$conc$data, group_i)
    o_res_i <- merge(o_nca$result, group_i)
    
    linplot_i <- general_lineplot(
      data = d_conc_i,
      selected_analytes = d_conc_i[["ANALYTE"]],
      selected_pcspec = d_conc_i[["PCSPEC"]],
      selected_usubjids = d_conc_i[["USUBJID"]],
      colorby_var = "USUBJID",
      facet_by = facet_vars,
      time_scale = "Whole",
      yaxis_scale = "Log",
      show_threshold = FALSE,
      threshold_value = 0,
      show_dose = FALSE,
      cycle = NULL,
      palette = NULL
    )
    meanplot_i <- general_meanplot(
      data = d_conc_i,
      selected_studyids = unique(d_conc_i[["STUDYID"]]),
      selected_analytes = unique(d_conc_i[["ANALYTE"]]),
      selected_pcspecs = unique(d_conc_i[["PCSPEC"]]),
      selected_cycles = unique(d_conc_i[["NCA_PROFILE"]]),
      id_variable = facet_vars,
      groupby_var = group_by_vars,
      plot_ylog = FALSE,
      plot_sd_min = TRUE,
      plot_sd_max = TRUE,
      plot_ci = FALSE
    )
    stats_i <- calculate_summary_stats(
      data = merge(o_res_i, d_conc_i[, c(group_vars(o_nca), "DOSEA")]),
      input_groups = facet_vars
    ) %>%
      filter(
        Statistic %in% statistics
      ) %>%
      select(
        any_of(c(facet_vars, "Statistic")),
        dplyr::matches(paste0("^(", paste(stats_parameters, collapse = "|"), ")(\\[.*\\])?$"))
      ) %>%
      unique()
    output_list[[paste0("Group_", i)]] <- list(
      linplot = linplot_i,
      meanplot = meanplot_i,
      statistics = stats_i
    )
  }
  output_list
}
