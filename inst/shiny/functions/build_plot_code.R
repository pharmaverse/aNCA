#' Build reproducible R code string for an exploration plot
#'
#' Assembles a self-contained R script that loads the input dataset,
#' applies mapping and filters, creates the PKNCAdata object, then
#' generates the exploration plot and exports it as PNG and interactive HTML.
#' Follows the same pattern as script_template.R.
#'
#' @param plot_type Character; either "individual" or "mean".
#' @param inputs List of current sidebar input values.
#' @param session Shiny session object (provides mapping, filters, etc.).
#'
#' @return A single character string of R code.
build_plot_code <- function(plot_type, inputs, session) {
  # Format helpers: quote R objects for code output
  fmt_chr <- function(x) {
    if (is.null(x) || length(x) == 0) return("NULL")
    if (length(x) == 1) return(paste0('"', x, '"'))
    paste0('c(', paste0('"', x, '"', collapse = ", "), ')')
  }

  fmt_lgl <- function(x) {
    if (is.null(x) || !is.logical(x)) return("FALSE")
    if (x) "TRUE" else "FALSE"
  }

  fmt_num <- function(x) {
    if (is.null(x) || length(x) == 0 || all(!is.finite(x))) return("NULL")
    if (length(x) == 1) return(as.character(x))
    paste0("c(", paste(x, collapse = ", "), ")")
  }

  # Serialize an R object to reproducible R code
  fmt_obj <- function(x) {
    if (is.null(x)) return("NULL")
    paste(deparse(x, width.cutoff = 500), collapse = "\n")
  }

  # --- Extract session data ---
  mapping <- session$userData$mapping
  applied_filters <- session$userData$applied_filters
  time_duplicate_rows <- session$userData$time_duplicate_rows

  # --- Data loading ---
  dataset_filename <- session$userData$dataset_filename %||% "input_data.csv"
  data_section <- paste0(
    '# Load raw data\n',
    'data_path <- "', dataset_filename, '"\n',
    'adnca_data <- aNCA::read_pk(data_path)\n'
  )

  # --- Mapping ---
  mapping_code <- paste0("mapping <- ", fmt_obj(mapping))

  # --- Filters ---
  if (!is.null(applied_filters) && length(applied_filters) > 0) {
    filters_code <- paste0("applied_filters <- ", fmt_obj(applied_filters))
  } else {
    filters_code <- "applied_filters <- NULL"
  }

  # --- Time duplicate rows ---
  if (!is.null(time_duplicate_rows) && length(time_duplicate_rows) > 0) {
    time_dup_code <- paste0(
      "time_duplicate_rows <- ", fmt_obj(time_duplicate_rows)
    )
  } else {
    time_dup_code <- "time_duplicate_rows <- NULL"
  }

  # --- PKNCA object creation ---
  pknca_section <- paste0(
    'pknca_data <- adnca_data %>%\n',
    '  aNCA::PKNCA_create_data_object(\n',
    '    mapping = mapping,\n',
    '    applied_filters = applied_filters,\n',
    '    time_duplicate_rows = time_duplicate_rows\n',
    '  )\n'
  )

  # --- Plot filtering_list ---
  fl <- inputs$filtering_list
  if (!is.null(fl) && length(fl) > 0) {
    filter_lines <- vapply(names(fl), function(col) {
      vals <- fmt_chr(fl[[col]])
      paste0('  ', col, ' = ', vals)
    }, character(1))
    plot_filter_code <- paste0(
      "filtering_list <- list(\n",
      paste(filter_lines, collapse = ",\n"),
      "\n)"
    )
  } else {
    plot_filter_code <- "filtering_list <- NULL"
  }

  # --- Plot function call ---
  threshold_code <- fmt_num(inputs$threshold_value)

  if (plot_type == "individual") {
    plot_call <- paste0(
      'p <- aNCA::exploration_individualplot(\n',
      '  pknca_data = pknca_data,\n',
      '  color_by = ', fmt_chr(inputs$color_by), ',\n',
      '  facet_by = ', fmt_chr(inputs$facet_by), ',\n',
      '  show_facet_n = ', fmt_lgl(inputs$show_facet_n), ',\n',
      '  ylog_scale = ', fmt_lgl(inputs$ylog_scale), ',\n',
      '  show_legend = ', fmt_lgl(inputs$show_legend), ',\n',
      '  show_dose = ', fmt_lgl(inputs$show_dose), ',\n',
      '  threshold_value = ', threshold_code, ',\n',
      '  x_limits = ', fmt_num(inputs$x_limits), ',\n',
      '  y_limits = ', fmt_num(inputs$y_limits), ',\n',
      '  filtering_list = filtering_list,\n',
      '  labels_df = aNCA::metadata_nca_variables,\n',
      '  use_time_since_last_dose = ',
      fmt_lgl(inputs$use_time_since_last_dose), ',\n',
      '  palette = ', fmt_chr(inputs$palette), ',\n',
      '  line_type = ', fmt_chr(inputs$y_axis_values), '\n',
      ')'
    )
  } else {
    plot_call <- paste0(
      'p <- aNCA::exploration_meanplot(\n',
      '  pknca_data = pknca_data,\n',
      '  color_by = ', fmt_chr(inputs$color_by), ',\n',
      '  facet_by = ', fmt_chr(inputs$facet_by), ',\n',
      '  show_facet_n = ', fmt_lgl(inputs$show_facet_n), ',\n',
      '  ylog_scale = ', fmt_lgl(inputs$ylog_scale), ',\n',
      '  show_legend = ', fmt_lgl(inputs$show_legend), ',\n',
      '  show_dose = ', fmt_lgl(inputs$show_dose), ',\n',
      '  threshold_value = ', threshold_code, ',\n',
      '  x_limits = ', fmt_num(inputs$x_limits), ',\n',
      '  y_limits = ', fmt_num(inputs$y_limits), ',\n',
      '  sd_min = ', fmt_lgl(inputs$sd_min), ',\n',
      '  sd_max = ', fmt_lgl(inputs$sd_max), ',\n',
      '  ci = ', fmt_lgl(inputs$ci), ',\n',
      '  filtering_list = filtering_list,\n',
      '  labels_df = aNCA::metadata_nca_variables,\n',
      '  use_time_since_last_dose = ',
      fmt_lgl(inputs$use_time_since_last_dose), ',\n',
      '  palette = ', fmt_chr(inputs$palette), ',\n',
      '  line_type = ', fmt_chr(inputs$y_axis_values), '\n',
      ')'
    )
  }

  # --- Assemble full script ---
  paste0(
    '# Reproducible plot code generated by aNCA\n',
    '# Requires: aNCA, dplyr, ggplot2, plotly, htmlwidgets\n',
    '\n',
    'library(aNCA)\n',
    'library(dplyr)\n',
    '\n',
    data_section,
    '\n',
    '## Preprocessing & PKNCA object creation\n',
    mapping_code, '\n',
    '\n',
    filters_code, '\n',
    '\n',
    time_dup_code, '\n',
    '\n',
    pknca_section,
    '\n',
    '## Plot filtering\n',
    plot_filter_code, '\n',
    '\n',
    '## Generate plot\n',
    plot_call, '\n',
    '\n',
    '# Export as PNG\n',
    'ggplot2::ggsave("plot.png", plot = p, width = 10, height = 6, dpi = 300)\n',
    '\n',
    '# Export as interactive HTML\n',
    'p_interactive <- plotly::ggplotly(p, tooltip = "tooltip_text")\n',
    'htmlwidgets::saveWidget(p_interactive, "plot.html", selfcontained = TRUE)\n'
  )
}
