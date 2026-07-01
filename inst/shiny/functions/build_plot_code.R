#' Build reproducible R code string for an exploration plot
#'
#' Assembles a self-contained R script that loads the input dataset,
#' applies mapping and filters, creates the PKNCAdata object, then
#' generates the exploration plot and exports it as PNG and interactive HTML.
#' Follows the same pattern as script_template.R.
#'
#' @param plot_type Character; one of "individual", "mean", or "qc".
#' @param inputs List of current sidebar input values.
#' @param session Shiny session object (provides mapping, filters, etc.).
#'
#' @returns A single character string of R code.
build_plot_code <- function(plot_type, inputs, session) {
  # --- Extract session data ---
  mapping <- session$userData$mapping
  applied_filters <- session$userData$applied_filters
  time_duplicate_rows <- session$userData$time_duplicate_rows

  # --- Data loading ---
  dataset_filename <- if (!is.null(session$userData$dataset_filename)) {
    session$userData$dataset_filename
  } else {
    "input_data.csv"
  }
  data_section <- paste0(
    "# Load raw data\n",
    "data_path <- \"", dataset_filename, "\"\n",
    "adnca_data <- aNCA::read_pk(data_path)\n"
  )

  # --- Mapping ---
  mapping_code <- paste0("mapping <- ", .fmt_obj(mapping))

  # --- Filters ---
  if (!is.null(applied_filters) && length(applied_filters) > 0) {
    filters_code <- paste0("applied_filters <- ", .fmt_obj(applied_filters))
  } else {
    filters_code <- "applied_filters <- NULL"
  }

  # --- Time duplicate rows ---
  if (!is.null(time_duplicate_rows) && length(time_duplicate_rows) > 0) {
    time_dup_code <- paste0(
      "time_duplicate_rows <- ", .fmt_obj(time_duplicate_rows)
    )
  } else {
    time_dup_code <- "time_duplicate_rows <- NULL"
  }

  # --- PKNCA object creation ---
  pknca_section <- paste0(
    "pknca_data <- adnca_data %>%\n",
    "  aNCA::PKNCA_create_data_object(\n",
    "    mapping = mapping,\n",
    "    applied_filters = applied_filters,\n",
    "    time_duplicate_rows = time_duplicate_rows\n",
    "  )\n"
  )

  # --- Plot filtering_list ---
  plot_filter_code <- .build_filter_code(inputs$filtering_list)

  # --- Plot function call ---
  plot_parts <- switch(plot_type,
    individual = .build_individual_call(inputs),
    mean = .build_mean_call(inputs),
    qc = .build_qc_call(inputs),
    stop("Unknown plot_type: ", plot_type)
  )
  plot_call <- plot_parts$call
  # QC plot doesn't use filtering_list
  if (!is.null(plot_parts$filter_override)) {
    plot_filter_code <- plot_parts$filter_override
  }

  # --- Assemble full script ---
  .assemble_script(
    data_section, mapping_code, filters_code, time_dup_code,
    pknca_section, plot_filter_code, plot_call, plot_type
  )
}

# --- Format helpers: quote R objects for code output ---

.fmt_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return("NULL")
  quoted <- vapply(
    x, function(v) encodeString(v, quote = "\""), character(1),
    USE.NAMES = FALSE
  )
  if (length(quoted) == 1) return(quoted)
  paste0("c(", paste(quoted, collapse = ", "), ")")
}

.fmt_lgl <- function(x) {
  if (is.null(x)) return("NULL")
  if (!is.logical(x)) return("FALSE")
  if (x) "TRUE" else "FALSE"
}

.fmt_num <- function(x) {
  if (is.null(x) || length(x) == 0 || all(!is.finite(x))) return("NULL")
  if (length(x) == 1) return(as.character(x))
  paste0("c(", paste(x, collapse = ", "), ")")
}

.fmt_obj <- function(x) {
  if (is.null(x)) return("NULL")
  paste(deparse(x, width.cutoff = 500), collapse = "\n")
}

# --- Filter code builder ---

.build_filter_code <- function(fl) {
  if (is.null(fl) || length(fl) == 0) return("filtering_list <- NULL")
  filter_lines <- vapply(names(fl), function(col) {
    paste0("  ", col, " = ", .fmt_chr(fl[[col]]))
  }, character(1))
  paste0(
    "filtering_list <- list(\n",
    paste(filter_lines, collapse = ",\n"),
    "\n)"
  )
}

# --- Plot-type call builders ---

.build_individual_call <- function(inputs) {
  threshold_code <- .fmt_num(inputs$threshold_value)
  call <- paste0(
    "p <- aNCA::exploration_individualplot(\n",
    "  pknca_data = pknca_data,\n",
    "  color_by = ", .fmt_chr(inputs$color_by), ",\n",
    "  facet_by = ", .fmt_chr(inputs$facet_by), ",\n",
    "  show_facet_n = ", .fmt_lgl(inputs$show_facet_n), ",\n",
    "  ylog_scale = ", .fmt_lgl(inputs$ylog_scale), ",\n",
    "  show_legend = ", .fmt_lgl(inputs$show_legend), ",\n",
    "  show_dose = ", .fmt_lgl(inputs$show_dose), ",\n",
    "  threshold_value = ", threshold_code, ",\n",
    "  x_limits = ", .fmt_num(inputs$x_limits), ",\n",
    "  y_limits = ", .fmt_num(inputs$y_limits), ",\n",
    "  filtering_list = filtering_list,\n",
    "  labels_df = aNCA::metadata_nca_variables,\n",
    "  use_time_since_last_dose = ",
    .fmt_lgl(inputs$use_time_since_last_dose), ",\n",
    "  palette = ", .fmt_chr(inputs$palette), ",\n",
    "  line_type = ", .fmt_chr(inputs$y_axis_values), "\n",
    ")"
  )
  list(call = call, filter_override = NULL)
}

.build_mean_call <- function(inputs) {
  threshold_code <- .fmt_num(inputs$threshold_value)
  call <- paste0(
    "p <- aNCA::exploration_meanplot(\n",
    "  pknca_data = pknca_data,\n",
    "  color_by = ", .fmt_chr(inputs$color_by), ",\n",
    "  facet_by = ", .fmt_chr(inputs$facet_by), ",\n",
    "  show_facet_n = ", .fmt_lgl(inputs$show_facet_n), ",\n",
    "  ylog_scale = ", .fmt_lgl(inputs$ylog_scale), ",\n",
    "  show_legend = ", .fmt_lgl(inputs$show_legend), ",\n",
    "  show_dose = ", .fmt_lgl(inputs$show_dose), ",\n",
    "  threshold_value = ", threshold_code, ",\n",
    "  x_limits = ", .fmt_num(inputs$x_limits), ",\n",
    "  y_limits = ", .fmt_num(inputs$y_limits), ",\n",
    "  sd_min = ", .fmt_lgl(inputs$sd_min), ",\n",
    "  sd_max = ", .fmt_lgl(inputs$sd_max), ",\n",
    "  ci = ", .fmt_lgl(inputs$ci), ",\n",
    "  filtering_list = filtering_list,\n",
    "  labels_df = aNCA::metadata_nca_variables,\n",
    "  use_time_since_last_dose = ",
    .fmt_lgl(inputs$use_time_since_last_dose), ",\n",
    "  palette = ", .fmt_chr(inputs$palette), ",\n",
    "  line_type = ", .fmt_chr(inputs$y_axis_values), "\n",
    ")"
  )
  list(call = call, filter_override = NULL)
}

.build_qc_call <- function(inputs) {
  subj_filter <- ""
  if (!is.null(inputs$usubjid) && length(inputs$usubjid) > 0) {
    subj_filter <- paste0(
      "subj_col <- pknca_data$conc$columns$subject\n",
      "conc_data <- conc_data[conc_data[[subj_col]] %in% ",
      .fmt_chr(inputs$usubjid), ", ]\n",
      "dose_data <- dose_data[dose_data[[subj_col]] %in% ",
      .fmt_chr(inputs$usubjid), ", ]\n"
    )
  }

  pcspec_col <- if (!is.null(inputs$pcspec_col)) inputs$pcspec_col else "PCSPEC"
  shape_var <- if (!is.null(inputs$shape_var)) inputs$shape_var else "PCSPEC"
  tooltip_vars <- if (!is.null(inputs$other_tooltip_vars)) {
    inputs$other_tooltip_vars
  } else {
    c("NFRLT", "DOSETRT")
  }

  pcspec_filter <- ""
  if (!is.null(inputs$pcspec) && length(inputs$pcspec) > 0) {
    pcspec_filter <- paste0(
      "conc_data <- conc_data[conc_data$", pcspec_col, " %in% ",
      .fmt_chr(inputs$pcspec), ", ]\n"
    )
  }

  call <- paste0(
    "# Filter subjects and specimens\n",
    "conc_data <- pknca_data$conc$data\n",
    "dose_data <- pknca_data$dose$data\n",
    subj_filter,
    pcspec_filter,
    "\nshow_pk_samples <- ",
    .fmt_lgl("PK Samples" %in% inputs$show_samples_doses), "\n",
    "show_doses <- ",
    .fmt_lgl("Doses" %in% inputs$show_samples_doses), "\n",
    "\n",
    "dose_col <- pknca_data$dose$columns$dose\n",
    "doseu_col <- pknca_data$dose$columns$doseu\n",
    "colour_var_units <- if (",
    .fmt_chr(inputs$colour_var),
    " == dose_col) doseu_col else NULL\n",
    "\np <- aNCA::pk_dose_qc_plot(\n",
    "  data_conc = conc_data,\n",
    "  data_dose = dose_data,\n",
    "  x_var = pknca_data$conc$columns$time,\n",
    "  y_var = pknca_data$conc$columns$subject,\n",
    "  colour_var = ", .fmt_chr(inputs$colour_var), ",\n",
    "  shape_var = ", .fmt_chr(shape_var), ",\n",
    "  grouping_vars = ", .fmt_chr(inputs$group_var), ",\n",
    "  other_tooltip_vars = ", .fmt_chr(tooltip_vars), ",\n",
    "  x_var_units = pknca_data$conc$columns$timeu,\n",
    "  colour_var_units = colour_var_units,\n",
    "  labels_df = aNCA::metadata_nca_variables,\n",
    "  title = \"Dose and Sample Events\",\n",
    "  show_pk_samples = show_pk_samples,\n",
    "  show_doses = show_doses,\n",
    "  as_plotly = FALSE\n",
    ")"
  )
  list(call = call, filter_override = "")
}

# --- Script assembly ---

.assemble_script <- function(data_section, mapping_code, filters_code,
                             time_dup_code, pknca_section, plot_filter_code,
                             plot_call, plot_type) {
  tooltip <- if (plot_type == "qc") "text" else "tooltip_text"

  filter_section <- if (nzchar(plot_filter_code)) {
    paste0("## Plot filtering\n", plot_filter_code, "\n\n")
  } else {
    ""
  }

  paste0(
    "# Reproducible plot code generated by aNCA\n",
    "# Requires: aNCA, dplyr, ggplot2, plotly, htmlwidgets\n",
    "\n",
    "library(aNCA)\n",
    "library(dplyr)\n",
    "\n",
    data_section,
    "\n",
    "## Preprocessing & PKNCA object creation\n",
    mapping_code, "\n",
    "\n",
    filters_code, "\n",
    "\n",
    time_dup_code, "\n",
    "\n",
    pknca_section,
    "\n",
    filter_section,
    "## Generate plot\n",
    plot_call, "\n",
    "\n",
    "# Export as PNG\n",
    "ggplot2::ggsave(\"plot.png\", plot = p, width = 10, height = 6, dpi = 300)\n",
    "\n",
    "# Export as interactive HTML\n",
    "p_interactive <- plotly::ggplotly(p, tooltip = \"", tooltip, "\")\n",
    "htmlwidgets::saveWidget(p_interactive, \"plot.html\", selfcontained = TRUE)\n"
  )
}
