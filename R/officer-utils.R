
# Utility functions for creating PowerPoint presentations with tables and plots using officer

#' Create a new PowerPoint document from a template and add a title slide
#' @param path File path to save the presentation
#' @param title Title for the presentation
#' @param template Path to PowerPoint template file
#' @returns rpptx object
#' @keywords internal
create_pptx_doc <- function(path, title, template) {
  pptx <- officer::read_pptx(template)
  add_pptx_sl_title(pptx, title)
}

#' Add a title slide to the rpptx document
#' @param pptx rpptx object
#' @param title Title text
#' @returns rpptx object with title slide added
#' @keywords internal
add_pptx_sl_title <- function(pptx, title) {
  officer::add_slide(pptx, layout = "Title Slide", master = "Office Theme") %>%
    officer::ph_with(
      value = title,
      location = officer::ph_location_type(type = "ctrTitle")
    )
}

#' Add a slide with both a plot and a table
#' @param pptx rpptx object
#' @param df Data frame to show as table
#' @param plot ggplot object to show as plot
#' @returns rpptx object with slide added
add_pptx_sl_plottable <- function(pptx, df, plot) {
  ft <- flextable::flextable(df, cwidth = 1) %>%
    flextable::fontsize(size = 9, part = "all")
  officer::add_slide(pptx, layout = "Content with Caption") %>%
    officer::ph_with(value = plot, location = "Content Placeholder 1") %>%
    officer::ph_with(value = ft, location = "Table Placeholder 1")
}

#' Add a slide with a table only
#' @param pptx rpptx object
#' @param df Data frame to show as table
#' @param title Title text for the slide
#' @param subtitle Subtitle text for the slide
#' @param footer Footer text for the slide
#' @returns rpptx object with slide added
add_pptx_sl_table <- function(pptx, df, title = "",
                              subtitle = "",
                              footer = "Click here for individual results") {

  title_formatted <- officer::fpar(
    officer::ftext(title),
    "\n",
    officer::ftext(subtitle, prop = officer::fp_text(font.size = 12)),
    fp_p = officer::fp_par(text.align = "center", line_spacing = 1)
  )

  # Set flextable to autofit and center for better appearance
  ft <- flextable::flextable(df) %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::autofit()

  officer::add_slide(pptx, layout = "Title Only") %>%
    officer::ph_with(value = ft, location = "Table Placeholder 1") %>%
    officer::ph_with(value = title_formatted, location = "Title 1") %>%
    officer::ph_with(value = footer, location = "Footer Placeholder 3")
}

#' Add a slide with a plot only
#' @param pptx rpptx object
#' @param plot ggplot object to show as plot
#' @returns rpptx object with slide added
add_pptx_sl_plot <- function(pptx, plot) {
  officer::add_slide(pptx, layout = "Picture with Caption") %>%
    officer::ph_with(value = plot, location = "Picture Placeholder 2")
}

#' Add one individual-subject slide (plot+table, plot-only, or table-only)
#' @param pptx An officer pptx object.
#' @param group_data One element of res_dose_slides (a dose group).
#' @param subj Subject identifier string.
#' @param in_sections Function(id) returning TRUE when the section id is selected.
#' @returns Updated pptx object.
#' @keywords internal
#' @noRd
.add_pptx_one_ind_slide <- function(pptx, group_data, subj, in_sections) {
  if (in_sections("ind_plots") && in_sections("ind_params")) {
    add_pptx_sl_plottable(pptx,
                          df   = group_data$ind_params[[subj]],
                          plot = group_data$ind_plots[[subj]])
  } else if (in_sections("ind_plots")) {
    add_pptx_sl_plot(pptx, plot = group_data$ind_plots[[subj]])
  } else {
    add_pptx_sl_table(pptx, df = group_data$ind_params[[subj]], footer = "")
  }
}

#' Add individual-subject slides for one dose group to a pptx object
#' @param pptx rpptx object
#' @param group_data Single element from res_dose_slides
#' @param group_index Integer index of the dose group
#' @param in_sections Function(id) returning TRUE when id is selected
#' @returns List with updated pptx and n_slides count
#' @keywords internal
#' @noRd
.add_pptx_ind_slides <- function(pptx, group_data, group_index, in_sections) {
  if (!in_sections("ind_plots") && !in_sections("ind_params")) {
    return(list(pptx = pptx, n_slides = 0))
  }
  if (length(group_data$ind_params) == 0 && length(group_data$ind_plots) == 0) {
    return(list(pptx = pptx, n_slides = 0))
  }
  pptx <- add_pptx_sl_table(
    pptx, group_data$info,
    title = paste0("Group ", group_index, " (individual)"),
    subtitle = paste(group_data$group),
    footer = ""
  )
  pptx <- purrr::reduce(
    names(group_data$ind_params),
    function(pptx, subj) .add_pptx_one_ind_slide(pptx, group_data, subj, in_sections),
    .init = pptx
  )
  list(pptx = pptx, n_slides = 1 + length(group_data$ind_params))
}

#' Filter an additional_analysis list to non-empty data frames,
#' optionally restricted to slide_sections
#' @param additional_analysis Named list of data frames
#' @param slide_sections Character vector of selected section IDs, or NULL for all
#' @returns Filtered named list
#' @keywords internal
#' @noRd
.filter_additional_analysis <- function(additional_analysis, slide_sections) {
  if (is.null(additional_analysis)) return(NULL)
  result <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, additional_analysis)
  if (!is.null(slide_sections)) {
    result <- result[names(result) %in% slide_sections]
  }
  result
}

#' Add the main summary slide (meanplot / statistics / both) for a dose group
#' @param pptx An officer pptx object.
#' @param group_data One element of res_dose_slides (a dose group).
#' @param i Integer index of this dose group.
#' @param in_sections Function(id) returning TRUE when the section id is selected.
#' @returns Updated pptx object.
#' @keywords internal
#' @noRd
.add_pptx_main_summary_slide <- function(pptx, group_data, i, in_sections) {
  if (in_sections("meanplot") && in_sections("statistics")) {
    add_pptx_sl_plottable(pptx, df = group_data$statistics, plot = group_data$meanplot)
  } else if (in_sections("meanplot")) {
    add_pptx_sl_plot(pptx, plot = group_data$meanplot)
  } else if (in_sections("statistics")) {
    add_pptx_sl_table(pptx,
                      df    = group_data$statistics,
                      title = paste0("Group ", i, " Summary Statistics"),
                      footer = "")
  } else {
    pptx
  }
}

#' Check whether a dose-normalised statistics data frame contains parameter columns
#' @param df A data frame or NULL.
#' @returns Logical scalar.
#' @keywords internal
#' @noRd
.has_dose_norm_data <- function(df) {
  !is.null(df) && any(!names(df) %in% c("Statistic", "DOSEA"))
}

#' Dispatch rendering for a dose-normalised slide (plot+table, plot-only, or table-only)
#' @param pptx An officer pptx object.
#' @param group_data One element of res_dose_slides (a dose group).
#' @param show_plot Logical; whether the dose_norm_plot section is selected.
#' @param has_stats Logical; whether dose_norm_statistics is available and selected.
#' @returns Updated pptx object.
#' @keywords internal
#' @noRd
.render_dose_norm_slide <- function(pptx, group_data, show_plot, has_stats) {
  has_plot <- show_plot && !is.null(group_data$dose_norm_meanplot)
  if (has_plot && has_stats) {
    add_pptx_sl_plottable(pptx,
                          df   = group_data$dose_norm_statistics,
                          plot = group_data$dose_norm_meanplot)
  } else if (has_plot) {
    add_pptx_sl_plot(pptx, plot = group_data$dose_norm_meanplot)
  } else if (has_stats) {
    add_pptx_sl_table(pptx, df = group_data$dose_norm_statistics, footer = "")
  } else {
    pptx
  }
}

#' Add a dose-normalized mean plot and statistics slide for a dose group
#' @param pptx An officer pptx object.
#' @param group_data One element of res_dose_slides (a dose group).
#' @param in_sections Function(id) returning TRUE when the section id is selected.
#' @returns List with elements `pptx` and `n_slides`.
#' @keywords internal
#' @noRd
.add_pptx_dose_norm_slide <- function(pptx, group_data, in_sections) {
  show_plot  <- in_sections("dose_norm_plot")
  show_stats <- in_sections("dose_norm_statistics")
  if (!show_plot && !show_stats) return(list(pptx = pptx, n_slides = 0L))
  has_stats <- show_stats && .has_dose_norm_data(group_data$dose_norm_statistics)
  has_plot  <- show_plot && !is.null(group_data$dose_norm_meanplot)
  if (!has_plot && !has_stats) return(list(pptx = pptx, n_slides = 0L))
  list(pptx = .render_dose_norm_slide(pptx, group_data, show_plot, has_stats), n_slides = 1L)
}

#' Add boxplot slides for a dose group and return updated pptx and slide count
#' @param pptx An officer pptx object.
#' @param group_data One element of res_dose_slides (a dose group).
#' @param in_sections Function(id) returning TRUE when the section id is selected.
#' @returns List with elements `pptx`, `n_slides`.
#' @keywords internal
#' @noRd
.add_pptx_boxplot_slides <- function(pptx, group_data, in_sections) {
  boxplots_i <- group_data$boxplot
  if (!in_sections("boxplot") || !is.list(boxplots_i)) {
    return(list(pptx = pptx, n_slides = 0L))
  }
  for (bp_plot in boxplots_i) {
    if (!is.null(bp_plot)) pptx <- add_pptx_sl_plot(pptx, bp_plot)
  }
  n_slides <- sum(vapply(boxplots_i, Negate(is.null), logical(1)))
  list(pptx = pptx, n_slides = n_slides)
}

#' Add summary slides for one dose group to a pptx object
#' @param pptx An officer pptx object.
#' @param group_data One element of res_dose_slides (a dose group).
#' @param i Integer index of this dose group.
#' @param in_sections Function(id) returning TRUE when the section id is selected.
#' @param lst_group_slide Integer slide index at the start of this group.
#' @returns List with elements `pptx`, `n_summary_slides`.
#' @keywords internal
#' @noRd
.add_pptx_group_summary <- function(pptx, group_data, i, in_sections, lst_group_slide) {
  pptx <- add_pptx_sl_table(pptx, group_data$info, paste0("Group ", i, " Summary"),
                            subtitle = paste(group_data$group)) %>%
    officer::ph_slidelink(ph_label = "Footer Placeholder 3", slide_index = (lst_group_slide + 1))
  pptx <- .add_pptx_main_summary_slide(pptx, group_data, i, in_sections)
  pptx <- pptx %>% {
    if (in_sections("linplot")) add_pptx_sl_plot(., group_data$linplot) else .
  }
  bp_result <- .add_pptx_boxplot_slides(pptx, group_data, in_sections)
  pptx <- bp_result$pptx
  dn_result <- .add_pptx_dose_norm_slide(pptx, group_data, in_sections)
  pptx <- dn_result$pptx
  n_main_slides <- as.integer(in_sections("meanplot") || in_sections("statistics"))
  n_summary_slides <- 1L + n_main_slides + as.integer(in_sections("linplot")) +
    bp_result$n_slides + dn_result$n_slides
  list(pptx = pptx, n_summary_slides = n_summary_slides)
}

#' Check whether any summary section is selected
#' @param in_sections Function(id) returning TRUE when the section id is selected.
#' @returns Logical scalar.
#' @keywords internal
#' @noRd
.has_summary_sections <- function(in_sections) {
  sections <- c("meanplot", "statistics", "linplot", "boxplot",
                "dose_norm_plot", "dose_norm_statistics")
  any(vapply(sections, in_sections, logical(1)))
}

#' Update slide index counters when summary slides are added for a dose group
#' @param lst_group_slide Current slide index counter (integer).
#' @param n_ind_slides Number of individual slides added.
#' @param n_summary_slides Number of summary slides added.
#' @param group_slides Integer vector of summary slide indices accumulated so far.
#' @returns List with elements `lst_group_slide` and `group_slides`.
#' @keywords internal
#' @noRd
.update_group_slide_index <- function(lst_group_slide, n_ind_slides, n_summary_slides,
                                      group_slides) {
  lst_group_slide <- lst_group_slide + n_ind_slides + n_summary_slides
  group_slides <- c(group_slides, (lst_group_slide - n_summary_slides + 1):(lst_group_slide))
  list(lst_group_slide = lst_group_slide, group_slides = group_slides)
}

#' Process one dose group's slides, returning updated pptx, lst_group_slide, group_slides
#' @param pptx An officer pptx object.
#' @param group_data One element of res_dose_slides (a dose group).
#' @param i Integer index of this dose group.
#' @param in_sections Function(id) returning TRUE when the section id is selected.
#' @param lst_group_slide Current slide index counter.
#' @param group_slides Integer vector of summary slide indices accumulated so far.
#' @returns List with elements `pptx`, `lst_group_slide`, `group_slides`.
#' @keywords internal
#' @noRd
.process_pptx_group_slides <- function(pptx, group_data, i, in_sections,
                                       lst_group_slide, group_slides) {
  ind_result <- .add_pptx_ind_slides(pptx, group_data, i, in_sections)
  pptx <- ind_result$pptx
  n_ind_slides <- ind_result$n_slides
  if (.has_summary_sections(in_sections)) {
    summary_result <- .add_pptx_group_summary(pptx, group_data, i, in_sections, lst_group_slide)
    pptx <- summary_result$pptx
    idx <- .update_group_slide_index(lst_group_slide, n_ind_slides,
                                     summary_result$n_summary_slides, group_slides)
    lst_group_slide <- idx$lst_group_slide
    group_slides <- idx$group_slides
  } else {
    lst_group_slide <- lst_group_slide + n_ind_slides
  }
  list(pptx = pptx, lst_group_slide = lst_group_slide, group_slides = group_slides)
}

#' Collect all unique PPTESTCDs used across dose group slide data
#'
#' Extracts PPTESTCDs from statistics tables (column names), individual
#' parameter tables (column names), and boxplot names.
#'
#' @param res_dose_slides List of dose group results as produced by
#'   `get_dose_esc_results()`.
#' @returns Character vector of unique PPTESTCDs.
#' @keywords internal
#' @noRd
.collect_pptestcds <- function(res_dose_slides) {
  codes <- unlist(lapply(res_dose_slides, .extract_group_codes), use.names = FALSE)
  unique(as.character(codes))
}

#' Extract PPTESTCDs from a single dose group's slide data
#' @param group A single element of res_dose_slides.
#' @returns Character vector of PPTESTCDs (may contain duplicates).
#' @keywords internal
#' @noRd
.extract_group_codes <- function(group) {
  codes <- character(0)
  # Statistics table columns: "PPTESTCD[unit]" or plain "PPTESTCD"
  for (tbl_name in c("statistics", "dose_norm_statistics")) {
    codes <- c(codes, .codes_from_df(group[[tbl_name]]))
  }
  # Individual parameter tables: same column format
  if (is.list(group$ind_params)) {
    codes <- c(codes, unlist(lapply(group$ind_params, .codes_from_df), use.names = FALSE))
  }
  # Boxplot names are PPTESTCDs directly
  if (is.list(group$boxplot)) {
    codes <- c(codes, names(group$boxplot))
  }
  codes
}

#' Extract PPTESTCDs from data frame column names by stripping unit suffixes
#' @param tbl A data frame or NULL.
#' @returns Character vector of codes, or empty character.
#' @keywords internal
#' @noRd
.codes_from_df <- function(tbl) {
  if (!is.data.frame(tbl) || ncol(tbl) == 0) return(character(0))
  gsub("\\[.*\\]$", "", names(tbl))
}

#' Build a glossary data frame of PPTESTCD to PPTEST mappings
#'
#' Uses `translate_terms()` to look up PPTEST for each PPTESTCD. Codes in
#' `metadata_nca_parameters` get their proper label; codes not in metadata
#' (e.g. custom ratio PPTESTCDs) are kept with the code itself as the label.
#' Non-parameter column names (where PPTESTCD == PPTEST and not in metadata)
#' are excluded.
#'
#' @param pptestcds Character vector of PPTESTCDs to include.
#' @returns A data frame with columns `PPTESTCD` and `PPTEST`, sorted by
#'   PPTESTCD.
#' @keywords internal
#' @noRd
.build_glossary <- function(pptestcds) {
  pptestcds <- unique(pptestcds)
  pptest <- translate_terms(pptestcds, mapping_col = "PPTESTCD", target_col = "PPTEST")
  known_codes <- metadata_nca_parameters$PPTESTCD
  # Keep codes that are in metadata OR where translate_terms changed the value
  # (custom codes not in metadata get themselves back — keep them too)
  is_known <- pptestcds %in% known_codes
  is_translated <- pptestcds != pptest
  keep <- is_known | is_translated
  glossary <- data.frame(
    PPTESTCD = pptestcds[keep],
    PPTEST = pptest[keep],
    stringsAsFactors = FALSE
  )
  glossary <- glossary[order(glossary$PPTESTCD), ]
  rownames(glossary) <- NULL
  glossary
}

#' Add glossary slides to a PowerPoint presentation
#'
#' Inserts one or more slides with a two-column table (PPTESTCD, PPTEST)
#' listing all PK parameters used in the presentation. Splits across
#' multiple slides when the table exceeds `max_rows` per slide.
#'
#' @param pptx An officer rpptx object.
#' @param glossary A data frame with columns `PPTESTCD` and `PPTEST`.
#' @param max_rows Maximum rows per glossary slide. Default 8.
#' @returns A list with `pptx` (updated rpptx object) and `n_slides`
#'   (number of glossary slides added).
#' @keywords internal
#' @noRd
.add_pptx_glossary_slides <- function(pptx, glossary, max_rows = 8L) {
  if (nrow(glossary) == 0) return(list(pptx = pptx, n_slides = 0L))

  # Position table closer to the title than the default template placeholder,
  # horizontally centered on the 10" slide
  table_loc <- officer::ph_location(
    left = 1.85, top = 1.2, width = 6.3, height = 3.8
  )

  chunks <- split(glossary, ceiling(seq_len(nrow(glossary)) / max_rows))
  for (i in seq_along(chunks)) {
    page_label <- if (length(chunks) > 1) {
      paste0("Glossary (", i, "/", length(chunks), ")")
    } else {
      "Glossary"
    }

    title_formatted <- officer::fpar(
      officer::ftext(page_label),
      fp_p = officer::fp_par(text.align = "center", line_spacing = 1)
    )
    ft <- flextable::flextable(chunks[[i]]) %>%
      flextable::fontsize(size = 9, part = "all") %>%
      flextable::autofit()

    pptx <- officer::add_slide(pptx, layout = "Title Only") %>%
      officer::ph_with(value = ft, location = table_loc) %>%
      officer::ph_with(value = title_formatted, location = "Title 1") %>%
      officer::ph_with(value = "", location = "Footer Placeholder 3")
  }
  list(pptx = pptx, n_slides = length(chunks))
}

#' Create a PowerPoint presentation with dose escalation results, including main and extra figures
#' Adds slides for summary tables, mean plots, line plots, and individual subject results
#' @param res_dose_slides List of results for each dose group
#' @param path File path to save the presentation
#' @param title Title for the presentation
#' @param template Path to PowerPoint template file
#' @returns TRUE (invisible). Writes the PowerPoint file to the specified path
create_pptx_dose_slides <- function(res_dose_slides, path, title, template) {
  for (pkg in c("officer", "flextable")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Package '", pkg, "' is required for PowerPoint export. ",
        "Install it with install.packages('", pkg, "')"
      )
    }
  }

  slide_sections     <- attr(res_dose_slides, "slide_sections")
  additional_analysis <- attr(res_dose_slides, "additional_analysis")

  in_sections <- function(id) is.null(slide_sections) || id %in% slide_sections

  pptx <- create_pptx_doc(path, title, template)

  # Insert glossary slide(s) after the title slide
  all_codes <- .collect_pptestcds(res_dose_slides)
  glossary <- .build_glossary(all_codes)
  glossary_result <- .add_pptx_glossary_slides(pptx, glossary)
  pptx <- glossary_result$pptx
  n_glossary_slides <- glossary_result$n_slides

  lst_group_slide <- 1 + n_glossary_slides
  group_slides <- numeric()
  for (i in seq_along(res_dose_slides)) {
    result <- .process_pptx_group_slides(pptx, res_dose_slides[[i]], i, in_sections,
                                         lst_group_slide, group_slides)
    pptx           <- result$pptx
    lst_group_slide <- result$lst_group_slide
    group_slides    <- result$group_slides
  }

  # Move summary slides to just after title + glossary
  first_content_pos <- 2L + n_glossary_slides
  if (length(group_slides) > 0) {
    group_slides_rev <- rev(group_slides) + (seq_along(group_slides) - 1)
    pptx <- purrr::reduce(
      group_slides_rev,
      function(pptx, slide_index) {
        officer::move_slide(pptx, index = slide_index, to = first_content_pos)
      },
      .init = pptx
    )
  }
  pptx <- add_pptx_sl_title(pptx, "Extra Figures")
  pptx <- officer::move_slide(
    x = pptx, index = length(pptx),
    to = (length(group_slides) + first_content_pos)
  )

  # Add additional analysis slides generically
  non_empty <- .filter_additional_analysis(additional_analysis, slide_sections)
  if (length(non_empty) > 0) {
    pptx <- add_pptx_sl_title(pptx, "Additional Analysis Figures")
    for (name in names(non_empty)) {
      label <- tools::toTitleCase(gsub("_", " ", name))
      pptx <- add_pptx_sl_table(pptx, non_empty[[name]], title = label)
    }
  }

  print(pptx, target = path)
  invisible(TRUE)
}
