
# Utility functions for creating PowerPoint presentations with tables and plots using officer

#' Create a new PowerPoint document from a template and add a title slide
#' @param path File path to save the presentation
#' @param title Title for the presentation
#' @param template Path to PowerPoint template file
#' @importFrom officer read_pptx add_slide ph_with ph_location_type
#' @return rpptx object
create_pptx_doc <- function(path, title, template) {
  pptx <- read_pptx(template)
  add_pptx_sl_title(pptx, title)
}

#' Add a title slide to the rpptx document
#' @param pptx rpptx object
#' @param title Title text
#' @importFrom officer add_slide ph_with ph_location_type
#' @return rpptx object with title slide added
#' @keywords internal
add_pptx_sl_title <- function(pptx, title) {
  add_slide(pptx, layout = "Title Slide", master = "Office Theme") %>%
    ph_with(
      value = title,
      location = ph_location_type(type = "ctrTitle")
    )
}

#' Add a slide with both a plot and a table
#' @param pptx rpptx object
#' @param df Data frame to show as table
#' @param plot ggplot object to show as plot
#' @importFrom officer add_slide ph_with ph_location_type
#' @importFrom flextable flextable
#' @return rpptx object with slide added
add_pptx_sl_plottable <- function(pptx, df, plot) {
  slide <- add_slide(pptx, layout = "Content with Caption")
  if (!is.null(plot)) {
    slide <- ph_with(slide, value = plot, location = "Content Placeholder 1")
  }
  ph_with(slide, value = flextable::flextable(df, cwidth = 1), location = "Table Placeholder 1")
}

#' Add a slide with a table only
#' @param pptx rpptx object
#' @param df Data frame to show as table
#' @param title Title text for the slide
#' @param subtitle Subtitle text for the slide
#' @param footer Footer text for the slide
#' @importFrom officer add_slide ph_with fpar ftext fp_par fp_text
#' @importFrom flextable flextable
#' @return rpptx object with slide added
add_pptx_sl_table <- function(pptx, df, title = "",
                              subtitle = "",
                              footer = "Click here for individual results") {

  title_formatted <- fpar(
    ftext(title),
    "\n",
    ftext(subtitle, prop = fp_text(font.size = 12)),
    fp_p = fp_par(text.align = "center", line_spacing = 1)
  )

  # Set flextable to autofit and center for better appearance
  ft <- flextable::flextable(df) %>%
    flextable::autofit()

  add_slide(pptx, layout = "Title Only") %>%
    ph_with(value = ft, location = "Table Placeholder 1") %>%
    ph_with(value = title_formatted, location = "Title 1") %>%
    ph_with(value = footer, location = "Footer Placeholder 3")
}

#' Add a slide with a plot only
#' @param pptx rpptx object
#' @param plot ggplot object to show as plot
#' @importFrom officer add_slide ph_with
#' @return rpptx object with slide added
add_pptx_sl_plot <- function(pptx, plot) {
  add_slide(pptx, layout = "Picture with Caption") %>%
    ph_with(value = plot, location = "Picture Placeholder 2")
}

#' Add individual-subject slides for one dose group to a pptx object
#' @param pptx rpptx object
#' @param group_data Single element from res_dose_slides
#' @param group_index Integer index of the dose group
#' @param in_sections Function(id) returning TRUE when id is selected
#' @return List with updated pptx and n_slides count
#' @keywords internal
.add_pptx_ind_slides <- function(pptx, group_data, group_index, in_sections) {
  if (!in_sections("ind_plots") && !in_sections("ind_params")) {
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
    function(pptx, subj) {
      if (in_sections("ind_plots") && in_sections("ind_params")) {
        add_pptx_sl_plottable(pptx,
                              df   = group_data$ind_params[[subj]],
                              plot = group_data$ind_plots[[subj]])
      } else if (in_sections("ind_plots")) {
        add_pptx_sl_plot(pptx, plot = group_data$ind_plots[[subj]])
      } else {
        add_pptx_sl_table(pptx, df = group_data$ind_params[[subj]], footer = "")
      }
    },
    .init = pptx
  )
  n_slides <- 1 + length(group_data$ind_params)
  list(pptx = pptx, n_slides = n_slides)
}

#' Filter an additional_analysis list to non-empty data frames,
#' optionally restricted to slide_sections
#' @param additional_analysis Named list of data frames
#' @param slide_sections Character vector of selected section IDs, or NULL for all
#' @return Filtered named list
#' @keywords internal
.filter_additional_analysis <- function(additional_analysis, slide_sections) {
  if (is.null(additional_analysis)) return(NULL)
  result <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, additional_analysis)
  if (!is.null(slide_sections)) {
    result <- result[names(result) %in% slide_sections]
  }
  result
}

#' Create a PowerPoint presentation with dose escalation results, including main and extra figures
#' Adds slides for summary tables, mean plots, line plots, and individual subject results
#' @param res_dose_slides List of results for each dose group
#' @param path File path to save the presentation
#' @param title Title for the presentation
#' @param template Path to PowerPoint template file
#' @importFrom officer read_pptx add_slide ph_with ph_location_type ph_slidelink move_slide
#' @return TRUE (invisible). Writes the PowerPoint file to the specified path
create_pptx_dose_slides <- function(res_dose_slides, path, title, template) {
  slide_sections <- attr(res_dose_slides, "slide_sections")
  additional_analysis <- attr(res_dose_slides, "additional_analysis")

  in_sections <- function(id) is.null(slide_sections) || id %in% slide_sections

  pptx <- create_pptx_doc(path, title, template)

  lst_group_slide <- 1
  group_slides <- numeric()
  for (i in seq_along(res_dose_slides)) {
    ind_result <- .add_pptx_ind_slides(pptx, res_dose_slides[[i]], i, in_sections)
    pptx <- ind_result$pptx
    n_ind_slides <- ind_result$n_slides

    # Generate summary figures and tables
    if (in_sections("meanplot") || in_sections("statistics") ||
        in_sections("linplot") || in_sections("boxplot")) {
      pptx <- add_pptx_sl_table(pptx, res_dose_slides[[i]]$info, paste0("Group ", i, " Summary"),
                                subtitle = paste(res_dose_slides[[i]]$group)) %>%
        ph_slidelink(ph_label = "Footer Placeholder 3", slide_index = (lst_group_slide + 1))
      if (in_sections("meanplot") && in_sections("statistics")) {
        pptx <- add_pptx_sl_plottable(pptx,
                                      df   = res_dose_slides[[i]]$statistics,
                                      plot = res_dose_slides[[i]]$meanplot)
      } else if (in_sections("meanplot")) {
        pptx <- add_pptx_sl_plot(pptx, plot = res_dose_slides[[i]]$meanplot)
      } else {
        pptx <- add_pptx_sl_table(pptx,
                                  df    = res_dose_slides[[i]]$statistics,
                                  title = paste0("Group ", i, " Summary Statistics"),
                                  footer = "")
      }
      pptx <- pptx %>% {
        if (in_sections("linplot")) add_pptx_sl_plot(., res_dose_slides[[i]]$linplot) else .
      }
      boxplots_i <- res_dose_slides[[i]]$boxplot
      if (in_sections("boxplot") && is.list(boxplots_i)) {
        for (bp_plot in boxplots_i) {
          if (!is.null(bp_plot)) pptx <- add_pptx_sl_plot(pptx, bp_plot)
        }
      }

      n_boxplot_slides <- if (in_sections("boxplot") && is.list(boxplots_i)) {
        sum(vapply(boxplots_i, Negate(is.null), logical(1)))
      } else {
        0L
      }
      n_summary_slides <- 2L + as.integer(in_sections("linplot")) + n_boxplot_slides
      lst_group_slide <- lst_group_slide + n_ind_slides + n_summary_slides
      group_slides <- c(group_slides, (lst_group_slide - n_summary_slides + 1):(lst_group_slide))
    } else {
      lst_group_slide <- lst_group_slide + n_ind_slides
    }
  }

  if (length(group_slides) > 0) {
    group_slides_rev <- rev(group_slides) + (seq_along(group_slides) - 1)
    pptx <- purrr::reduce(
      group_slides_rev,
      function(pptx, slide_index) move_slide(pptx, index = slide_index, to = 2),
      .init = pptx
    )
  }
  pptx <- add_pptx_sl_title(pptx, "Extra Figures")
  pptx <- move_slide(x = pptx, index = length(pptx), to = (length(group_slides) + 2))

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
