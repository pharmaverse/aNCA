
# Utility functions for creating PowerPoint presentations with tables and plots using officer

#' Create a new PowerPoint document from a template and add a title slide
#' @param path File path to save the presentation
#' @param title Title for the presentation
#' @param template Path to PowerPoint template file
#' @return rpptx object
#' @keywords internal
create_pptx_doc <- function(path, title, template) {
  pptx <- officer::read_pptx(template)
  add_pptx_sl_title(pptx, title)
}

#' Add a title slide to the rpptx document
#' @param pptx rpptx object
#' @param title Title text
#' @return rpptx object with title slide added
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
#' @return rpptx object with slide added
add_pptx_sl_plottable <- function(pptx, df, plot) {
  officer::add_slide(pptx, layout = "Content with Caption") %>%
    officer::ph_with(value = plot, location = "Content Placeholder 1") %>%
    officer::ph_with(
      value = flextable::flextable(df, cwidth = 1), location = "Table Placeholder 1"
    )
}

#' Add a slide with a table only
#' @param pptx rpptx object
#' @param df Data frame to show as table
#' @param title Title text for the slide
#' @param subtitle Subtitle text for the slide
#' @param footer Footer text for the slide
#' @return rpptx object with slide added
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
    flextable::autofit()

  officer::add_slide(pptx, layout = "Title Only") %>%
    officer::ph_with(value = ft, location = "Table Placeholder 1") %>%
    officer::ph_with(value = title_formatted, location = "Title 1") %>%
    officer::ph_with(value = footer, location = "Footer Placeholder 3")
}

#' Add a slide with a plot only
#' @param pptx rpptx object
#' @param plot ggplot object to show as plot
#' @return rpptx object with slide added
add_pptx_sl_plot <- function(pptx, plot) {
  officer::add_slide(pptx, layout = "Picture with Caption") %>%
    officer::ph_with(value = plot, location = "Picture Placeholder 2")
}

#' Create a PowerPoint presentation with dose escalation results, including main and extra figures
#' Adds slides for summary tables, mean plots, line plots, and individual subject results
#' @param res_dose_slides List of results for each dose group
#' @param path File path to save the presentation
#' @param title Title for the presentation
#' @param template Path to PowerPoint template file
#' @return TRUE (invisible). Writes the PowerPoint file to the specified path
create_pptx_dose_slides <- function(res_dose_slides, path, title, template) {
  for (pkg in c("officer", "flextable")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Package '", pkg, "' is required for PowerPoint export. ",
        "Install it with install.packages('", pkg, "')"
      )
    }
  }

  pptx <- create_pptx_doc(path, title, template)

  lst_group_slide <- 1
  group_slides <- numeric()
  for (i in seq_along(res_dose_slides)) {

    # Generate the individual figures
    pptx <- add_pptx_sl_table(
      pptx, res_dose_slides[[i]]$info,
      title = paste0("Group ", i, " (individual)"),
      subtitle = paste(res_dose_slides[[i]]$group),
      footer = ""
    )
    pptx <- purrr::reduce(
      names(res_dose_slides[[i]]$ind_params),
      function(pptx, subj) {
        add_pptx_sl_plottable(
          pptx,
          df = res_dose_slides[[i]]$ind_params[[subj]],
          plot = res_dose_slides[[i]]$ind_plots[[subj]]
        )
      },
      .init = pptx
    )

    # Generate summary figures and tables
    pptx <- add_pptx_sl_table(pptx, res_dose_slides[[i]]$info, paste0("Group ", i, " Summary"),
                              subtitle = paste(res_dose_slides[[i]]$group)) %>%
      officer::ph_slidelink(ph_label = "Footer Placeholder 3",
                            slide_index = (lst_group_slide + 1)) %>%
      add_pptx_sl_plottable(
        df = res_dose_slides[[i]]$statistics,
        plot = res_dose_slides[[i]]$meanplot
      ) %>%
      add_pptx_sl_plot(res_dose_slides[[i]]$linplot) %>%
      add_pptx_sl_plot(res_dose_slides[[i]]$boxplot)

    n_ind <- length(res_dose_slides[[i]]$ind_params)
    lst_group_slide <- lst_group_slide + 1 + n_ind + 4
    group_slides <- c(group_slides, (lst_group_slide - 3):(lst_group_slide))
  }

  group_slides_rev <- rev(group_slides) + (seq_along(group_slides) - 1)
  pptx <- purrr::reduce(
    group_slides_rev,
    function(pptx, slide_index) officer::move_slide(pptx, index = slide_index, to = 2),
    .init = pptx
  )
  pptx <- add_pptx_sl_title(pptx, "Extra Figures")
  pptx <- officer::move_slide(
    x = pptx, index = length(pptx), to = (length(group_slides) + 2)
  )

  additional_analysis <- attr(res_dose_slides, "additional_analysis")
  additional_tables <- list(
    "Matrix Ratios" = additional_analysis$matrix_ratios,
    "Excretion Summary" = additional_analysis$excretion_summary
  )
  additional_tables <- additional_tables[
    vapply(additional_tables, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))
  ]
  if (length(additional_tables) > 0) {
    pptx <- add_pptx_sl_title(pptx, "Additional Analysis Figures")
    pptx <- purrr::reduce(
      names(additional_tables),
      function(pptx, title) add_pptx_sl_table(pptx, additional_tables[[title]], title = title, footer = ""),
      .init = pptx
    )
  }

  print(pptx, target = path)
  invisible(TRUE)
}
