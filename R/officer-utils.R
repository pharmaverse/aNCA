
# Utility functions for creating PowerPoint presentations with tables and plots using officer

#' Create a new PowerPoint document from a template and add a title slide
#' @param path File path to save the presentation
#' @param title Title for the presentation
#' @param template Path to PowerPoint template file
#' @importFrom officer read_pptx add_slide ph_with ph_location_type
#' @return rpptx object
create_pptx_doc <- function(path, title, template) {
  pptx <- read_pptx(template)
  # If the title contains a project name in parentheses, format the first slide as:
  # "NCA Results\nProjectName" per user request. Otherwise use the passed title.
  if (grepl("\\(", title)) {
    proj <- sub(".*\\((.*)\\).*", "\\1", title)
    slide_title <- paste0("NCA Results\n", proj)
  } else {
    slide_title <- title
  }
  add_pptx_sl_title(pptx, slide_title)
}

#' Add a title slide to the rpptx document
#' @param pptx rpptx object
#' @param title Title text
#' @importFrom officer add_slide ph_with ph_location_type
#' @return rpptx object with title slide added
#' @keywords internal
add_pptx_sl_title <- function(pptx, title) {
  add_slide(pptx, layout = "Title Slide", master = "Office Theme") |>
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
add_pptx_sl_plottable <- function(pptx, df, plot, plot_title = NULL, table_title = "") {
  # Add a slide containing a plot and a table. Optionally include a title for the plot/table.
  slide <- add_slide(pptx, layout = "Content with Caption")
  if (!is.null(plot_title) && nzchar(plot_title)) {
    slide <- ph_with(slide, value = plot_title, location = ph_location_type(type = "title"))
  }
  slide <- ph_with(slide, value = plot, location = "Content Placeholder 1")
  # If a table title is provided, use it for the Title placeholder instead of empty
  if (nzchar(table_title)) {
    slide <- ph_with(slide, value = flextable::flextable(df, cwidth = 1), location = "Table Placeholder 1") |>
      ph_with(value = table_title, location = "Title 1")
  } else {
    slide <- ph_with(slide, value = flextable::flextable(df, cwidth = 1), location = "Table Placeholder 1")
  }
  slide
}

#' Add a slide with a table only
#' @param pptx rpptx object
#' @param df Data frame to show as table
#' @param title Title text for the slide
#' @param footer Footer text for the slide
#' @importFrom officer add_slide ph_with
#' @importFrom flextable flextable
#' @return rpptx object with slide added
add_pptx_sl_table <- function(pptx, df, title = "", footer = "Click here for individual results") {
  add_slide(pptx, layout = "Title Only") |>
    ph_with(value = flextable::flextable(df, cwidth = 1), location = "Table Placeholder 1") |>
    ph_with(value = title, location = "Title 1") |>
    ph_with(value = footer, location = "Footer Placeholder 3")
}

#' Add a slide with a plot only
#' @param pptx rpptx object
#' @param plot ggplot object to show as plot
#' @importFrom officer add_slide ph_with
#' @return rpptx object with slide added
add_pptx_sl_plot <- function(pptx, plot, title = NULL) {
  # Add a slide with an optional title plus a plot
  slide <- add_slide(pptx, layout = "Picture with Caption")
  if (!is.null(title) && nzchar(title)) {
    slide <- ph_with(slide, value = title, location = ph_location_type(type = "title"))
  }
  ph_with(slide, value = plot, location = "Picture Placeholder 2")
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
  pptx <- create_pptx_doc(path, title, template)

  lst_group_slide <- 1
  group_slides <- numeric()
  for (i in seq_len(length(res_dose_slides))) {

    # Generate the individual figures
    # For the individual table slide, keep existing behaviour but keep title consistent with new naming
    pptx <- add_pptx_sl_table(
      pptx, res_dose_slides[[i]]$info,
      title = paste0("Group, ", i, " (individual)"),
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
    # Prepare info table: remove STUDYID, PCSPEC, DRUG, PARAM from the table and build a title
    info_df <- res_dose_slides[[i]]$info
    cols_to_remove <- intersect(c("STUDYID", "PCSPEC", "DRUG", "PARAM"), names(info_df))
    title_values <- ""
    if (length(cols_to_remove) > 0) {
      val_list <- vapply(cols_to_remove, function(col) {
        paste(unique(na.omit(info_df[[col]])), collapse = ",")
      }, FUN.VALUE = "")
      title_values <- paste(val_list[val_list != ""], collapse = ",")
    }
    # Table title will include the group id and the concatenated values
    table_title <- if (nzchar(title_values)) paste0("Group, ", i, " — ", title_values) else paste0("Group, ", i)
    # Remove the identified columns from the table shown
    info_table_df <- if (length(cols_to_remove) > 0) select(info_df, -any_of(cols_to_remove)) else info_df

    pptx <- add_pptx_sl_table(pptx, info_table_df, table_title) |>
      ph_slidelink(ph_label = "Footer Placeholder 3", slide_index = (lst_group_slide + 1)) |>
      add_pptx_sl_plottable(
        df = res_dose_slides[[i]]$statistics,
        plot = res_dose_slides[[i]]$meanplot,
        plot_title = paste0("Mean plot — Group, ", i)
      ) |>
      add_pptx_sl_plot(res_dose_slides[[i]]$linplot, title = paste0("PK parameter plot — Group, ", i)) |>
      add_pptx_sl_plot(res_dose_slides[[i]]$boxplot, title = paste0("PK parameter boxplot — Group, ", i))

    n_ind <- length(res_dose_slides[[i]]$ind_params)
    lst_group_slide <- lst_group_slide + 1 + n_ind + 4
    group_slides <- c(group_slides, (lst_group_slide - 3):(lst_group_slide))
  }

  group_slides_rev <- rev(group_slides) + (seq_len(length(group_slides)) - 1)
  pptx <- purrr::reduce(
    group_slides_rev,
    function(pptx, slide_index) move_slide(pptx, index = slide_index, to = 2),
    .init = pptx
  )
  pptx <- add_pptx_sl_title(pptx, "Extra Figures")
  pptx <- move_slide(x = pptx, index = length(pptx), to = (length(group_slides) + 2))

  print(pptx, target = path)
  invisible(TRUE)
}
