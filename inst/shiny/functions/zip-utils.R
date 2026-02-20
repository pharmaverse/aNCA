#' Save aNCA Result to the Output Directory
#'
#' @param output Output object, can be a data frame, plot or a list of them.
#' @param output_path Path to the output directory (should exist or be creatable).
#' @returns Invisibly returns the file path written.


# Helper for saving ggplot objects (multiple formats)
save_ggplot_format <- function(x, file_name, formats) {
  if ("png" %in% formats) {
    ggsave(paste0(file_name, ".png"), plot = x, width = 10, height = 6)
  }
  if ("html" %in% formats) {
    plotly_obj <- plotly::ggplotly(x)
    htmlwidgets::saveWidget(plotly_obj, file = paste0(file_name, ".html"))
  }
}

# Helper for saving data.frame objects (multiple formats)
save_table_format <- function(x, file_name, formats) {
  if ("csv" %in% formats) {
    write.csv(x, file = paste0(file_name, ".csv"), row.names = FALSE)
  }
  if ("rds" %in% formats) {
    saveRDS(x, file = paste0(file_name, ".rds"))
  }
  if ("xpt" %in% formats) {
    tryCatch(
      haven::write_xpt(format_to_xpt_compatible(x), paste0(file_name, ".xpt")),
      error = function(e) {
        message("Error writing XPT file for ", file_name, ": ", e$message)
      }
    )
  }
}

# Helper for saving plotly objects (only html for now)
save_plotly_format <- function(x, file_name, formats = "html") {
  if ("html" %in% formats) {
    htmlwidgets::saveWidget(x, file = paste0(file_name, ".html"))
  }
}

# Helper for saving different object types
save_dispatch <- function(x, file_name, ggplot_formats, table_formats) {
  if (inherits(x, "ggplot")) {
    save_ggplot_format(x, file_name, ggplot_formats)
  } else if (inherits(x, "data.frame")) {
    save_table_format(x, file_name, table_formats)
  } else if (inherits(x, "plotly")) {
    save_plotly_format(x, file_name, "html")
  } else {
    stop("Unsupported output type object in the list: ", paste0(class(x), collapse = ", "))
  }
}

# Check whether a name matches the export list (exact or numbered variant)
.is_exportable <- function(name, obj_names) {
  if (is.null(obj_names)) {
    TRUE
  } else if (name %in% obj_names) {
    TRUE
  } else {
    any(vapply(
      obj_names,
      function(n) grepl(paste0("^", n, "[0-9]+$"), name),
      logical(1)
    ))
  }
}

save_output <- function(
  output, output_path,
  ggplot_formats = c("png", "html"),
  table_formats = c("csv", "rds", "xpt"),
  obj_names = NULL
) {
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  for (name in names(output)) {
    x <- output[[name]]
    is_leaf <- inherits(x, "ggplot") || inherits(x, "data.frame") ||
      inherits(x, "plotly")

    if (!is_leaf && inherits(x, "list")) {
      file_name <- paste0(output_path, "/", name)
      if (!dir.exists(file_name)) {
        dir.create(file_name, recursive = TRUE)
      }
      save_output(
        output = x,
        output_path = file_name,
        ggplot_formats = ggplot_formats,
        table_formats = table_formats,
        obj_names = obj_names
      )
    } else if (.is_exportable(name, obj_names)) {
      save_dispatch(
        x = x,
        file_name = paste0(output_path, "/", name),
        ggplot_formats = ggplot_formats,
        table_formats = table_formats
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

#' Generate Dose Escalation Results
#'
#' This function processes PKNCA results and concentration data to produce summary statistics,
#' plots, and individual subject outputs for dose escalation studies.
#'
#' @param o_nca NCA analysis object containing results and concentration data.
#' @param group_by_vars Character vector of column names to group results by.
#' @param statistics Character vector of summary statistics to include (default: "Mean").
#' @param facet_vars Character vector of column names to facet plots by (default: "DOSEA").
#' @param stats_parameters Character vector of parameter codes to summarize
#' @param boxplot_parameter Character string of the parameter to use for boxplot.
#' @param info_vars Character vector of additional info columns to include
#' @param labels_df Data frame containing variable labels (default: metadata_nca_variables).
#'
#' @return A list containing dose escalation plots, summary statistics & info tables for each group.
get_dose_esc_results <- function(
  o_nca, group_by_vars,
  statistics = "Mean",
  facet_vars = "DOSEA",
  stats_parameters = c("CMAX", "TMAX", "VSSO", "CLSTP", "LAMZHL", "AUCIFO", "AUCLST", "FABS"),
  boxplot_parameter = "AUCIFO",
  info_vars = c("SEX", "STRAIN", "RACE", "DOSFRM"),
  labels_df = metadata_nca_variables
) {
  # Define column names
  studyid_col <- "STUDYID"
  subj_col <- o_nca$data$conc$columns$subject
  analyte_col <- o_nca$data$conc$columns$groups$group_analyte
  pcspec_col <- "PCSPEC"
  profile_col <- "ATPTREF"

  groups <- unique(o_nca$data$intervals[, c(group_by_vars, profile_col)])
  output_list <- list()
  o_nca_i <- o_nca
  # Loop over each of the groups
  for (i in seq_len(nrow(groups))) {
    group_i <- groups[i, , drop = FALSE]
    d_conc_i <- merge(o_nca$data$conc$data, group_i)
    o_res_i <- merge(o_nca$result, group_i)
    o_nca_i$result <- o_res_i

    linplot_i <- exploration_individualplot(
      pknca_data = o_nca_i$data,
      color_by = subj_col,
      facet_by = facet_vars,
      filtering_list = list(
        PARAM = d_conc_i[[analyte_col]],
        PCSPEC = d_conc_i[[pcspec_col]],
        USUBJID = d_conc_i[[subj_col]],
        ATPTREF = unique(d_conc_i[[profile_col]])
      ),
      ylog_scale = TRUE
    )

    meanplot_i <- exploration_meanplot(
      pknca_data = o_nca_i$data,
      color_by = group_by_vars,
      facet_by = facet_vars,
      filtering_list = list(
        PARAM = unique(d_conc_i[[analyte_col]]),
        PCSPEC = unique(d_conc_i[[pcspec_col]]),
        ATPTREF = unique(d_conc_i[[profile_col]])
      ),
      ylog_scale = TRUE,
      sd_max = TRUE
    )

    stats_i <- calculate_summary_stats(
      data = merge(o_res_i, d_conc_i[, c(group_vars(o_nca), facet_vars)]),
      input_groups = facet_vars
    ) %>%
      filter(
        Statistic %in% statistics
      ) %>%
      select(
        any_of(c(facet_vars, "Statistic")),
        any_of(names(.)[gsub("\\[.*\\]", "", names(.)) %in% stats_parameters])
      ) %>%
      unique()

    info_i <- merge(o_nca$data$conc$data, group_i) %>%
      # Group by cols from info vars that are in the data
      group_by(across(any_of(info_vars))) %>%
      summarise(n = n_distinct(!!sym(subj_col)), .groups = "drop")

    # Create character string of Group
    # Where group_by_vars are concatenated with ": " between label and value
    group_string <- merge(o_nca$data$conc$data, group_i) %>%
      mutate(group = apply(select(., any_of(c(group_by_vars, profile_col))), 1, function(x) {
        lbls <- sapply(names(x), function(v) get_label(v, type = "ADNCA", labels_df = labels_df))
        paste(lbls, x, sep = ": ", collapse = "\n")
      })) %>%
      pull(group) %>%
      unique()

    boxplot_i <- flexible_violinboxplot(
      res_nca = o_nca_i,
      parameter = boxplot_parameter,
      xvars = facet_vars,
      colorvars = analyte_col,
      varvalstofilter = NULL,
      box = TRUE,
      tooltip_vars = NULL,
      plotly = FALSE
    )

    ind_params <- merge(o_nca$result, group_i) %>%
      filter(PPTESTCD %in% stats_parameters) %>%
      mutate(parameter_unit = paste0(PPTESTCD, "[", PPSTRESU, "]")) %>%
      select(any_of(
        c(
          o_nca$data$conc$columns$subject,
          "start", "end", "parameter_unit", "PPSTRES"
        )
      )) %>%
      pivot_wider(names_from = parameter_unit, values_from = PPSTRES) %>%
      split(.[[o_nca$data$conc$columns$subject]])

    ind_plots <- merge(o_nca$data$conc$data, group_i) %>%
      split(.[[o_nca$data$conc$columns$subject]]) %>%
      lapply(function(d_conc_i) {
        exploration_individualplot(
          pknca_data = o_nca_i$data,
          color_by = subj_col,
          facet_by = facet_vars,
          filtering_list = list(
            PARAM = d_conc_i[[analyte_col]],
            PCSPEC = d_conc_i[[pcspec_col]],
            USUBJID = d_conc_i[[subj_col]]
          ),
          ylog_scale = TRUE
        )
      })

    output_list[[paste0("Group_", i)]] <- list(
      linplot = linplot_i,
      meanplot = meanplot_i,
      statistics = stats_i,
      boxplot = boxplot_i,
      info = info_i,
      ind_params = ind_params,
      ind_plots = ind_plots,
      group = group_string
    )
  }
  output_list
}

#' Create a tree structure from a named list, with 'text', 'id', and 'children' fields
#' @param x A named list
#' @param parent_id Internal use. Used to build unique ids for each node.
#' @return A list of nodes suitable for shinyWidgets::create_tree-like UI
create_tree_from_list_names <- function(x, parent_id = "tree") {
  if (!inherits(x, "list")) return(NULL)
  nms <- names(x)
  lapply(seq_along(nms), function(i) {
    nm <- nms[i]
    child <- x[[nm]]
    this_id <- paste0(parent_id, "_", i)
    node <- list(
      text = nm,
      id = this_id
    )
    if (inherits(child, "list") && !inherits(child, "data.frame")) {
      node$children <- create_tree_from_list_names(child, parent_id = this_id)
    }
    node
  })
}

#' Get All Leaf Node IDs from a Tree Structure
#' Recursively traverses a tree list (shinyWidgets object) to return its leaf nodes' IDs.
#' @param tree A list representing a tree structure,
#' where each node may have an 'id' and optionally 'children'.
#' @return A character vector of leaf node IDs.
get_tree_leaf_ids <- function(tree) {
  if (is.null(tree) || length(tree) == 0) return(character(0))
  ids <- character(0)
  for (node in tree) {
    if (!is.null(node$children) && length(node$children) > 0) {
      ids <- c(ids, get_tree_leaf_ids(node$children))
    } else if (!is.null(node$id)) {
      ids <- c(ids, node$id)
    }
  }
  ids
}

#' Prepare export files
#'
#' @param target_dir Path to the directory where files will be written.
#' @param res_nca NCA results object.
#' @param settings Settings object.
#' @param grouping_vars Reactive or list of grouping variables.
#' @param input Shiny input object from the zip module.
#' @param session Shiny session object.
prepare_export_files <- function(target_dir,
                                 res_nca,
                                 settings,
                                 grouping_vars,
                                 input,
                                 session,
                                 progress) {

  # Save Standard Outputs (Tables/Plots)
  progress$set(message = "Creating exports...",
               detail = "Saving tables and images...")
  # Include custom exploration plot names in the export list
  custom_names <- session$userData$exploration_custom_names
  obj_names <- unique(c(input$res_tree, custom_names))
  save_output(
    output = session$userData$results,
    output_path = target_dir,
    ggplot_formats = input$plot_formats,
    table_formats = input$table_formats,
    obj_names = obj_names
  )

  progress$inc(0.2)

  if ("results_slides" %in% input$res_tree) {
    progress$set(message = "Creating exports...",
                 detail = "Saving slideshow...")
    .export_slides(target_dir, res_nca, grouping_vars, input, session)
  }
  progress$inc(0.4)

  if ("settings_file" %in% input$res_tree) {
    progress$set(message = "Creating exports...",
                 detail = "Saving settings...")
    .export_settings(target_dir, session)
  }
  progress$inc(0.6)

  data_tmpdir <- file.path(target_dir, "data")
  dir.create(data_tmpdir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(session$userData$raw_data, file.path(data_tmpdir, "data.rds"))

  if ("r_script" %in% input$res_tree) {
    progress$set(message = "Creating exports...",
                 detail = "Saving R script...")
    .export_script(target_dir, session)
  }
  progress$inc(0.8)

  .clean_export_dir(target_dir, input, custom_names)
}

# Helpers to export different output types
#' Export slides helper
#' @param target_dir Target directory to save the slides.
#' @param res_nca NCA results object.
#' @param grouping_vars Grouping variables for dose escalation results.
#' @param input Shiny input object.
#' @param session Shiny session object.
#' @keywords internal
#' @noRd
.export_slides <- function(target_dir, res_nca, grouping_vars, input, session) {
  res_dose_slides <- get_dose_esc_results(
    o_nca = res_nca,
    group_by_vars = setdiff(group_vars(res_nca), res_nca$data$conc$columns$subject),
    facet_vars = "DOSEA",
    statistics = "Mean",
    stats_parameters = c("CMAX", "TMAX", "VSSO", "CLSTP", "LAMZHL", "AUCIFO", "AUCLST", "FABS"),
    info_vars = grouping_vars
  )

  path <- file.path(target_dir, "presentations")
  dir.create(path, showWarnings = FALSE)

  if ("qmd" %in% input$slide_formats) {
    create_qmd_dose_slides(
      res_dose_slides,
      file.path(path, "results_slides.qmd"),
      paste0("NCA Results\n", session$userData$project_name()),
      TRUE
    )
  }
  if ("pptx" %in% input$slide_formats) {
    create_pptx_dose_slides(
      res_dose_slides,
      file.path(path, "results_slides.pptx"),
      paste0("NCA Results\n", session$userData$project_name()),
      "www/templates/template.pptx"
    )
  }
}

#' Helper to export settings file
#' @param target_dir Target directory to save the settings
#' @param session Shiny session object
#' @keywords internal
#' @noRd
.export_settings <- function(target_dir, session) {
  path <- file.path(target_dir, "settings")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  settings_to_save <- list(
    settings = session$userData$settings(),
    slope_rules = session$userData$slope_rules$manual_slopes()
  )
  yaml::write_yaml(settings_to_save, paste0(path, "/settings.yaml"))
}

#' Helper to export R script
#' @param target_dir Target directory to save the R script
#' @param session Shiny session object
#' @keywords internal
#' @noRd
.export_script <- function(target_dir, session) {
  path <- file.path(target_dir, "code")
  template_path <- "shiny/www/templates/script_template.R"
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  get_session_code(
    template_path = system.file(template_path, package = "aNCA"),
    session,
    file.path(path, "session_code.R")
  )
}

#' Clean Export Directory
#' @param target_dir Target directory to clean
#' @param input Shiny input object
#' @keywords internal
#' @noRd
.clean_export_dir <- function(target_dir, input, custom_names = NULL) {
  all_files <- list.files(target_dir, recursive = TRUE, full.names = TRUE)

  exts <- c(input$table_formats, input$plot_formats, input$slide_formats, "yaml", "R")
  exts_patt <- paste0("((", paste0(exts, collapse = ")|("), "))$")
  fnames <- unique(c(input$res_tree, custom_names))
  fnames <- ifelse(fnames == "r_script", "session_code", fnames)
  fnames <- ifelse(fnames == "settings_file", "settings", fnames)
  # Match exact names and numbered variants (e.g. individualplot1, meanplot2)
  fnames_patt <- paste0("((", paste0(fnames, collapse = "[0-9]*)|("), "[0-9]*))")
  pattern <- paste0("/", fnames_patt, "\\.", exts_patt)
  files_req <- grep(pattern, all_files, value = TRUE)
  files_req <- c(files_req, grep("data/data.rds", all_files, value = TRUE))
  file.remove(all_files[!all_files %in% files_req])

  # Recursive directory cleanup
  dirs <- list.dirs(target_dir, recursive = TRUE, full.names = TRUE)
  for (d in dirs[rev(order(nchar(dirs)))]) {
    if (length(list.files(d, all.files = TRUE)) == 0 && d != target_dir) unlink(d, recursive = TRUE)
  }
}
