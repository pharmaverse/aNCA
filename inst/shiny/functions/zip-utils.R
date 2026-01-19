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

save_output <- function(output, output_path, ggplot_formats = c("png", "html"), table_formats = c("csv", "rds", "xpt"), obj_names = NULL) {
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  for (name in names(output)) {
    file_name <- paste0(output_path, "/", name)
    if (!dir.exists(file_name)) {
      dir.create(file_name, recursive = TRUE)
    }
    x <- output[[name]]
    is_obj_to_export <- is.null(obj_names) || name %in% obj_names

    if (inherits(x, "list")) {
      save_output(output = x, output_path = file_name, ggplot_formats = ggplot_formats, table_formats = table_formats, obj_names = obj_names)
    } else if (inherits(x, "ggplot") && is_obj_to_export) {
      save_ggplot_format(x, file_name, ggplot_formats)
    } else if (inherits(x, "data.frame") && is_obj_to_export) {
      save_table_format(x, file_name, table_formats)
    } else if (inherits(x, "plotly") && is_obj_to_export) {
      save_plotly_format(x, file_name, "html")
    } else if (is_obj_to_export) {
      stop(
        "Unsupported output type object in the list: ",
        paste0(class(x), collapse = ", ")
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

    linplot_data_i <- process_data_individual(
      data = d_conc_i,
      selected_analytes = d_conc_i[[analyte_col]],
      selected_pcspec = d_conc_i[[pcspec_col]],
      selected_usubjids = d_conc_i[[subj_col]],
      profiles_selected = unique(d_conc_i[[profile_col]]),
      ylog_scale = TRUE
    )

    linplot_i <- g_lineplot(
      data = linplot_data_i,
      x_var = "ARRLT",
      y_var = "AVAL",
      color_by = subj_col,
      facet_by = facet_vars,
      ylog_scale = TRUE
    )

    meanplot_data_i <- process_data_mean(
      data = d_conc_i,
      selected_analytes = unique(d_conc_i[[analyte_col]]),
      selected_pcspec = unique(d_conc_i[[pcspec_col]]),
      profiles_selected = unique(d_conc_i[[profile_col]]),
      ylog_scale = TRUE,
      facet_by = facet_vars,
      color_by = group_by_vars
    )

    meanplot_i <- g_lineplot(
      data = meanplot_data_i,
      x_var = "NRRLT",
      y_var = "Mean",
      facet_by = facet_vars,
      color_by = group_by_vars,
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
      summarise(n = n())

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
        d_conc_processed_i <- process_data_individual(
          data = d_conc_i,
          selected_analytes = d_conc_i[[analyte_col]],
          selected_pcspec = d_conc_i[[pcspec_col]],
          selected_usubjids = d_conc_i[[subj_col]],
          ylog_scale = TRUE
        )

        g_lineplot(
          data = d_conc_processed_i,
          x_var = "AFRLT",
          y_var = "AVAL",
          color_by = subj_col,
          facet_by = facet_vars,
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
