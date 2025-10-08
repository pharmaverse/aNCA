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

#' Generate Dose Escalation Results
#'
#' This function processes PKNCA results and concentration data to produce summary statistics,
#' plots, and individual subject outputs for dose escalation studies.
#'
#' @param o_nca NCA analysis object containing results and concentration data.
#' @param group_by_vars Character vector of column names to group results by.
#' @param statistics Character vector of summary statistics to include (default: "Mean").
#' @param facet_vars Character vector of column names to facet plots by (default: "DOSEA").
#' @param stats_parameters Character vector of parameter codes to summarize (default: c("CMAX", "TMAX", ...)).
#' @param info_vars Character vector of additional info columns to include (default: c("SEX", "STRAIN", ...)).
#'
#' @return A list containing plots, summary statistics, info tables, and individual subject outputs for each group.
#' @export
get_dose_esc_results <- function(
  o_nca, group_by_vars,
  statistics = "Mean",
  facet_vars = "DOSEA",
  stats_parameters = c("CMAX", "TMAX", "VSS", "CLSTP", "LAMZHL", "AUCIFO", "AUCLST", "FABS"),
  boxplot_parameter = "AUCIFO",
  info_vars = c("SEX", "STRAIN", "RACE", "DOSFRM")
) {

  groups <- unique(o_nca$data$intervals[, group_by_vars])
  output_list <- list()
  o_nca_i <- o_nca
  # Loop over each of the groups
  for (i in seq_len(nrow(groups))) {
    group_i <- groups[i, , drop = FALSE]
    d_conc_i <- merge(o_nca$data$conc$data, group_i)
    o_res_i <- merge(o_nca$result, group_i)
    o_nca_i$result <- o_res_i

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

    info_i <- merge(o_nca$data$conc$data, group_i) %>%
      select(any_of(unique(c(group_by_vars, info_vars)))) %>%
      unique()

    boxplot_i <- flexible_violinboxplot(
      res_nca = o_nca_i,
      parameter = boxplot_parameter,
      xvars = facet_vars,
      colorvars = "ANALYTE",
      varvalstofilter = NULL,
      box = TRUE,
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
        general_lineplot(
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
      })

    output_list[[paste0("Group_", i)]] <- list(
      linplot = linplot_i,
      meanplot = meanplot_i,
      statistics = stats_i,
      boxplot = boxplot_i,
      info = info_i,
      ind_params = ind_params,
      ind_plots = ind_plots
    )
  }
  output_list
}
