#' Summary Statistics Module
#'
#' This module provides a user interface and server function for creating and displaying
#' summary statistics tables.
#'
#' @param id A character string used to uniquely identify the module.
#' @param res_nca A reactive expression that returns the NCA results.
#' @param grouping_vars A reactive expression that returns the grouping variables.
#'
#' @returns A list containing the reactive expression for the summary statistics table.

# UI function for the summary statistics module
descriptive_statistics_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("param_to_display_ui_wrapper")
    ),
    pickerInput(
      inputId = ns("select_display_statistic"),
      label = "Statistic to display:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    uiOutput(ns("groupby_ui_wrapper")
    ),
    card(reactable_ui(ns("descriptive_stats")), class = "border-0 shadow-none"),
    card(
      downloadButton(ns("download_summary"), "Download the NCA Summary Data")
    )
  )
}

# Server function for the summary statistics module
descriptive_statistics_server <- function(id, res_nca, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update the input for the group by picker
    observeEvent(res_nca(), {
      req(res_nca())

      subj_col <- res_nca()$data$conc$columns$subject
      group_cols <- setdiff(unname(unlist(res_nca()$data$conc$columns$groups)),
                            # By default SUBJECT column is aggregated
                            subj_col)
      classification_cols <- sort(c(grouping_vars(), "DOSEA", "ATPTREF"))
      classification_cols <- classification_cols[
        classification_cols %in% names(res_nca()$data$conc$data)
      ]

      grouping_vars <- c(group_cols, classification_cols, subj_col)
      initial_selection <-  c(group_cols, classification_cols)

      # Rendering the group by selector
      selector_label(input = input,
                     output = output,
                     session = session,
                     choices = grouping_vars,
                     initial_selection = initial_selection,
                     selector_ui_wrapper = "groupby_ui_wrapper",
                     id = "summary_groupby",
                     label = "Group by variables:",
                     metadata_type = "variable")

      updatePickerInput(session, "summary_groupby",
                        choices = unique(c(group_cols, classification_cols, subj_col)),
                        selected = unique(c(group_cols, classification_cols)))

    })

    # Reactive expression for summary table based on selected group and parameters
    summary_stats <- reactive({
      req(res_nca())

      classification_cols <- sort(c(grouping_vars(), res_nca()$data$dose$columns$dose))
      classification_cols <- classification_cols[
        classification_cols %in% names(res_nca()$data$conc$data)
      ]

      results <- res_nca()

      # Join subject data to allow the user to group by it
      cols_to_join <- c(classification_cols, names(PKNCA::getGroups(results)))
      results_to_join <- res_nca()$data$conc$data %>%
        select(any_of(cols_to_join)) %>%
        distinct()
      stats_data <- inner_join(
        results$result,
        results_to_join,
        by = intersect(names(results$result), names(results_to_join)),
        relationship = "many-to-many"
      ) %>%
        # Exclude flagged records from summary statistics
        filter(is.na(exclude) | exclude == "") %>%
        # Rename manual interval parameters to include the range suffix
        # (e.g. AUCINT -> AUCINT_0-12) so they appear as distinct parameters
        rename_interval_params()

      # Calculate summary stats and filter by selected parameters
      calculate_summary_stats(stats_data, input$summary_groupby)
    })

    summary_stats_filtered <- reactive({
      # Map clean parameter names (e.g. "CMAX") back to actual column names
      # that include units (e.g. "CMAX[ng/mL]")
      all_cols <- colnames(summary_stats())
      selected_params <- input$select_display_parameters
      matched_cols <- all_cols[gsub("\\[.*", "", all_cols) %in% selected_params]

      summary_stats() %>%
        select(any_of(c(input$summary_groupby, "Statistic", matched_cols))) %>%
        filter(Statistic %in% input$select_display_statistic)
    })

    observeEvent(res_nca(), {
      req(summary_stats())

      # Get the statistics variables needed
      params_needed <- setdiff(colnames(summary_stats()), c("Statistic", input$summary_groupby))
      clean_params_needed <- gsub("\\[.*", "", params_needed)

      # Rendering the parameter to display variable
      selector_label(input = input,
                     output = output,
                     session = session,
                     choices = clean_params_needed,
                     initial_selection = clean_params_needed,
                     selector_ui_wrapper = "param_to_display_ui_wrapper",
                     id = "select_display_parameters",
                     label = "Parameter to display:",
                     metadata_type = "parameter")

      # Update the select display statistics picker input
      updatePickerInput(
        session,
        "select_display_statistic",
        choices = unique(summary_stats()$Statistic),
        selected = unique(summary_stats()$Statistic)
      )
    })

    # Save the updates of the object for the ZIP file
    observeEvent(summary_stats(), {
      session$userData$results$nca_results$nca_statistics <- summary_stats()
    })

    # Render the reactive summary table in a data table
    reactable_server(
      "descriptive_stats",
      summary_stats_filtered,
      defaultPageSize = 10,
      pageSizeOptions = reactive(c(10, 25, 50, 100, nrow(summary_stats_filtered())))
    )

    # Download summary statistics as CSV
    output$download_summary <- downloadHandler(
      filename = function() {
        paste0(
          session$userData$project_prefix("-"),
          "NCA_summary_",
          format(Sys.time(), "%Y-%m-%d"), ".csv"
        )
      },
      content = function(file) {
        log_info("Downloading summary statistics as CSV")
        write.csv(summary_stats_filtered(), file)
      }
    )
  })
}
