#' Summary Statistics Module
#'
#' This module provides a user interface and server function for creating and displaying
#' summary statistics tables.
#'
#' @param id A character string used to uniquely identify the module.
#' @param res_nca A reactive expression that returns the NCA results. Used only
#'   to derive default grouping columns from the PKNCA object.
#' @param grouping_vars A reactive expression that returns the grouping variables.
#' @param adpp A reactive expression that returns the ADPP dataset. This is the
#'   single source for summary statistics: exclusions are already applied
#'   (PPSUMXF) and all grouping columns are present as regular columns.
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
descriptive_statistics_server <- function(id, res_nca, grouping_vars, adpp) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Grouping columns available for summary statistics, all sourced from ADPP.
    # Priority order: PKNCA group columns (minus subject), then classification
    # columns, then the subject column.
    adpp_group_cols <- reactive({
      req(res_nca())
      req(adpp())
      adpp_cols <- names(adpp())

      subj_col <- res_nca()$data$conc$columns$subject
      group_cols <- setdiff(
        unname(unlist(res_nca()$data$conc$columns$groups)), subj_col
      )
      classification_cols <- sort(c(grouping_vars(), "DOSEA", "ATPTREF"))

      # Keep only columns that actually exist in ADPP.
      group_cols <- intersect(group_cols, adpp_cols)
      classification_cols <- intersect(classification_cols, adpp_cols)
      subj_col <- intersect(subj_col, adpp_cols)

      list(
        group_cols = group_cols,
        classification_cols = classification_cols,
        subj_col = subj_col
      )
    })

    # Update the input for the group by picker
    observeEvent(adpp_group_cols(), {
      gc <- adpp_group_cols()
      group_cols <- gc$group_cols
      classification_cols <- gc$classification_cols
      subj_col <- gc$subj_col

      grouping_vars <- c(group_cols, classification_cols, subj_col)
      initial_selection <- unique(c(group_cols, intersect("ATPTREF", classification_cols)))

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
                        choices = grouping_vars,
                        selected = initial_selection)

    })

    # Reactive expression for summary table based on selected group and parameters.
    # ADPP is the single source: exclusions are already applied via PPSUMXF and
    # all grouping columns are present, so no join from concentration data is
    # needed.
    summary_stats <- reactive({
      req(adpp())
      gc <- adpp_group_cols()

      # Fall back to default grouping when the picker hasn't rendered yet
      selected_groupby <- input$summary_groupby
      if (is.null(selected_groupby)) {
        selected_groupby <- unique(c(
          gc$group_cols, intersect("ATPTREF", gc$classification_cols)
        ))
      }

      stats_data <- adpp()
      # ADPP stores PPORRES as CDISC character; calculate_summary_stats needs
      # it numeric. Recover the numeric value from PPSTRESN when available,
      # else coerce PPORRES.
      if ("PPSTRESN" %in% names(stats_data)) {
        stats_data$PPORRES <- suppressWarnings(as.numeric(stats_data$PPSTRESN))
        if ("PPSTRESU" %in% names(stats_data)) {
          stats_data$PPORRESU <- stats_data$PPSTRESU
        }
      } else if ("PPORRES" %in% names(stats_data)) {
        stats_data$PPORRES <- suppressWarnings(as.numeric(stats_data$PPORRES))
      }

      # Exclude records flagged for summary exclusion (flag rules + manual).
      if ("PPSUMXF" %in% names(stats_data)) {
        stats_data <- stats_data[
          is.na(stats_data$PPSUMXF) | stats_data$PPSUMXF != "Y", ,
          drop = FALSE
        ]
      }

      # Partial intervals share a PPTESTCD in ADPP but differ by PPSTINT/
      # PPENINT (ISO durations). Suffix the interval bounds so they are treated
      # as distinct parameters in the summary, matching the results view.
      if (all(c("PPSTINT", "PPENINT") %in% names(stats_data))) {
        has_int <- !is.na(stats_data$PPSTINT) & stats_data$PPSTINT != ""
        stats_data$PPTESTCD[has_int] <- paste0(
          stats_data$PPTESTCD[has_int], "_",
          stats_data$PPSTINT[has_int], "-", stats_data$PPENINT[has_int]
        )
      }

      # Calculate summary stats and filter by selected parameters
      calculate_summary_stats(stats_data, selected_groupby)
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

    observeEvent(summary_stats(), {
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
