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
    pickerInput(
      inputId = ns("select_display_parameters"),
      label = "Parameter to display:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    pickerInput(
      inputId = ns("select_display_statistic"),
      label = "Statistic to display:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    card(
      orderInput(
        ns("summary_groupby_source"),
        "Drag and drop these variables...",
        items = "USUBJID",
        width = shiny::validateCssUnit("100%"),
        connect = ns("summary_groupby")
      ),
      orderInput(
        ns("summary_groupby"),
        "..to hierarchically group by (order matters!):",
        items = NULL,
        width = shiny::validateCssUnit("100%"),
        connect = ns("summary_groupby_source"),
        placeholder = "Drag items here to group hierarchically..."
      )
    ),
    card(
      reactable_ui(ns("descriptive_stats"))
    ),
    card(
      downloadButton(ns("download_summary"), "Download the NCA Summary Data")
    )
  )
}

# Server function for the summary statistics module
descriptive_statistics_server <- function(id, res_nca, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    # Update the input for the group by picker
    observeEvent(res_nca(), {
      req(res_nca())

      group_cols <- setdiff(unname(unlist(res_nca()$data$conc$columns$groups)),
                            # By default SUBJECT column is aggregated
                            res_nca()$data$conc$columns$subject)
      classification_cols <- sort(c(grouping_vars(), "DOSEA", "AVISIT"))
      classification_cols <- classification_cols[
        classification_cols %in% names(res_nca()$data$conc$data)
      ]

      updateOrderInput(session, "summary_groupby",
                       items = c(group_cols, classification_cols))
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
      results_to_join <- select(res_nca()$data$conc$data, any_of(cols_to_join))
      stats_data <- inner_join(
        results$result,
        results_to_join,
        by = intersect(names(results$result), names(results_to_join)),
        relationship = "many-to-many"
      ) %>%
        filter(type_interval != "manual")

      # Calculate summary stats and filter by selected parameters
      calculate_summary_stats(stats_data, input$summary_groupby)
    })

    summary_stats_filtered <- reactive({
      summary_stats() %>%
        select(any_of(c(input$summary_groupby, "Statistic")), input$select_display_parameters) %>%
        filter(Statistic %in% input$select_display_statistic) %>%
        apply_labels()
    })

    observeEvent(summary_stats(), {
      req(summary_stats())

      session$userData$results$nca_results$descriptive_statistics <- summary_stats()

      # Update the select display parameters picker input
      updatePickerInput(
        session,
        "select_display_parameters",
        choices = setdiff(colnames(summary_stats()), c("Statistic", input$summary_groupby)),
        selected = setdiff(colnames(summary_stats()), c("Statistic", input$summary_groupby))
      )

      # Update the select display parameters picker input
      updatePickerInput(
        session,
        "select_display_statistic",
        choices = unique(summary_stats()$Statistic),
        selected = unique(summary_stats()$Statistic)
      )
    })

    # Render the reactive summary table in a data table
    reactable_server(
      "descriptive_stats",
      summary_stats_filtered,
      pageSizeOptions = reactive(c(10, 25, 50, 100, nrow(summary_stats_filtered()))),
      defaultPageSize = 10,
      striped = TRUE,
      bordered = TRUE,
      compact = TRUE,
      style = list(fontSize = "0.75em")
    )

    # Download summary statistics as CSV
    output$download_summary <- downloadHandler(
      filename = function() {
        paste0(
          session$userData$project_name(), "-",
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
