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
      reactableOutput(ns("descriptive_stats"))
    ),
    card(
      downloadButton(ns("download_browser"), "Download the NCA Summary Data")
    )
  )
}

# Server function for the summary statistics module
descriptive_statistics_server <- function(id, res_nca, grouping_vars, auc_options) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update the input for the group by picker
    observeEvent(res_nca(), {
      req(res_nca())

      group_cols <- setdiff(unname(unlist(res_nca()$data$conc$columns$groups)),
                            "USUBJID")
      classification_cols <- sort(c(grouping_vars(), "DOSEA", "DOSNO"))
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

      bioavailability <- calculate_bioavailability(res_nca(), auc_options())
      results <- bioavailability_in_pkncaresult(res_nca(), bioavailability)

      # Join subject data to allow the user to group by it
      cols_to_join <- c(classification_cols, unname(unlist(res_nca()$data$conc$columns$groups)))
      stats_data <- inner_join(
        results$result,
        select(res_nca()$data$conc$data, any_of(cols_to_join))
      ) %>%
        filter(!(type_interval == "manual" & PPTESTCD != "aucint.last")) %>%
        mutate(PPTESTCD = ifelse(
          type_interval == "manual", paste0(PPTESTCD, signif(start), "-", signif(end)),
          PPTESTCD
        ))

      # Calculate summary stats and filter by selected parameters
      calculate_summary_stats(stats_data, input$summary_groupby)
    })

    observeEvent(summary_stats(), {
      req(summary_stats())

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
    output$descriptive_stats <- renderReactable({
      req(summary_stats())
      log_info("Rendering descriptive statistics table")

      data <- summary_stats() %>%
        select(any_of(c(input$summary_groupby, "Statistic")), input$select_display_parameters) %>%
        filter(Statistic %in% input$select_display_statistic)

      reactable(
        data,
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = TRUE,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        bordered = TRUE
      )
    })

    # Download summary statistics as CSV
    output$download_browser <- downloadHandler(
      filename = function() {
        paste("NCA_summary.csv")
      },
      content = function(file) {
        log_info("Downloading summary statistics as CSV")
        write.csv(summary_stats(), file)
      }
    )

  })
}
