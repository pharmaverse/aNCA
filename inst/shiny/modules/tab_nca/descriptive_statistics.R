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
      
      formatted_choices <- reactive({
        req(metadata_nca_variables)
        
        # Taking the variables and labels from the metadata
        choices_df <- metadata_nca_variables %>%
          select(Variable, Label) %>%
          distinct(Variable, .keep_all = TRUE) %>%
          filter(!is.na(Variable), Variable != "") %>%
          filter(Variable %in% grouping_vars)
        
        unname(purrr::pmap(list(choices_df$Variable, choices_df$Label), function(var, lab) {
          list(
            label = as.character(var),
            value = as.character(var),
            description = as.character(lab)
          )
        }))
      })
      
      # Rendering the colorby selector
      output$groupby_ui_wrapper <- renderUI({
        req(formatted_choices())
        grouping_vars <- formatted_choices()
        
        shinyWidgets::virtualSelectInput(
          inputId = ns("summary_groupby"),
          label = "Group by variables:",
          choices = grouping_vars,
          multiple = TRUE,
          selected = initial_selection,
          search = TRUE,
          hasOptionDescription = TRUE
        )
      })
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
        filter(Statistic %in% input$select_display_statistic)
    })

    observeEvent(res_nca(), {
      req(summary_stats())
      
      # Get the statistics variables needed
      params_needed <- setdiff(colnames(summary_stats()), c("Statistic", input$summary_groupby))
      clean_params_needed <- gsub("\\[.*", "", params_needed)
      
      
      # Generate dataset for parameters and labels in the dropdowns
      formatted_choices <- reactive({
        req(metadata_nca_parameters)
        
        # Taking the parameters and labels from the metadata
        choices_df <- metadata_nca_parameters %>%
          select(PPTESTCD, PPTEST) %>%
          distinct(PPTESTCD, .keep_all = TRUE) %>%
          filter(!is.na(PPTESTCD), PPTESTCD != "") %>%
          filter(PPTESTCD %in% clean_params_needed)
        print(choices_df)
        
        unname(purrr::pmap(list(params_needed, choices_df$PPTEST), function(var, lab) {
          list(
            label = as.character(var),
            value = as.character(var),
            description = as.character(lab)
          )
        }))
      })
      
      # Rendering the parameters to display selector
      output$param_to_display_ui_wrapper <- renderUI({
        req(summary_stats())
        params_to_display <- formatted_choices()
        
        shinyWidgets::virtualSelectInput(
          inputId = ns("select_display_parameters"),
          label = "Parameter to display:",
          choices = params_to_display,
          multiple = TRUE,
          selected = params_needed,
          search = TRUE,
          hasOptionDescription = TRUE
        )
      })

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
      session$userData$results$nca_results$descriptive_statistics <- summary_stats()
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

