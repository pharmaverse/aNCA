# nca_results UI Module
nca_results_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "NCA Results",
    pickerInput(
      ns("params"),
      "Select Parameters :",
      choices = list("Run NCA first" = ""),
      selected = list("Run NCA first" = ""),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    units_table_ui(ns("units_table")),
    reactableOutput(ns("myresults")),
    downloadButton(ns("local_download_NCAres"), "Download locally the NCA Data")
  )
}

# nca_results Server Module
nca_results_server <- function(id, pknca_data, res_nca, rules, grouping_vars, auc_options) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    units_table_server(
      "units_table",
      reactive({          #' Pass `pknca_data` to the units table only when the results
        req(res_nca())    #' are available.
        pknca_data()
      })
    )

    final_results <- reactive({
      req(res_nca())
      res <- res_nca()

      #' Apply units
      if (!is.null(session$userData$units_table())) {
        res$data$units <- session$userData$units_table()
        res$result <- res$result %>%
          select(-PPSTRESU, -PPSTRES) %>%
          left_join(
            session$userData$units_table() %>%
              mutate(PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD")),
            by = intersect(names(.), names(session$userData$units_table()))
          ) %>%
          mutate(PPSTRES = PPORRES * conversion_factor) %>%
          select(-conversion_factor)
      }

      #' Transform results
      # Calculate bioavailability if available
      results <- calculate_F(res, auc_options()) %>%
        PKNCA_add_F(res, .)

      # Transform results
      final_results <- pivot_wider_pknca_results(results)

      # Apply flag rules
      current_rules <- rules()
      for (param in names(current_rules)) {
        if (current_rules[[param]]$is.checked) {
          # Find the proper column/s that should be considered (in principle should be 1)
          param_cdisc <- translate_terms(param, "PKNCA", "PPTESTCD")
          param_cols <- grep(paste0("^", param_cdisc, "(\\[.*\\])?$"),
                             names(final_results),
                             value = TRUE)

          # Include a flag column for that parameter that is TRUE when the threshold is surpassed
          final_results <- final_results %>%
            mutate(!!paste0("flag_", param) := case_when(
              startsWith(param, "auc") ~ rowSums(
                .[, param_cols] >= current_rules[[param]]$threshold
              ) > 0,
              TRUE ~ rowSums(.[, param_cols] <= current_rules[[param]]$threshold) > 0
            ))
        }
      }

      # Join subject data to allow the user to group by it
      conc_data_to_join <- res_nca()$data$conc$data %>%
        select(any_of(c(
          grouping_vars(),
          unname(unlist(res_nca()$data$conc$columns$groups)),
          "DOSEA",
          "NCA_PROFILE",
          "ROUTE"
        )))

      final_results <- final_results %>%
        inner_join(conc_data_to_join, by = intersect(names(.), names(conc_data_to_join))) %>%
        distinct()

      # Add flagged column
      final_results %>%
        mutate(
          flagged = case_when(
            rowSums(is.na(select(., starts_with("flag_")))) > 0 ~ "MISSING",
            rowSums(select(., starts_with("flag_")), na.rm = TRUE) > 0 ~ "FLAGGED",
            TRUE ~ "ACCEPTED"
          )
        )
    })

    observeEvent(final_results(), {
      req(final_results())

      param_pptest_cols <- intersect(unname(var_labels(final_results())), pknca_cdisc_terms$PPTEST)
      param_inputnames <- translate_terms(param_pptest_cols, "PPTEST", "input_names")

      updatePickerInput(
        session = session,
        inputId = "params",
        label = "Select Parameters :",
        choices = sort(param_inputnames),
        selected =  param_inputnames
      )
    })

    output_results <- reactive({
      req(final_results(), input$params)

      # Select columns of parameters selected, considering each can have multiple diff units
      param_cols <- unique(res_nca()$result$PPTESTCD)
      input_params <- sub(":.*", "", input$params)
      #identify parameters to be removed from final results
      params_rem_cols <- setdiff(param_cols, input_params)

      col_names <- names(final_results())
      # Extract base names before the "[", or leave as-is if no "["
      col_base_names <- ifelse(str_detect(col_names, "\\["),
                               str_remove(col_names, "\\[.*"),
                               col_names)

      final_results() %>%
        select(c(all_of(col_names[!(col_base_names %in% params_rem_cols)])))
    })

    output$myresults <- reactable::renderReactable({
      req(output_results())

      # Generate column definitions that can be hovered in the UI
      col_defs <- generate_col_defs(output_results())

      # Make the reactable object
      reactable(
        output_results(),
        columns = col_defs,
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        resizable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        bordered = TRUE,
        height = "68vh",
        rowStyle = function(index) {
          flagged_value <- output_results()$flagged[index]
          if (flagged_value == "FLAGGED") {
            list(backgroundColor = "#f5b4b4")
          } else if (flagged_value == "MISSING") {
            list(backgroundColor = "#cbaddd")
          } else {
            NULL
          }
        }
      )
    })

    output$local_download_NCAres <- downloadHandler(
      filename = function() {
        paste0(res_nca()$data$conc$data$STUDYID[1], "PK_Parameters.csv")
      },
      content = function(file) {
        write.csv(output_results(), file, row.names = FALSE)
      }
    )
  })
}
