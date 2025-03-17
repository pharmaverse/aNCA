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
    units_table_ui(ns("units_table_postNCA")),
    reactableOutput(ns("myresults")),
    downloadButton(ns("local_download_NCAres"), "Download locally the NCA Data")
  )
}

# nca_results Server Module
nca_results_server <- function(id, res_nca, rules, grouping_vars, auc_options) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    final_results <- reactive({
      req(res_nca())
      # Transform results
      final_results <- pivot_wider_pknca_results(res_nca())

      # Calculate bioavailability if available
      bioavailability <- calculate_bioavailability(res_nca(), auc_options())

      # Extract ID groups
      id_groups <- res_nca()$data$conc$columns$groups %>%
        purrr::list_c() %>%
        append("DOSNO") %>%
        purrr::keep(~ !is.null(.) && . != "DRUG" &&
                      length(unique(res_nca()$data$conc$data[[.]])) > 1)

      final_results <- final_results %>%
        mutate(Grouping_EX = apply(select(., all_of(id_groups), -USUBJID),
                                   1, paste, collapse = " ")) %>%
        left_join(bioavailability, by = c("USUBJID", "Grouping_EX")) %>%
        select(-Grouping_EX)

      # Apply rules
      for (rule_input in grep("^rule_", names(rules), value = TRUE)) {
        if (!rules[[rule_input]]) next

        pptestcd <- rule_input |>
          gsub("^rule_", "", x = _) |>
          gsub("_", ".", x = _, fixed = TRUE)

        final_pptestcd <- res_nca()$result %>%
          filter(PPTESTCD == pptestcd) %>%
          slice(1) %>%
          mutate(new_pptestcd = paste0(pptestcd, "[", PPSTRESU, "]")) %>%
          pull(new_pptestcd) %>%
          unique()

        final_results <- final_results %>%
          mutate(!!paste0("flag_", pptestcd) := case_when(
            startsWith(pptestcd, "auc") ~ .data[[final_pptestcd]]
            >= rules[[paste0(pptestcd, "_threshold")]],
            TRUE ~ .data[[final_pptestcd]] <= rules[[paste0(pptestcd, "_threshold")]]
          ))
      }

      # Join subject data to allow the user to group by it
      final_results <- final_results %>%
        inner_join(
          res_nca()$data$conc$data %>%
            select(
              any_of(c(grouping_vars(),
                       unname(unlist(res_nca()$data$conc$columns$groups)),
                       "DOSEA",
                       "DOSNO",
                       "ROUTE"))
            )
        ) %>%
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
      param_cols <- c(unique(res_nca()$result$PPTESTCD), "Exclude", "flagged")

      updatePickerInput(
        session = session,
        inputId = "params",
        label = "Select Parameters :",
        choices = sort(param_cols),
        selected = sort(param_cols)
      )
    })

    output$myresults <- reactable::renderReactable({
      req(final_results(), input$params)

      # Select columns of parameters selected, considering each can have multiple diff units
      param_label_cols <- formatters::var_labels(final_results())
      param_cols <- c(unique(res_nca()$result$PPTESTCD), "Exclude", "flagged")
      remove_params <- setdiff(input$params, param_cols)
      #identify parameters to be removed from final results
      params_rem_cols <- param_label_cols[param_label_cols %in% remove_params] |>
        names()

      final_results <- final_results() %>%
        select(-(c(any_of(params_rem_cols), conc_groups)))

      # Generate column definitions that can be hovered in the UI
      col_defs <- generate_col_defs(final_results)

      # Make the reactable object
      reactable(
        final_results,
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
          flagged_value <- final_results$flagged[index]
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
        write.csv(final_res_nca(), file, row.names = FALSE)
      }
    )
  })
}
