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
    actionButton(ns("download"), "Download the NCA Data"),
    downloadButton(ns("local_download_NCAres"), "Download locally the NCA Data")
  )
}

# nca_results Server Module
nca_results_server <- function(id, res_nca, rules, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    final_results <- reactive({
      req(res_nca())

      # Transform results
      final_results <- pivot_wider_pknca_results(res_nca())

      # Apply rules
      for (rule_input in grep("^rule_", names(rules), value = TRUE)) {
        if (!rules[[rule_input]]) next

        print(rule_input)

        pptestcd <- rule_input |>
          gsub("^rule_", "", x = _) |>
          gsub("_", ".", x = _, fixed = TRUE)

        print(pptestcd)

        if (startsWith(pptestcd, "auc")) {
          final_results[[paste0("flag_", pptestcd)]] <-
            final_results[[pptestcd]] >= rules[[paste0(pptestcd, "_threshold")]]
        } else {
          final_results[[paste0("flag_", pptestcd)]] <-
            final_results[[pptestcd]] <= rules[[paste0(pptestcd, "_threshold")]]
        }
      }

      # Join subject data to allow the user to group by it
      final_results <- merge(
        final_results,
        res_nca()$data$conc$data %>%
          select(any_of(c(grouping_vars(), unname(unlist(res_nca()$data$conc$columns$groups)))))
      ) %>%
        unique()

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
      
      param_cols <- unique(res_nca()$result$PPTESTCD)
      
      updatePickerInput(
        session = session,
        inputId = "params",
        label = "Select Parameters :",
        choices = sort(param_cols),
        selected = sort(param_cols)
      )
      
    })
    
    final_res_nca <- reactive({
      req(final_results(), input$params)

      # Sort columns
      group_cols <- c(unname(unique(c(unlist(res_nca()$data$conc$columns$groups),
                                      unlist(res_nca()$data$dose$columns$groups)))))
      param_cols <- unique(res_nca()$result$PPTESTCD)
      int_cols <- c("DOSNO", "start", "end")
      other_cols <- setdiff(names(final_results), c(group_cols, int_cols, param_cols))
      id_cols <- grouping_vars()
      
      final_results <- final_results() %>%
        select(-all_of(setdiff(param_cols, input$params)))
      
      # Include units in column names
      dict_pttestcd_with_units <- res_nca()$result %>%
        select(PPTESTCD, PPSTRESU) %>%
        unique() %>%
        pull(PPSTRESU, PPTESTCD)
      
      final_results %>%
        rename_with(~ifelse(
          gsub("_.*", "", .x) %in% names(dict_pttestcd_with_units),
          paste0(.x, "[", dict_pttestcd_with_units[gsub("_.*", "", .x)], "]"),
          .x
        ))
    })
    
    output$myresults <- reactable::renderReactable({
      req(final_res_nca())
      
      col_defs <- generate_col_defs(final_res_nca())
      
      reactable(
        final_res_nca(),
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
          flagged_value <- final_res_nca()$flagged[index]
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

    observeEvent(input$download, {
      showModal(modalDialog(
        title = "Please enter the path to the folder on Improve for your results:",
        textInput(ns("pathresults"), "Path:"),
        actionButton(ns("go"), "GO"),
        footer = modalButton("Close")
      ))
    })

    output$local_download_NCAres <- downloadHandler(
      filename = function() {
        paste0(mydata()$conc$data$STUDYID[1], "PK_Parameters.csv")
      },
      content = function(file) {
        old_wd <- getwd()
        tempdir <- tempdir()
        setwd(tempdir)

        write.csv(final_res_nca(), file, row.names = FALSE)

        setwd(old_wd)
      }
    )
  })
}
