# nca_results UI Module
nca_results_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
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
    DTOutput(ns("myresults")),
    tableOutput(ns("summaryTable")),
    actionButton(ns("download"), "Download the NCA Data"),
    downloadButton(ns("local_download_NCAres"), "Download locally the NCA Data")
  )
}

# nca_results Server Module
nca_results_server <- function(id, res_nca) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    final_res_nca <- reactiveVal(NULL)
    
    observeEvent(res_nca(), {
      req(res_nca())
      
      # Transform results
      final_results <- pivot_wider_pknca_results(res_nca())
      
      # Apply rules
      for (rule_input in grep("^rule_", names(input), value = TRUE)) {
        if (!input[[rule_input]]) next
        
        pptestcd <- rule_input |>
          gsub("^rule_", "", x = _) |>
          gsub("_", ".", x = _, fixed = TRUE)
        
        if (startsWith(pptestcd, "auc")) {
          final_results[[paste0("flag_", pptestcd)]] <- final_results[[pptestcd]] >= input[[paste0(pptestcd, "_threshold")]]
        } else {
          final_results[[paste0("flag_", pptestcd)]] <- final_results[[pptestcd]] <= input[[paste0(pptestcd, "_threshold")]]
        }
      }
      
      # Include units in column names
      dict_pttestcd_with_units <- res_nca()$result %>%
        select(PPTESTCD, PPSTRESU) %>%
        unique() %>%
        pull(PPSTRESU, PPTESTCD)
      
      final_results <- final_results %>%
        rename_with(~ifelse(
          gsub("_.*", "", .x) %in% names(dict_pttestcd_with_units),
          paste0(.x, "[", dict_pttestcd_with_units[gsub("_.*", "", .x)], "]"),
          .x
        ))

      # Sort columns
      group_cols <- c(unname(unique(c(unlist(res_nca()$data$conc$columns$groups),
                                      unlist(res_nca()$data$dose$columns$groups)))))
      int_cols <- c("DOSNO", "start", "end")
      param_cols <- names(final_results)[endsWith(names(final_results), "]")]
      other_cols <- setdiff(names(final_results), c(group_cols, int_cols, param_cols))
      
      final_results <- final_results %>%
        dplyr::select(any_of(c(group_cols, int_cols, param_cols, other_cols)))
      
      # Add flagged column
      final_results <- final_results %>%
        mutate(
          flagged = case_when(
            rowSums(is.na(select(., starts_with("flag_")))) > 0 ~ "MISSING",
            rowSums(select(., starts_with("flag_")), na.rm = TRUE) > 0 ~ "FLAGGED",
            TRUE ~ "ACCEPTED"
          )
        )
      
      final_res_nca(final_results)
      
      updatePickerInput(
        session = session,
        inputId = "params",
        label = "Select Parameters :",
        choices = sort(colnames(final_res_nca())),
        selected = sort(colnames(final_res_nca()))
      )
    })
    
    output$myresults <- DT::renderDataTable({
      req(final_res_nca())
      DT::datatable(
        data = final_res_nca(),
        extensions = "FixedHeader",
        options = list(
          scrollX = TRUE,
          scrollY = TRUE,
          lengthMenu = list(c(10, 25, -1), c("10", "25", "All")),
          fixedHeader = TRUE,
          columnDefs = list(list(
            visible = FALSE, targets = setdiff(colnames(final_res_nca()), input$params)
          ))
        )
      ) %>%
        formatStyle(
          "flagged",
          target = "row",
          backgroundColor = styleEqual(c("FLAGGED", "MISSING"), c("#f5b4b4", "#cbaddd"))
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