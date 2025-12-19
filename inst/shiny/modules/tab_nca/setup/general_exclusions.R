  general_exclusions_ui <- function(id) {
    ns <- NS(id)
    tagList(
      h3("General Exclusions"),
      reactable_ui(ns("conc_table")),
      textInput(ns("exclusion_reason"), "Exclusion reason", ""),
      actionButton(ns("add_exclusion_reason"), "Add exclusion reason"),
      uiOutput(ns("exclusion_list_ui"))
    )
  }

  general_exclusions_server <- function(id, processed_pknca_data) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      exclusion_list <- reactiveVal(list())

      # Render the concentration data table with row selection
      conc_data <- reactive({
        req(processed_pknca_data())
        processed_pknca_data()$conc$data
      })

      reactable_server(
        "conc_table",
        conc_data,
        selection = "multiple",
        defaultPageSize = 25,
        style = list(fontSize = "0.75em"),
        rowStyle = function(x) {
          function(index) {
            # Get all indices from exclusion_list
            excl_indices <- unlist(lapply(exclusion_list(), function(x) x$rows))
            if (index %in% excl_indices) {
              return(list(background = "#FFCCCC")) # red
            }
            row <- x[index,]
            if (!is.null(row$nca_exclude) && nzchar(row$nca_exclude)) {
              return(list(background = "#FFFF99")) # yellow
            }
            return(NULL)
          }
        }
      )

      observeEvent(input$add_exclusion_reason, {
        browser()
        rows_sel <- getReactableState("conc_table-table", "selected")
        reason <- input$exclusion_reason
        if (length(rows_sel) > 0 && nzchar(reason)) {
          current <- exclusion_list()
          exclusion_list(append(current, list(list(reason = reason, rows = rows_sel))))

          # Clear selected rows and reason input
          updateTextInput(session, "exclusion_reason", value = "")
          updateReactable("conc_table-table", selected = NA)
        }
      })

      observeEvent(input$remove_exclusion_reason, {
        idx <- as.numeric(input$remove_exclusion_reason)
        current <- exclusion_list()
        if (!is.na(idx) && idx > 0 && idx <= length(current)) {
          exclusion_list(current[-idx])
        }
      })

      output$exclusion_list_ui <- renderUI({
        lst <- exclusion_list()
        if (length(lst) == 0) return(NULL)
        tagList(
          h4("Exclusion reasons:"),
          lapply(seq_along(lst), function(i) {
            div(
              paste0("Rows: ", paste(lst[[i]]$rows, collapse = ", "), " | Reason: ", lst[[i]]$reason),
              actionButton(ns(paste0("remove_exclusion_reason_", i)), "Remove",
                onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("remove_exclusion_reason"), i)
              )
            )
          })
        )
      })

      # Return the exclusion list as a reactive
      return(list(exclusion_list = exclusion_list))
    })
  }