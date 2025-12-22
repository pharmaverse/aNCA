general_exclusions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("General Exclusions"),
    # Exclusion reason input and add button at the top, inline
    div(
      style = "display: flex; gap: 8px; align-items: center; margin-bottom: 16px;",
      textInput(
        ns("exclusion_reason"),
        label = NULL,
        placeholder = "Enter exclusion reason"
      ),
      actionButton(
        ns("add_exclusion_reason"),
        label = "Add",
        class = "btn btn-primary btn-sm"
      )
    ),
    # Exclusion reasons table (compact, below input)
    uiOutput(ns("exclusion_list_ui")),
    # Color legend for the exclusions table
    div(
      class = "results-legend",
      style = "display:flex; gap:12px; align-items:center; margin:8px 0;",
      div(style = "font-weight:600; font-size:0.95em; margin-right:8px;", "Row Colors:"),
      div(style = "display:flex; align-items:center; gap:6px;",
          div(style = paste0(
            "width:14px; height:14px; background:", EXCL_COLOR_DEFAULT, "; border:1px solid #ddd;"
          )),
          span("Default NCA exclusion", style = "font-size:0.9em;")
      ),
      div(style = "display:flex; align-items:center; gap:6px;",
          div(style = paste0(
            "width:14px; height:14px; background:", EXCL_COLOR_MANUAL, "; border:1px solid #ddd;"
          )),
          span("Manual exclusion", style = "font-size:0.9em;")
      )
    ),
    reactable_ui(ns("conc_table"))
  )
}

general_exclusions_server <- function(id, processed_pknca_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    exclusion_list <- reactiveVal(list())
    exclusion_btn_counter <- reactiveVal(0)
    
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
          excl_indices <- unlist(lapply(exclusion_list(), function(x) x$rows))
          if (index %in% excl_indices) {
            return(list(background = EXCL_COLOR_MANUAL))
          }
          row <- x[index,]
          if (!is.null(row$nca_exclude) && nzchar(row$nca_exclude)) {
            return(list(background = EXCL_COLOR_DEFAULT))
          }
          return(NULL)
        }
      }
    )
    
    observeEvent(input$add_exclusion_reason, {
      rows_sel <- getReactableState("conc_table-table", "selected")
      reason <- input$exclusion_reason
      if (length(rows_sel) > 0 && nzchar(reason)) {
        current <- exclusion_list()
        btn_id <- paste0("remove_exclusion_reason_", exclusion_btn_counter() + 1)
        exclusion_btn_counter(exclusion_btn_counter() + 1)
        exclusion_list(append(current, list(list(reason = reason, rows = rows_sel, btn_id = btn_id))))
        # Clear selected rows and reason input
        updateTextInput(session, "exclusion_reason", value = "")
        updateReactable("conc_table-table", selected = NA)
      }
    })
    

    # Dynamically observe all remove buttons for exclusion reasons
    observe({
      lst <- exclusion_list()
      lapply(lst, function(item) {
        btn_id <- item$btn_id
        observeEvent(input[[btn_id]], {
          current <- exclusion_list()
          # Remove the item with this btn_id
          exclusion_list(Filter(function(x) x$btn_id != btn_id, current))
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
    
    output$exclusion_list_ui <- renderUI({
      lst <- exclusion_list()
      if (length(lst) == 0) return(NULL)
      tags$table(
        style = "width:100%; background:#f9f9f9; font-size:0.95em; margin-bottom:12px; border-radius:4px; border-collapse:separate; border-spacing:0;",
        tags$thead(
          tags$tr(
            tags$th("Rows", style="font-weight:600; padding:4px 8px;"),
            tags$th("Reason", style="font-weight:600; padding:4px 8px;"),
            tags$th("", style="width:36px;")
          )
        ),
        tags$tbody(
          lapply(lst, function(item) {
            tags$tr(
              tags$td(paste(item$rows, collapse = ", "), style="padding:4px 8px;"),
              tags$td(item$reason, style="padding:4px 8px;"),
              tags$td(
                actionButton(
                  ns(item$btn_id),
                  label = NULL,
                  icon = shiny::icon("times"),
                  class = "btn btn-link btn-sm",
                  style = "padding:2px 6px;"
                )
              )
            )
          })
        )
      )
    })
    
    # Prepare exclusion list for return (without btn_id)
    exclusion_list_no_btnid <- reactive({
      lapply(exclusion_list(), function(x) x[setdiff(names(x), "btn_id")])
    })

    # Return the exclusion list as a reactive
    return(list(exclusion_list = exclusion_list_no_btnid))
  })
}

# Color constants for exclusion row highlighting
EXCL_COLOR_DEFAULT <- "#FFFF99"  # yellow
EXCL_COLOR_MANUAL  <- "#FFCCCC"  # red
