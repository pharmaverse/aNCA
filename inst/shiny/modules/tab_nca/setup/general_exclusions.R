#' General Exclusions Shiny Module
#'
#' UI and server logic for managing and displaying manual and default NCA exclusions.
#' Allows users to select rows from a concentration table, provide exclusion reasons,
#' and manage a list of exclusions with visual feedback and color coding.
#'
#' - Yellow: Default NCA exclusions (from data)
#' - Red: Manual in-app exclusions

# Color constants for exclusion row highlighting
EXCL_COLOR_DEFAULT <- "#FFFF99"  # yellow
EXCL_COLOR_MANUAL  <- "#FFCCCC"  # red

general_exclusions_ui <- function(id) {
  # UI for the General Exclusions module
  ns <- NS(id)
  tagList(
    # Input row for exclusion reason and add button
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
      ),
      # Help button (dropdown)
      dropdown(
        div(
          style = "min-width:340px; max-width:480px;",
          tags$h2("NCA Exclusions Help", style = "font-size:1.2em; margin-bottom:8px;"),
          p("Records excluded here are not used in the NCA PK calculations."),
          tags$ul(
            tags$li(tags$b("Yellow"), ": Default exclusion (NCAwXRS columns mapped)"),
            tags$li(tags$b("Red"), ": In-App exclusion (added via this interface)")
          ),
          p("Select rows and add a reason to exclude. Remove exclusions anytime.")
        ),
        style = "unite",
        right = TRUE,
        icon = icon("question"),
        status = "primary"
      )
    ),
    # Table of current manual exclusions (compact, below input)
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
    # Main concentration data table with row selection and color coding
    reactable_ui(ns("conc_table"))
  )
}

general_exclusions_server <- function(id, processed_pknca_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Store the list of manual exclusions and a counter for unique button IDs
    exclusion_list <- reactiveVal(list())
    xbtn_counter <- reactiveVal(0)

    # Reactive for the concentration data table rendered
    conc_data <- reactive({
      req(processed_pknca_data())
      processed_pknca_data()$conc$data
    })

    # Render the reactable with row coloring for exclusions
    reactable_server(
      "conc_table",
      conc_data,
      selection = "multiple",
      onClick = "select",
      borderless = TRUE,
      defaultPageSize = 25,
      style = list(fontSize = "0.75em"),
      rowStyle = function(x) {
        function(index) {
          excl_indices <- unlist(lapply(exclusion_list(), function(excl) excl$rows))
          if (index %in% excl_indices) {
            return(list(background = EXCL_COLOR_MANUAL))
          }
          row <- x[index, ]
          if (!is.null(row$nca_exclude) && nzchar(row$nca_exclude)) {
            return(list(background = EXCL_COLOR_DEFAULT))
          }
          NULL
        }
      }
    )

    # Add a new manual exclusion when the Add button is pressed
    observeEvent(input$add_exclusion_reason, {
      rows_sel <- getReactableState("conc_table-table", "selected")
      reason <- input$exclusion_reason
      if (length(rows_sel) > 0 && nzchar(reason)) {
        current <- exclusion_list()
        xbtn_id <- paste0("remove_exclusion_reason_", xbtn_counter() + 1)
        xbtn_counter(xbtn_counter() + 1)
        list_new_reason <- list(list(reason = reason, rows = rows_sel, xbtn_id = xbtn_id))
        exclusion_list(append(current, list_new_reason))
        # Clear selected rows and reason input
        updateTextInput(session, "exclusion_reason", value = "")
        updateReactable("conc_table-table", selected = NA)
      }
    })


    # Dynamically observe all remove buttons for exclusion reasons
    observe({
      lst <- exclusion_list()
      lapply(lst, function(item) {
        xbtn_id <- item$xbtn_id
        observeEvent(input[[xbtn_id]], {
          current <- exclusion_list()
          # Remove the item with this xbtn_id
          exclusion_list(Filter(function(x) x$xbtn_id != xbtn_id, current))
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # Render the manual exclusions table (not shown if empty)
    output$exclusion_list_ui <- renderUI({
      lst <- exclusion_list()
      if (length(lst) == 0) return(NULL)
      tags$table(
        style = paste(
          "width:100%",
          "background:#f9f9f9",
          "font-size:0.95em",
          "margin-bottom:12px",
          "border-radius:4px",
          "border-collapse:separate",
          "border-spacing:0",
          sep = "; "
        ),
        tags$thead(
          tags$tr(
            tags$th("Rows", style = "font-weight:600; padding:4px 8px;"),
            tags$th("Reason", style = "font-weight:600; padding:4px 8px;"),
            tags$th("", style = "width:36px;")
          )
        ),
        tags$tbody(
          lapply(lst, function(item) {
            tags$tr(
              tags$td(paste(item$rows, collapse = ", "), style = "padding:4px 8px;"),
              tags$td(item$reason, style = "padding:4px 8px;"),
              tags$td(
                actionButton(
                  ns(item$xbtn_id),
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

    # Prepare exclusion list for return (without xbtn_id, for downstream use)
    exclusion_list_for_return <- reactive({
      lapply(exclusion_list(), function(x) x[setdiff(names(x), "xbtn_id")])
    })

    # Return the exclusion list as a reactive
    list(exclusion_list = exclusion_list_for_return)
  })
}
