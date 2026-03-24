#' Parameter Exclusions Shiny Module
#'
#' UI and server logic for excluding PK parameter rows from TLG summary tables.
#' Users select rows from the NCA results table and mark them for exclusion.
#' Excluded rows are flagged via PKSUM1F = "1" in ADPP.
#'
#' - Red: Manually excluded parameter rows

# Color constant for excluded parameter rows
PARAM_EXCL_COLOR <- "#FFCCCC"  # red, matching general_exclusions convention

parameter_exclusions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "display: flex; gap: 8px; align-items: center; margin-bottom: 16px;",
      textInput(
        ns("exclusion_reason"),
        label = NULL,
        placeholder = "Enter exclusion reason"
      ),
      actionButton(
        ns("add_exclusion"),
        label = "Exclude Selected",
        class = "btn btn-primary btn-sm"
      ),
      dropdown(
        div(
          style = "min-width:340px; max-width:480px;",
          tags$h2("Parameter Exclusions Help",
                  style = "font-size:1.2em; margin-bottom:8px;"),
          p("Exclude PK parameter results from TLG summary tables and mean plots."),
          tags$ul(
            tags$li("Select rows in the table below and provide a reason."),
            tags$li(tags$b("Red"), ": Excluded from PK summaries (PKSUM1F = \"1\" in ADPP)"),
            tags$li("Excluded rows remain in the dataset but are filtered from summaries.")
          ),
          p("Remove exclusions anytime by clicking the X button.")
        ),
        style = "unite",
        right = TRUE,
        icon = icon("question"),
        status = "primary"
      )
    ),
    uiOutput(ns("exclusion_list_ui")),
    div(
      class = "results-legend",
      style = "display:flex; gap:12px; align-items:center; margin:8px 0;",
      div(style = "font-weight:600; font-size:0.95em; margin-right:8px;",
          "Row Colors:"),
      div(style = "display:flex; align-items:center; gap:6px;",
        div(style = paste0(
          "width:14px; height:14px; background:", PARAM_EXCL_COLOR,
          "; border:1px solid #ddd;"
        )),
        span("Excluded from PK summaries (summary tables and mean plots)",
             style = "font-size:0.9em;")
      )
    ),
    card(reactable_ui(ns("param_table")), class = "border-0 shadow-none")
  )
}

parameter_exclusions_server <- function(id, res_nca) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    exclusion_list <- reactiveVal(list())
    xbtn_counter <- reactiveVal(0)

    # Clear exclusions when NCA results change (stale row indices)
    observeEvent(res_nca(), {
      exclusion_list(list())
      xbtn_counter(0)
    })

    # Build a wide-format results table for display.
    # Depends on exclusion_list() so the table re-renders with updated row colors.
    param_data <- reactive({
      req(res_nca())
      exclusion_list()
      res <- res_nca()
      result_df <- res$result

      # Select key columns for display
      display_cols <- c(
        "USUBJID", "PARAM", "PCSPEC", "ATPTREF",
        "PPTESTCD", "PPTEST", "PPORRES", "PPORRESU",
        "PPSTRESN", "PPSTRESU", "exclude"
      )
      available_cols <- intersect(display_cols, names(result_df))
      result_df[, available_cols, drop = FALSE]
    })

    # Render the reactable with row coloring for exclusions
    reactable_server(
      "param_table",
      param_data,
      selection = "multiple",
      onClick = "select",
      borderless = TRUE,
      rowStyle = function(x) {
        function(index) {
          excl_indices <- unlist(lapply(exclusion_list(), function(excl) excl$rows))
          if (index %in% excl_indices) {
            return(list(background = PARAM_EXCL_COLOR))
          }
          NULL
        }
      }
    )

    # Add exclusion when button is pressed
    observeEvent(input$add_exclusion, {
      rows_sel <- getReactableState("param_table-table", "selected")
      reason <- input$exclusion_reason
      if (length(rows_sel) > 0 && nzchar(reason)) {
        current <- exclusion_list()
        xbtn_id <- paste0("remove_param_excl_", xbtn_counter() + 1)
        xbtn_counter(xbtn_counter() + 1)
        new_entry <- list(list(reason = reason, rows = rows_sel, xbtn_id = xbtn_id))
        exclusion_list(append(current, new_entry))
        updateTextInput(session, "exclusion_reason", value = "")
        updateReactable("param_table-table", selected = NA)
      }
    })

    # Observe remove buttons
    observe({
      lst <- exclusion_list()
      lapply(lst, function(item) {
        xbtn_id <- item$xbtn_id
        observeEvent(input[[xbtn_id]], {
          current <- exclusion_list()
          exclusion_list(Filter(function(x) x$xbtn_id != xbtn_id, current))
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # Render exclusion list table
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
              tags$td(paste(item$rows, collapse = ", "),
                      style = "padding:4px 8px;"),
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

    # Return excluded row indices as a reactive
    reactive({
      sort(unique(unlist(lapply(exclusion_list(), function(x) x$rows))))
    })
  })
}
