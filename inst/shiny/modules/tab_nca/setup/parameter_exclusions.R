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

# Build a reason-per-index vector from an exclusion list.
# Concatenates with "; " when multiple exclusions cover the same row.
.build_exclusion_reasons <- function(lst) {
  all_indices <- sort(unique(unlist(lapply(lst, function(x) x$rows))))
  reasons <- rep(NA_character_, max(c(all_indices, 0L)))
  for (entry in lst) {
    for (idx in entry$rows) {
      if (idx <= length(reasons)) {
        reasons[idx] <- if (is.na(reasons[idx])) {
          entry$reason
        } else {
          paste(reasons[idx], entry$reason, sep = "; ")
        }
      }
    }
  }
  list(indices = all_indices, reasons = reasons[all_indices])
}

# Render the exclusion list as an HTML table with remove buttons.
.render_exclusion_table <- function(lst, ns) {
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
    # Includes .__is_excluded__ so rowStyle can use the data directly.
    param_data <- reactive({
      req(res_nca())
      lst <- exclusion_list()
      result_df <- res_nca()$result

      display_cols <- c(
        "USUBJID", "PARAM", "PCSPEC", "ATPTREF",
        "PPTESTCD", "PPTEST", "PPORRES", "PPORRESU",
        "PPSTRESN", "PPSTRESU", "exclude"
      )
      available_cols <- intersect(display_cols, names(result_df))
      df <- result_df[, available_cols, drop = FALSE]

      excl_indices <- sort(unique(unlist(lapply(lst, function(e) e$rows))))
      df$.__is_excluded__ <- seq_len(nrow(df)) %in% excl_indices
      df
    })

    # Render the reactable with row coloring driven by data
    param_table_state <- reactable_server(
      "param_table",
      param_data,
      selection = "multiple",
      onClick = "select",
      borderless = TRUE,
      columns = list(.__is_excluded__ = reactable::colDef(show = FALSE)),
      rowStyle = function(x) {
        function(index) {
          if (isTRUE(x[index, ".__is_excluded__"])) {
            return(list(background = PARAM_EXCL_COLOR))
          }
          NULL
        }
      }
    )

    # Add exclusion when button is pressed
    observeEvent(input$add_exclusion, {
      rows_sel <- param_table_state()$selected
      reason <- input$exclusion_reason
      if (length(rows_sel) > 0 && nzchar(reason)) {
        current <- exclusion_list()
        xbtn_id <- paste0("remove_param_excl_", xbtn_counter() + 1)
        xbtn_counter(xbtn_counter() + 1)
        new_entry <- list(list(
          reason = reason, rows = rows_sel, xbtn_id = xbtn_id
        ))
        exclusion_list(append(current, new_entry))
        updateTextInput(session, "exclusion_reason", value = "")
        updateReactable("param_table-table", selected = NA)
      }
    })

    # Track which remove buttons already have observers to avoid duplicates
    registered_xbtns <- reactiveVal(character(0))

    # Register observers only for new remove buttons
    observe({
      lst <- exclusion_list()
      already <- registered_xbtns()
      new_ids <- setdiff(
        vapply(lst, function(x) x$xbtn_id, character(1)),
        already
      )
      for (xbtn_id in new_ids) {
        local({
          local_id <- xbtn_id
          observeEvent(input[[local_id]], {
            current <- exclusion_list()
            exclusion_list(Filter(function(x) x$xbtn_id != local_id, current))
          }, ignoreInit = TRUE, once = TRUE)
        })
      }
      registered_xbtns(union(already, new_ids))
    })

    output$exclusion_list_ui <- renderUI({
      .render_exclusion_table(exclusion_list(), ns)
    })

    reactive(.build_exclusion_reasons(exclusion_list()))
  })
}
