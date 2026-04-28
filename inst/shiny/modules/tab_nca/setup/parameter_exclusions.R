#' Parameter Exclusions Shiny Module
#'
#' UI and server logic for excluding PK parameter rows from TLG summary tables.
#' Users select rows from the NCA results table and mark them for exclusion.
#' Excluded rows are flagged via PPSUMFL = "Y" in ADPP.
#'
#' - Yellow: Excluded parameter rows (auto from flag rules + manual)

# Color constant defined in utils-exclusions.R as EXCL_COLOR_PARAM

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
        label = "Add",
        class = "btn btn-primary btn-sm"
      ),
      dropdown(
        div(
          style = "min-width:340px; max-width:480px;",
          tags$h2("TLG Exclusion",
                  style = "font-size:1.2em; margin-bottom:8px;"),
          p("Exclude PK parameter rows from tables, listings, and graphs."),
          tags$ul(
            tags$li("Select rows in the table below and provide a reason."),
            tags$li(tags$b("Yellow"), ": excluded (PPSUMFL = \"Y\" in ADPP)"),
            tags$li(
              "Rows excluded by flag rules are shown with PPSUMFL/PPSUMRSN",
              "but remain in summary tables and plots."
            ),
            tags$li(
              "Manually added exclusions are filtered from",
              "summary tables and mean plots."
            )
          ),
          p("Remove manual exclusions anytime by clicking the X button.")
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
      .legend_swatch(EXCL_COLOR_PARAM, "TLG exclusion")
    ),
    card(reactable_ui(ns("param_table")), class = "border-0 shadow-none")
  )
}

# Build the display data frame for the parameter exclusions table.
# Derives PPSUMFL/PPSUMRSN from the PKNCA exclude column, then layers
# manual exclusions on top.
.build_param_display <- function(result_df, group_cols, manual_exclusions) {
  display_cols <- c(
    group_cols, "ATPTREF",
    "PPTESTCD", "PPTEST", "PPORRES", "PPORRESU",
    "PPSTRESN", "PPSTRESU"
  )
  available_cols <- intersect(display_cols, names(result_df))
  df <- result_df[, available_cols, drop = FALSE]

  exclude_vals <- result_df[["exclude"]]
  if (!is.null(exclude_vals)) {
    exclude_vals[is.na(exclude_vals)] <- ""
  } else {
    exclude_vals <- rep("", nrow(df))
  }

  excl_info <- .build_exclusion_reasons(manual_exclusions, n_rows = nrow(df))
  if (length(excl_info$indices) > 0) {
    for (j in seq_along(excl_info$indices)) {
      idx <- excl_info$indices[j]
      reason <- excl_info$reasons[j]
      existing <- exclude_vals[idx]
      exclude_vals[idx] <- if (existing == "") reason else paste(existing, reason, sep = "; ")
    }
  }

  ppsum <- .derive_ppsum_flags(exclude_vals)
  df$PPSUMFL <- ppsum$PPSUMFL
  df$PPSUMRSN <- ppsum$PPSUMRSN

  apply_labels(df, type = "ADPP")
}

# .build_exclusion_reasons and .render_exclusion_table are defined in
# inst/shiny/functions/utils-exclusions.R and shared with general_exclusions.

parameter_exclusions_server <- function(id, res_nca) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    exclusion_list <- reactiveVal(list())
    xbtn_counter <- reactiveVal(0)
    prev_fingerprint <- reactiveVal(NULL)

    # Clear exclusions only when result structure changes (row count or columns),
    # not on every recomputation (e.g. unit changes that preserve row identity).
    observeEvent(res_nca(), {
      res <- res_nca()$result
      fp <- paste(nrow(res), paste(names(res), collapse = ","))
      if (!identical(fp, prev_fingerprint())) {
        exclusion_list(list())
        xbtn_counter(0)
        prev_fingerprint(fp)
      }
    })

    param_data <- reactive({
      req(res_nca())
      group_cols <- unique(unlist(unname(
        res_nca()$data$conc$columns$groups
      )))
      .build_param_display(res_nca()$result, group_cols, exclusion_list())
    })

    # Render the reactable with row coloring for all rows where PPSUMFL = "Y"
    # (auto-populated from flag rules + manual exclusions).
    param_table_state <- reactable_server(
      "param_table",
      param_data,
      selection = "multiple",
      onClick = "select",
      borderless = TRUE,
      rowStyle = function(x) {
        ppsumfl <- x$PPSUMFL
        function(index) {
          if (ppsumfl[index] == "Y") {
            return(list(background = EXCL_COLOR_PARAM))
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
        updateReactable(ns("param_table-table"), selected = NA)
      }
    })

    # Track which remove buttons already have observers to avoid duplicates
    registered_xbtns <- reactiveVal(character(0))

    # Register observers for new remove buttons (shared helper)
    observe({
      .register_remove_observers(exclusion_list, registered_xbtns, input)
    })

    output$exclusion_list_ui <- renderUI({
      tbl <- .render_exclusion_table(exclusion_list(), ns)
      if (is.null(tbl)) return(NULL)
      tagList(
        tbl,
        tags$script("setTimeout(function(){ Shiny.bindAll(); }, 100);")
      )
    })

    reactive(.build_exclusion_reasons(exclusion_list()))
  })
}
