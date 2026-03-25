#' Parameter Exclusions Shiny Module
#'
#' UI and server logic for excluding PK parameter rows from TLG summary tables.
#' Users select rows from the NCA results table and mark them for exclusion.
#' Excluded rows are flagged via PPSUM1F = "1" in ADPP.
#'
#' - Yellow: Manually excluded parameter rows

# Color constant for excluded parameter rows
PARAM_EXCL_COLOR <- "#FFF3CD"  # yellow

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
            tags$li(tags$b("Yellow"), ": excluded (PPSUM1F = \"1\" in ADPP)"),
            tags$li(
              "Excluded rows remain in the dataset but are",
              "filtered from summary tables and mean plots."
            )
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
        span("TLG exclusion",
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

# Render the exclusion list as a reactable with remove buttons.
.render_exclusion_table <- function(lst, ns) {
  if (length(lst) == 0) return(NULL)

  df <- data.frame(
    Rows = vapply(lst, function(item) {
      paste(item$rows, collapse = ", ")
    }, character(1)),
    Reason = vapply(lst, "[[", character(1), "reason"),
    xbtn_id = vapply(lst, "[[", character(1), "xbtn_id"),
    stringsAsFactors = FALSE
  )

  reactable::reactable(
    df,
    compact = TRUE,
    bordered = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    defaultPageSize = nrow(df),
    theme = reactable::reactableTheme(
      headerStyle = list(background = "#e9e9e9")
    ),
    columns = list(
      Rows = reactable::colDef(name = "Rows"),
      Reason = reactable::colDef(name = "Reason"),
      xbtn_id = reactable::colDef(
        name = "",
        width = 50,
        sortable = FALSE,
        cell = function(value) {
          as.character(
            actionButton(
              ns(value),
              label = NULL,
              icon = shiny::icon("times"),
              class = "btn btn-link btn-sm",
              style = "padding:2px 6px;"
            )
          )
        },
        html = TRUE
      )
    )
  )
}

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

    # Build a wide-format results table for display.
    # Includes PPSUM1F/PPSUM1R columns and .__is_excluded__ for row styling.
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

      excl_info <- .build_exclusion_reasons(lst)
      n <- nrow(df)
      is_excl <- seq_len(n) %in% excl_info$indices
      reason_vec <- rep(NA_character_, n)
      if (length(excl_info$indices) > 0) {
        reason_vec[excl_info$indices] <- excl_info$reasons
      }

      df$PPSUM1F <- ifelse(is_excl, "1", NA_character_)
      df$PPSUM1R <- reason_vec
      df
    })

    # Render the reactable with row coloring via exclusion_list() closure
    # (same pattern as general_exclusions.R).
    param_table_state <- reactable_server(
      "param_table",
      param_data,
      selection = "multiple",
      onClick = "select",
      borderless = TRUE,
      rowStyle = function(x) {
        function(index) {
          excl_indices <- unlist(lapply(
            exclusion_list(), function(excl) excl$rows
          ))
          if (index %in% excl_indices) {
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
