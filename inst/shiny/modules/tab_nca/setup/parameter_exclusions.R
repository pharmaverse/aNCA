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

# Columns used to identify excluded rows across NCA runs.
.get_excl_key_cols <- function(res_nca) {
  group_cols <- unname(unlist(res_nca$data$conc$columns$groups))
  unique(c(group_cols, "PPTESTCD", "PPORRES"))
}

# Extract key columns from result rows at given indices.
.extract_row_keys <- function(result_df, row_indices, key_cols) {
  available <- intersect(key_cols, names(result_df))
  result_df[row_indices, available, drop = FALSE]
}

# Match key rows against a result data frame.
# Returns a list with `matched` (row indices) and `unmatched` (key data frame rows).
.match_keys_to_rows <- function(keys_df, result_df) {
  available <- intersect(names(keys_df), names(result_df))
  if (length(available) == 0) {
    return(list(matched = integer(0), unmatched = keys_df))
  }
  matched <- integer(0)
  unmatched_idx <- integer(0)
  for (i in seq_len(nrow(keys_df))) {
    key_row <- keys_df[i, available, drop = FALSE]
    # Find rows in result_df that match all key columns
    match_mask <- rep(TRUE, nrow(result_df))
    for (col in available) {
      match_mask <- match_mask & (as.character(result_df[[col]]) == as.character(key_row[[col]]))
    }
    hits <- which(match_mask)
    if (length(hits) > 0) {
      matched <- c(matched, hits[1])
    } else {
      unmatched_idx <- c(unmatched_idx, i)
    }
  }
  list(
    matched = matched,
    unmatched = if (length(unmatched_idx) > 0) keys_df[unmatched_idx, , drop = FALSE] else NULL
  )
}

# Restore exclusions from uploaded settings by matching keys against current results.
# Matched entries are added to the exclusion list with resolved row indices.
# Unmatched entries produce a warning message stored in restore_warnings.
.restore_exclusions_from_keys <- function(overrides, res_nca_val, exclusion_list,
                                          xbtn_counter, restore_warnings) {
  result_df <- res_nca_val$result
  key_cols <- .get_excl_key_cols(res_nca_val)
  all_unmatched <- list()
  restored <- list()

  for (entry in overrides) {
    keys_df <- entry$keys
    if (is.null(keys_df) || !is.data.frame(keys_df) || nrow(keys_df) == 0) next

    match_result <- .match_keys_to_rows(keys_df, result_df)

    if (length(match_result$matched) > 0) {
      counter_val <- xbtn_counter() + length(restored) + 1L
      restored <- append(restored, list(list(
        reason = entry$reason,
        rows = match_result$matched,
        keys = .extract_row_keys(result_df, match_result$matched, key_cols),
        xbtn_id = paste0("remove_param_excl_", counter_val)
      )))
    }

    if (!is.null(match_result$unmatched)) {
      all_unmatched <- append(all_unmatched, list(list(
        reason = entry$reason,
        keys = match_result$unmatched
      )))
    }
  }

  if (length(restored) > 0) {
    xbtn_counter(xbtn_counter() + length(restored))
    exclusion_list(restored)
  }

  if (length(all_unmatched) > 0) {
    msgs <- vapply(all_unmatched, function(u) {
      key_desc <- paste(
        apply(u$keys, 1, function(r) paste(names(u$keys), "=", r, collapse = ", ")),
        collapse = "; "
      )
      paste0("\"", u$reason, "\": ", key_desc)
    }, character(1))
    restore_warnings(paste(
      "Some exclusions from settings could not be restored",
      "(values may have changed):\n",
      paste("-", msgs, collapse = "\n")
    ))
  } else {
    restore_warnings(NULL)
  }
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

parameter_exclusions_server <- function(id, res_nca, param_excl_override) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    exclusion_list <- reactiveVal(list())
    xbtn_counter <- reactiveVal(0)
    prev_fingerprint <- reactiveVal(NULL)
    pending_override <- reactiveVal(NULL)
    restore_warnings <- reactiveVal(NULL)

    # Clear exclusions when result structure changes (row count or columns),
    # not on every recomputation (e.g. unit changes that preserve row identity).
    # After clearing, apply any pending override from uploaded settings.
    observeEvent(res_nca(), {
      res <- res_nca()$result
      fp <- paste(nrow(res), paste(names(res), collapse = ","))
      if (!identical(fp, prev_fingerprint())) {
        exclusion_list(list())
        xbtn_counter(0)
        prev_fingerprint(fp)

        overrides <- pending_override()
        if (!is.null(overrides) && length(overrides) > 0) {
          .restore_exclusions_from_keys(
            overrides, res_nca(), exclusion_list, xbtn_counter, restore_warnings
          )
          pending_override(NULL)
        }
      }
    })

    # Queue uploaded exclusions for application after NCA results are ready.
    observeEvent(param_excl_override(), {
      overrides <- param_excl_override()
      if (!is.null(overrides) && length(overrides) > 0) {
        restore_warnings(NULL)
        pending_override(overrides)
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
        result_df <- res_nca()$result
        key_cols <- .get_excl_key_cols(res_nca())
        keys <- .extract_row_keys(result_df, rows_sel, key_cols)

        current <- exclusion_list()
        xbtn_id <- paste0("remove_param_excl_", xbtn_counter() + 1)
        xbtn_counter(xbtn_counter() + 1)
        new_entry <- list(list(
          reason = reason, rows = rows_sel, keys = keys, xbtn_id = xbtn_id
        ))
        exclusion_list(append(current, new_entry))
        updateTextInput(session, "exclusion_reason", value = "")
        updateReactable("param_table-table", selected = NA)
      }
    })

    # Register remove-button observers for exclusion entries
    registered_xbtns <- reactiveVal(character(0))
    observe_remove_buttons(exclusion_list, registered_xbtns, input)

    output$exclusion_list_ui <- renderUI({
      tbl <- .render_exclusion_table(exclusion_list(), ns)
      if (is.null(tbl)) return(NULL)
      tagList(
        tbl,
        tags$script("setTimeout(function(){ Shiny.bindAll(); }, 100);")
      )
    })

    # Return the exclusion list for persistence.
    # Strip xbtn_id (session-only) and rows (ephemeral indices).
    # Only reason + keys are persisted to YAML.
    reactive({
      lapply(exclusion_list(), function(x) {
        x[intersect(names(x), c("reason", "keys"))]
      })
    })
  })
}
