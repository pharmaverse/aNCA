#' Manual Slopes Table UI for Slope Selection
#'
#' UI module for displaying and editing the manual slopes table (inclusion/exclusion rules).
#' Provides buttons to add/remove rules and a reactable table for editing.
#'
#' @param id Shiny module id
#' @param help_widget Optional Shiny UI element to display in the header row (e.g., help dropdown)
#' @return Shiny UI element (tagList of fluidRows)
manual_slopes_table_ui <- function(id, help_widget = NULL) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 10,
        div(
          style = "display: flex; gap: 8px; align-items: center;",
          actionButton(ns("add_rule"), "+ Exclusion/Selection", class = "btn-success"),
          actionButton(ns("remove_rule"), "- Remove selected rows", class = "btn-warning")
        )
      ),
      if (!is.null(help_widget)) column(width = 2, help_widget)
    ),
    fluidRow(
      reactableOutput(ns("manual_slopes"))
    )
  )
}

# Delay before re-enabling edit events after a table re-render.
# reactable.extras widgets fire spurious edit events (default dropdown/empty
# text) while they re-initialize; 500ms reliably covers that init window so
# those events are suppressed rather than written back into the data.
EDIT_SUPPRESS_MS <- 500

#' Manual Slopes Table Server for Slope Selection
#'
#' Server module for managing the manual slopes table (inclusion/exclusion rules).
#' Handles adding/removing/editing rules, table's reactivity, and optional override logic.
#'
#' @param id Shiny module id
#' @param pknca_data Reactive providing the current PKNCA data object
#' @param manual_slopes_override Optional reactive providing a table to override manual slopes
#' @return List with:
#'   - manual_slopes: reactiveVal containing the current manual slopes table
#'   - refresh_reactable: reactiveVal for triggering table re-render
manual_slopes_table_server <- function( # nolint: cyclocomp_linter
  id, pknca_data, manual_slopes_override = NULL
) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    # Get group columns for the current PKNCA data (for table structure)
    slopes_pknca_groups <- reactive({
      req(pknca_data())
      pknca_data()$intervals %>%
        select(any_of(c(group_vars(pknca_data()))))
    })

    # manual_slopes: stores the current table of user rules (inclusion/exclusion).
    # Stays NULL until the user adds a rule or a settings override is applied.
    manual_slopes <- reactiveVal(NULL)

    # When pknca_data() changes with a settings override, apply it
    observeEvent(pknca_data(), {
      req(is.null(manual_slopes()), slopes_pknca_groups())

      if (!is.null(manual_slopes_override())) {
        # Integrate slope rules to work with settings upload
        if (!is.data.frame(manual_slopes_override()) ||
              nrow(manual_slopes_override()) == 0) return(NULL)
        log_debug_list("Manual slopes override:", manual_slopes_override())
        # Identify columns to match (all except TYPE, RANGE, REASON)
        match_cols <- setdiff(names(manual_slopes_override()), c("TYPE", "RANGE", "REASON"))

        override_valid <- manual_slopes_override() %>%
          semi_join(pknca_data()$conc$data, by = match_cols) %>%
          nrow() == nrow(manual_slopes_override())

        if (!override_valid) {
          msg <- "Manual slopes not compatible with current data, leaving as default."
          log_warn(msg)
          showNotification(msg, type = "warning", duration = 5)
          return(NULL)
        }
        manual_slopes(manual_slopes_override())
      }
    })

    # create a reactive to update the reactable UI when the table changes
    refresh_reactable <- reactiveVal(0)

    # Flag to suppress edit events during widget (re-)initialization after table re-render.
    # When TRUE, edit observer ignores incoming events to prevent reactable.extras widgets
    # from writing default values (e.g. first dropdown choice, empty text) into the data.
    # Uses a generation counter so rapid successive operations don't prematurely unblock.
    suppress_edit_events <- reactiveVal(0)  # 0 = not suppressing, >0 = suppressing

    # Set suppression on each re-render; clear after delay only if no newer re-render occurred.
    observeEvent(refresh_reactable(), {
      gen <- suppress_edit_events() + 1  # read current, then increment
      suppress_edit_events(gen)
      shinyjs::delay(EDIT_SUPPRESS_MS, {
        # only clear if no newer re-render bumped the generation
        if (identical(suppress_edit_events(), gen)) {
          suppress_edit_events(0)
        }
      })
    }, ignoreInit = TRUE)

    # Add a new row to the table when the user clicks the add button
    observeEvent(input$add_rule, {
      log_trace("{id}: adding manual slopes row")
      first_group <- slopes_pknca_groups()[1, ]
      time_col <- pknca_data()$conc$columns$time
      new_row <- cbind(
        first_group,
        data.frame(
          TYPE = "Exclusion",
          RANGE = paste0(
            inner_join(
              first_group,
              pknca_data()$conc$data,
              by = intersect(
                names(first_group),
                names(pknca_data()$conc$data)
              )
            )[[time_col]][2]
          ),
          REASON = ""
        )
      )

      old_rows <- if (!is.null(manual_slopes())) {
        filter(manual_slopes(), !is.na(TYPE))
      } else {
        new_row[0, ] # empty DF with matching columns
      }
      # Keep only columns present in new_row to prevent rbind column mismatch
      # when old rules (e.g. from plot clicks) have extra columns like ATPTREF (#1302)
      common_cols <- intersect(names(old_rows), names(new_row))
      old_rows <- old_rows[, common_cols, drop = FALSE]
      new_row <- new_row[, common_cols, drop = FALSE]
      updated_data <- as.data.frame(
        rbind(old_rows, new_row),
        stringsAsFactors = FALSE
      )
      manual_slopes(updated_data)
      reset_reactable_memory()
      refresh_reactable(refresh_reactable() + 1)
    })

    # Remove selected rows from the table when the user clicks the remove button
    observeEvent(input$remove_rule, {
      log_trace("{id}: removing manual slopes row")
      req(manual_slopes())
      selected <- getReactableState("manual_slopes", "selected")
      if (is.null(selected) || length(selected) == 0) {
        return()
      }
      edited_slopes <- manual_slopes()[-selected, , drop = FALSE]
      if (nrow(edited_slopes) == 0) edited_slopes <- edited_slopes[0, ]
      manual_slopes(edited_slopes)
      reset_reactable_memory()
      refresh_reactable(refresh_reactable() + 1)
    })

    # Render the manual slopes table (reactable)
    output$manual_slopes <- renderReactable({
      data <- manual_slopes()
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      log_trace("{id}: rendering slope edit data table")
      # Drop stray columns (e.g. ATPTREF from plot clicks) not in canonical column set (#1302)
      canonical_cols <- c(colnames(slopes_pknca_groups()), "TYPE", "RANGE", "REASON")
      data <- data[, intersect(names(data), canonical_cols), drop = FALSE]
      # Define columns: group columns (dynamic), then TYPE/RANGE/REASON (fixed)
      fixed_columns <- list(
        TYPE = colDef(
          cell = dropdown_extra(
            id = ns("edit_TYPE"),
            choices = c("Selection", "Exclusion"),
            class = "dropdown-extra"
          ),
          width = 200
        ),
        RANGE = colDef(
          cell = text_extra(
            id = ns("edit_RANGE")
          )
        ),
        REASON = colDef(
          cell = text_extra(
            id = ns("edit_REASON")
          ),
          width = 400
        )
      )
      dynamic_columns <- lapply(colnames(slopes_pknca_groups()), function(col) {
        colDef(
          cell = dropdown_extra(
            id = ns(paste0("edit_", col)),
            choices = unique(slopes_pknca_groups()[[col]]),
            class = "dropdown-extra"
          ),
          width = 150
        )
      })
      names(dynamic_columns) <- colnames(slopes_pknca_groups())
      all_columns <- c(dynamic_columns, fixed_columns)
      reactable(
        data = data,
        defaultColDef = colDef(align = "center"),
        columns = all_columns,
        selection = "multiple",
        defaultExpanded = TRUE,
        borderless = TRUE,
        theme = reactableTheme(
          rowSelectedStyle = list(
            backgroundColor = "#eee",
            boxShadow = "inset 2px 0 0 0 #ffa62d"
          )
        )
      )
    }) %>%
      shiny::bindEvent(refresh_reactable())

    # Dynamically attach observers for each editable column in the table
    observe({
      req(manual_slopes())
      purrr::walk(colnames(manual_slopes()), function(colname) {
        observeEvent(input[[paste0("edit_", colname)]], {
          req(suppress_edit_events() == 0)
          edit <- input[[paste0("edit_", colname)]]
          edited_slopes <- manual_slopes()
          edited_slopes[edit$row, edit$column] <- edit$value
          manual_slopes(edited_slopes)
        })
      })
    })

    # Output: manual_slopes (reactiveVal) and refresh_reactable (for UI updates)
    list(
      manual_slopes = manual_slopes,
      refresh_reactable = refresh_reactable
    )
  })
}
