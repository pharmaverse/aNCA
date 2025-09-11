manual_slopes_table_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    # Selection and exclusion controls #
    div(
      class = "plot-widget-group",
      actionButton(ns("add_rule"), "+ Exclusion/Selection", class = "btn-success")
    ),
    div(
      class = "plot-widget-group",
      actionButton(ns("remove_rule"), "- Remove selected rows", class = "btn-warning")
    ),
    # Table with selections and exclusions #
    fluidRow(
      reactableOutput(ns("manual_slopes"))
    )
  )
}


manual_slopes_table_server <- function(
    id, mydata, manual_slopes_override = NULL
) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    slopes_pknca_groups <- reactive({
      req(mydata())
      mydata()$conc$data %>%
        select(any_of(c(group_vars(mydata()), "NCA_PROFILE")))
    })

    #' Object for storing exclusion and selection data for lambda slope calculation
    # TODO (Gerardo): Parameter selection is still affecting mydata() and re-creating the manual slopes!
    manual_slopes <- reactiveVal({NULL})
    observeEvent(mydata(), {
      #browser()
      req(is.null(manual_slopes()))
      req(slopes_pknca_groups())
      ms_colnames <- c(colnames(slopes_pknca_groups()), c("TYPE", "RANGE", "REASON"))
      initial_manual_slopes <- data.frame(
        matrix(character(), ncol = length(ms_colnames), nrow = 0, dimnames = list(character(), ms_colnames))
      )

      # Update the reactive Val
      manual_slopes(initial_manual_slopes)
    })

    #' Adds new row to the selection/exclusion datatable
    observeEvent(input$add_rule, {
      #browser()
      log_trace("{id}: adding manual slopes row")

      # Create the new row with both fixed and dynamic columns
      first_group <- slopes_pknca_groups()[1, ]
      new_row <- cbind(
        first_group,
        data.frame(
          TYPE = "Exclusion",
          RANGE = paste0(
            inner_join(slopes_pknca_groups()[1, ], mydata()$conc$data)[[mydata()$conc$columns$time]][2]
          ),
          REASON = ""
        )
      )

      updated_data <- as.data.frame(rbind(manual_slopes(), new_row), stringsAsFactors = FALSE)
      manual_slopes(updated_data)
      reset_reactable_memory()
      refresh_reactable(refresh_reactable() + 1)
    })

    #' Removes selected row
    observeEvent(input$remove_rule, {
#browser()
      log_trace("{id}: removing manual slopes row")

      selected <- getReactableState("manual_slopes", "selected")
      req(selected)
      edited_slopes <- manual_slopes()[-selected, ]
      manual_slopes(edited_slopes)
      reset_reactable_memory()
      refresh_reactable(refresh_reactable() + 1)
    })

    #' Render manual slopes table
    refresh_reactable <- reactiveVal(1)
    output$manual_slopes <- renderReactable({
      req(manual_slopes())
      log_trace("{id}: rendering slope edit data table")
      # Isolate to prevent unnecessary re-renders on every edit
      isolate({
        data <- manual_slopes()
      })

      # Fixed columns (TYPE, RANGE, REASON)
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

      # Dynamic group column definitions
      dynamic_columns <- lapply(colnames(slopes_pknca_groups()), function(col) {
        colDef(
          cell = dropdown_extra(
            id = ns(paste0("edit_", col)),
            choices = unique(slopes_pknca_groups()[[col]]), # Dynamically set choices
            class = "dropdown-extra"
          ),
          width = 150
        )
      })
      names(dynamic_columns) <- colnames(slopes_pknca_groups())

      # Combine columns in the desired order
      all_columns <- c(dynamic_columns, fixed_columns)

      # Render reactable
      reactable(
        data = data,
        defaultColDef = colDef(
          align = "center"
        ),
        columns = all_columns,
        selection = "multiple",
        defaultExpanded = TRUE,
        borderless = TRUE,
        theme = reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        )
      )
    }) %>%
      shiny::bindEvent(refresh_reactable())

    observe({
      req(manual_slopes())
      # Dynamically attach observers for each column
      purrr::walk(colnames(manual_slopes()), \(colname) {
        observeEvent(input[[paste0("edit_", colname)]], {
          edit <- input[[paste0("edit_", colname)]]
          edited_slopes <- manual_slopes()
          edited_slopes[edit$row, edit$column] <- edit$value
          manual_slopes(edited_slopes)
        })
      })
    })

    # --- Manual slopes override logic (moved from slope_selector_server) ---
    if (!is.null(manual_slopes_override)) {
      observeEvent(manual_slopes_override(), {
        req(manual_slopes_override())

        if (nrow(manual_slopes_override()) == 0) return(NULL)

        log_debug_list("Manual slopes override:", manual_slopes_override())

        # Use mydata() for validation
        override_valid <- apply(manual_slopes_override(), 1, function(r) {
          dplyr::filter(
            mydata()$conc$data,
            PCSPEC == r["PCSPEC"],
            USUBJID == r["USUBJID"],
            PARAM == r["PARAM"],
            NCA_PROFILE == r["NCA_PROFILE"],
            DOSNOA == r["DOSNOA"]
          ) |>
            NROW() != 0
        }) |>
          all()

        if (!override_valid) {
          msg <- "Manual slopes not compatible with current data, leaving as default."
          log_warn(msg)
          showNotification(msg, type = "warning", duration = 5)
          return(NULL)
        }
        manual_slopes(manual_slopes_override())
      })
    }

    list(
      manual_slopes = manual_slopes,
      refresh_reactable = refresh_reactable
    )
  })
}
