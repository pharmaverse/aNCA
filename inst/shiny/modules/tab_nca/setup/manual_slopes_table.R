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
    id, mydata, slopes_pknca_groups
) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Reactive for Slope selector columns
    slope_selector_columns <- reactive({
      req(slopes_pknca_groups())

      c(names(slopes_pknca_groups()), "TYPE", "RANGE", "REASON")
    })

    #' Object for storing exclusion and selection data for lambda slope calculation
    manual_slopes <- reactiveVal({
      data.frame(
        TYPE = character(),
        RANGE = character(),
        REASON = character(),
        stringsAsFactors = FALSE
      )
    })

    observeEvent(mydata(), {
      current_slopes <- manual_slopes()
      
      # Add missing dynamic columns with default values (e.g., NA_character_)
      missing_cols <- setdiff(colnames(slopes_pknca_groups()), colnames(current_slopes))
      for (missing_col in missing_cols) {
        current_slopes[[missing_col]] <- character()
      }

      # Define the desired column order
      ordered_cols <- c(colnames(slopes_pknca_groups()), "TYPE", "RANGE", "REASON")
      current_slopes <- current_slopes[, ordered_cols, drop = FALSE]

      # Update the reactive Val
      manual_slopes(current_slopes)
    })

    #' Adds new row to the selection/exclusion datatable
    observeEvent(input$add_rule, {
      
      log_trace("{id}: adding manual slopes row")
browser()

      # Create the new row with both fixed and dynamic columns
      new_row <- cbind(
        slopes_pknca_groups()[1, ],
        data.frame(
          TYPE = "Selection",
          RANGE = "1:3",
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
browser()
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
      all_columns <- c(
        dynamic_columns,
        list(
          TYPE = fixed_columns$TYPE,
          RANGE = fixed_columns$RANGE,
          REASON = fixed_columns$REASON
        )
      )

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

    #' #' Separate event handling updating displayed reactable upon every change (adding and removing
    #' #' rows, plots selection, edits). This needs to be separate call, since simply re-rendering
    #' #' the table would mean losing focus on text inputs when entering values.
    #' observeEvent(manual_slopes(), {
    #'   req(manual_slopes())
    #' 
    #'   reactable::updateReactable(
    #'     outputId = "manual_slopes",
    #'     data = manual_slopes()
    #'   )
    #'   
    #'   update_plots <- function(pknca_data, pnew_rules)
    #' })

    #' For each of the columns in slope selector data frame, attach an event that will read
    #' edits for that column made in the reactable.
    observe({
      req(slope_selector_columns(), manual_slopes())

      # Dynamically attach observers for each column
      purrr::walk(slope_selector_columns(), \(colname) {
        observeEvent(input[[paste0("edit_", colname)]], {
          browser()
          edit <- input[[paste0("edit_", colname)]]
          edited_slopes <- manual_slopes()
          edited_slopes[edit$row, edit$column] <- edit$value
          manual_slopes(edited_slopes)
        })
      })
    })

    list(
      manual_slopes = manual_slopes,
      refresh_reactable = refresh_reactable
    )
  })
}
