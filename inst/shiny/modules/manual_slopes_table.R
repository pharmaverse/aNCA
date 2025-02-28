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
    div(
      class = "plot-widget-group",
      actionButton(ns("save_ruleset"), tags$b("Apply"), class = "btn-primary")
    ),
    # Table with selections and exclusions #
    fluidRow(
      reactableOutput(ns("manual_slopes"))
    )
  )
}


manual_slopes_table_server <- function(
  id, mydata, profiles_per_patient, slopes_groups, pk_nca_trigger
) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
  
    # Reactive for Slope selector columns
    slope_selector_columns <- reactive({
      req(slopes_groups())
      c(slopes_groups(), "TYPE", "RANGE", "REASON")
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
      for (col in slopes_groups()) {
        if (!col %in% colnames(current_slopes)) {
          current_slopes[[col]] <- character()
        }
      }
      # Define the desired column order
      ordered_cols <- c(slopes_groups(), "TYPE", "RANGE", "REASON")
      current_slopes <- current_slopes[, ordered_cols, drop = FALSE]
      
      # Update the reactive Val
      manual_slopes(current_slopes)
    })
    
    row_counter <- reactiveVal(0)
    
    #' Adds new row to the selection/exclusion datatable
    observeEvent(input$add_rule, {
      log_trace("{id}: adding manual slopes row")

      # Create a named list for dynamic columns based on `profiles_per_patient`
      dynamic_values <- lapply(slopes_groups(), function(col) {
        value <- as.character(unique(profiles_per_patient()[[col]]))
        if (length(value) > 0) value[1] else NA_character_  # Handle empty or NULL cases
      })
      
      names(dynamic_values) <- slopes_groups()
      
      # Create the new row with both fixed and dynamic columns
      new_row <- as.data.frame(c(
        dynamic_values,
        TYPE = "Selection",
        RANGE = "1:3",
        REASON = ""
      ), stringsAsFactors = FALSE)
      
      updated_data <- as.data.frame(rbind(manual_slopes(), new_row), stringsAsFactors = FALSE)
      manual_slopes(updated_data)
    })
    
    output$debug_output <- renderPrint({
      manual_slopes()
    })
    
    #' Removes selected row
    observeEvent(input$remove_rule, {
      log_trace("{id}: removing manual slopes row")
      
      selected <- getReactableState("manual_slopes", "selected")
      req(selected)
      edited_slopes <- manual_slopes()[-selected, ]
      manual_slopes(edited_slopes)
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
      dynamic_columns <- lapply(slopes_groups(), function(col) {
        colDef(
          cell = dropdown_extra(
            id = ns(paste0("edit_", col)),
            choices = unique(profiles_per_patient()[[col]]), # Dynamically set choices
            class = "dropdown-extra"
          ),
          width = 150
        )
      })
      names(dynamic_columns) <- slopes_groups()
      
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
    
    #' Separate event handling updating displayed reactable upon every change (adding and removing
    #' rows, plots selection, edits). This needs to be separate call, since simply re-rendering
    #' the table would mean losing focus on text inputs when entering values.
    observeEvent(manual_slopes(), {

      reactable::updateReactable(
        outputId = "manual_slopes",
        data = manual_slopes()
      )
    })
    
    #' For each of the columns in slope selector data frame, attach an event that will read
    #' edits for that column made in the reactable.
    observe({
      req(slope_selector_columns())
      # Dynamically attach observers for each column
      purrr::walk(slope_selector_columns(), \(colname) {
        observeEvent(input[[paste0("edit_", colname)]], {
          edit <- input[[paste0("edit_", colname)]]
          edited_slopes <- manual_slopes()
          edited_slopes[edit$row, edit$column] <- edit$value
          manual_slopes(edited_slopes)
        })
      })
    })
    
    #' saves and implements provided ruleset
    observeEvent(input$save_ruleset, {
      mydata(filter_slopes(mydata(), manual_slopes(), profiles_per_patient(), slopes_groups()))
      pk_nca_trigger(pk_nca_trigger() + 1)
    })

    list(
      manual_slopes = manual_slopes,
      refresh_reactable = refresh_reactable
    )
    
  })
  
}