study_type_param_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # --- This is the "Selected NCA Parameters" pill display ---
    card(
      full_screen = FALSE,
      style = "margin-top: 1em;",
      card_header("Selected NCA Parameters"),
      card_body(
        div(
          # Custom class for styling the pills, like your old "nca-pill-grid"
          class = "nca-pill-grid",
          uiOutput(ns("selected_pills_ui"))
        )
      )
    ),
    
    br(),
    
    # --- This is the "inner" accordion for parameter groups ---
    h5("Select Parameters by Group"),
    uiOutput(ns("inner_accordion_ui"))
  )
}

study_type_param_selector_server <- function(id,
                                             all_params_grouped,
                                             initial_selections = list()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # A safe ID for each select input (removes spaces)
    safe_group_ids <- str_replace_all(names(all_params_grouped), "[^A-Za-z0-9]", "_")
    
    # Render the inner accordion (Standard, Urine, etc.)
    output$inner_accordion_ui <- renderUI({
      # Create one panel for each parameter group
      all_inner_panels <- pmap(
        list(names(all_params_grouped), all_params_grouped, safe_group_ids),
        function(group_name, params, group_id) {
          
          # Get the choices for this group (just the PKNCA column)
          param_choices <- params$PKNCA
          
          # Name the choices with PPTESTCD for better display
          names(param_choices) <- paste(params$PPTESTCD, "-", params$PPTEST)
          
          # Get the selections that apply to this group
          current_selection <- initial_selections[[group_name]]
          
          bslib::accordion_panel(
            # Use count to show how many params in the group
            title = paste(group_name, "Parameters (", length(param_choices), ")"),
            
            # --- Use pickerInput for better multi-select ---
            shinyWidgets::pickerInput(
              inputId = ns(group_id),
              label = "Select parameters:",
              choices = param_choices,
              selected = current_selection,
              multiple = TRUE,
              options = list(`actions-box` = TRUE, `live-search` = TRUE),
              width = "100%"
            )
          )
        }
      )
      
      # Return the final accordion
      bslib::accordion(
        !!!all_inner_panels,
        id = ns("param_group_accordion"),
        multiple = TRUE, # Allow multiple groups open
        open = FALSE     # Start all closed
      )
    })
    
    # This reactive collects the selections from *all* selectInputs
    all_selections_grouped <- reactive({
      # Read the value from every selectInput
      selections <- map(safe_group_ids, ~ input[[.x]])
      
      # Name the list correctly
      setNames(selections, names(all_params_grouped))
    })
    
    # This renders the blue "pills"
    output$selected_pills_ui <- renderUI({
      # Get the grouped list and flatten it into one vector
      flat_list <- unlist(all_selections_grouped())
      
      if (length(flat_list) == 0) {
        return(p(em("No parameters selected.")))
      }
      
      # Create one pill for each selected item
      map(flat_list, ~ tags$span(class = "nca-pill", .x))
    })
    
    # Return the grouped selections reactive to the main app
    return(all_selections_grouped)
  })
}
