#' Renders the UI content for a single study type's parameter selection.
#'
study_type_param_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # We render the virtual select from the server to handle 
    # the dynamic choice structure cleanly.
    uiOutput(ns("virtual_select_ui"))
  )
}

#' Server logic for a single study type's parameter selection.
#' 
#' Adapts the flat input from virtualSelectInput back into the grouped
#' list structure expected by the main application logic.
study_type_param_selector_server <- function(id,
                                             all_params_grouped,
                                             initial_selections = list()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Build the Hierarchical Choices List
    # virtualSelectInput expects: list(list(label="Group", options=list(...)), ...)
    formatted_choices <- reactive({
      # Iterate over the groups (Standard, Urine, etc.)
      purrr::imap(all_params_grouped, function(df, group_name) {
        
        # Build the options list for this group
        # label = PPTESTCD (e.g., "CMAX")
        # value = PKNCA (e.g., "cmax")
        # description = PPTEST (e.g., "Maximum observed concentration")
        options_list <- purrr::pmap(list(df$PPTESTCD, df$PKNCA, df$PPTEST), 
                                    function(lab, val, desc) {
                                      list(
                                        label = as.character(lab), 
                                        value = as.character(val), 
                                        description = as.character(desc)
                                      )
                                    })
        
        # Return the group structure
        list(
          label = group_name,
          options = options_list
        )
      }) %>% unname() # Ensure it's a list of groups, not a named list
    })
    
    # 2. Render the Virtual Select Widget
    output$virtual_select_ui <- renderUI({
      req(formatted_choices())
      
      # The widget expects a single flat vector of selected values,
      # but our state is a list of vectors (by group). Flatten it.
      flat_initial_selection <- unlist(initial_selections, use.names = FALSE)
      
      shinyWidgets::virtualSelectInput(
        inputId = ns("nca_parameters_selector"),
        label = "Select Parameters:",
        choices = formatted_choices(),
        selected = flat_initial_selection,
        multiple = TRUE,
        search = TRUE,
        showSelectedOptionsFirst = TRUE,
        hasOptionDescription = TRUE,
        showValueAsTags = TRUE,
        width = "100%"
      )
    })
    
    # 3. Reconstruct Grouped List for Output
    # The main module expects selections to be grouped: 
    # list("Standard" = c(...), "Urine" = c(...))
    # We reconstruct this from the flat widget input.
    all_selections_grouped <- reactive({
      # Get the flat vector of selected values (e.g. c("cmax", "ae"))
      selected_flat <- input$nca_parameters_selector
      
      # Iterate over the original groups to categorize the selections
      map(all_params_grouped, function(df) {
        if (is.null(selected_flat)) return(NULL)
        # Find which of the user's selected params belong to this group
        intersect(selected_flat, df$PKNCA)
      })
    })
    
    # Return the grouped selections reactive to the main app
    return(all_selections_grouped)
  })
}