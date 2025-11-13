
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

#' NCA parameter selection module
#'
#' @details
#' Provides a data table with summary of processed NCA data, based on settings provided
#' by the user.
#'
#' @param id Shiny id for the module
#' @param processed_pknca_data PKNCA data that was processed in accordance to setup rules.
#' @param parameter_override A 
#'
#' @returns A reactive data frame with the parameter selections by study type.
parameter_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("The following study types were detected in the data:"),
    # This reactable showing the summary stays the same
    reactable_ui(ns("study_types")),
    
    br(),
    h5("Parameter Selection"),
    br(),
    p("Select the parameters to calculate for each study type.
      Selections can be overridden by uploading a settings file."),
    
    # --- This is the new placeholder for the dynamic accordion ---
    uiOutput(ns("dynamic_study_accordion"))
  )
}

parameter_selection_server <- function(id, processed_pknca_data, parameter_override) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- 1. DATA PREP (Mostly from your old code) ---
    
    study_types_df <- reactive({
      req(processed_pknca_data())
      # ... [Your exact logic for detect_study_types] ...
      # (Assuming detect_study_types returns a df with a 'type' column)
      
      # Dummy implementation for placeholder:
      conc_group_columns <- group_vars(processed_pknca_data()$conc)
      dose_group_columns <- group_vars(processed_pknca_data()$dose)
      group_columns <- unique(c(conc_group_columns, dose_group_columns))
      
      groups <- group_columns %>%
        purrr::keep(\(col) {
          !is.null(col) && length(unique(processed_pknca_data()$conc$data[[col]])) > 1
        })
      
      filtered_intervals <- processed_pknca_data()$intervals %>%
        select(all_of(groups))
      
      df <- semi_join(processed_pknca_data()$conc$data, filtered_intervals)
      
      detect_study_types(df,
                         groups,
                         metabfl_column = "METABFL",
                         route_column = processed_pknca_data()$dose$columns$route,
                         volume_column = processed_pknca_data()$conc$columns$volume)
    })
    
    study_types_summary <- reactive({
      req(study_types_df())
      # ... [Your exact logic for study_types_summary] ...
      
      # Dummy implementation for placeholder:
      conc_group_columns <- group_vars(processed_pknca_data()$conc)
      dose_group_columns <- group_vars(processed_pknca_data()$dose)
      group_columns <- unique(c(conc_group_columns, dose_group_columns))
      
      groups <- group_columns %>%
        purrr::keep(\(col) {
          !is.null(col) && col != "USUBJID" &&
            length(unique(processed_pknca_data()$conc$data[[col]])) > 1
        })
      
      study_types_df()  %>%
        group_by(!!!syms(groups), type) %>%
        summarise(USUBJID_Count = n_distinct(USUBJID), .groups = "drop")
    })
    
    DEFAULT_PARAMS <- c(
      "aucinf.obs", "aucinf.obs.dn", "auclast", "auclast.dn",
      "cmax", "cmax.dn", "clast.obs", "clast.obs.dn", "tlast", "tmax",
      "half.life", "cl.obs", "vss.obs", "vz.obs", "mrt.last", "mrt.obs",
      "lambda.z", "lambda.z.n.points", "r.squared", "adj.r.squared",
      "lambda.z.time.first", "aucpext.obs", "aucpext.pred", "ae", "fe"
    )
    
    # --- 2. DEFINE PARAMETER GROUP STRUCTURES ---
    
    # This reactive defines the *structure* of the inner accordion
    ALL_PARAMS_GROUPED <- reactive({
      metadata_nca_parameters %>%
        filter(!TYPE %in% c("PKNCA-not-covered", "IV")) %>%
        select(TYPE, PKNCA, PPTESTCD, PPTEST) %>%
        # Split the data frame into a list of data frames, one per TYPE
        split(.$TYPE)
    })
    
    # This defines the *empty state* for a new study type
    EMPTY_STATE_FOR_ONE_STUDY <- reactive({
      req(ALL_PARAMS_GROUPED())
      # Creates a named list: list("Standard" = NULL, "Urine" = NULL, ...)
      setNames(map(ALL_PARAMS_GROUPED(), ~ NULL), names(ALL_PARAMS_GROUPED()))
    })
    
    
    # --- 3. MASTER STATE MANAGEMENT ---
    
    # This is the "Single Source of Truth" for all selections
    selections_state <- reactiveVal()
    
    # This reactive builds the *base* state from data or overrides
    base_selections <- reactive({
      req(study_types_df(), EMPTY_STATE_FOR_ONE_STUDY())
      
      study_type_names <- unique(study_types_df()$type)
      
      # This is the master list we will build
      master_selection_list <- setNames(
        # Create a list of empty states, one for each study type
        map(study_type_names, ~ EMPTY_STATE_FOR_ONE_STUDY()),
        study_type_names
      )
      
      # Get override file
      selections_override <- tryCatch({
        parameter_override()
      }, error = function(e) { NULL })
      
      # Apply defaults OR overrides
      if (is.null(selections_override)) {
        # --- Apply Defaults ---
        # Loop over each study and apply default params
        for (study in study_type_names) {
          # Find which default params are in which groups
          for (group in names(EMPTY_STATE_FOR_ONE_STUDY())) {
            group_params <- ALL_PARAMS_GROUPED()[[group]]$PKNCA
            defaults_in_this_group <- intersect(DEFAULT_PARAMS, group_params)
            
            if (length(defaults_in_this_group) > 0) {
              master_selection_list[[study]][[group]] <- defaults_in_this_group
            }
          }
        }
      } else {
        # --- Apply Override File ---
        # We assume selections_override is a list:
        # list("Study Type A" = c("p1", "p2"), "Study Type B" = c("p1", "p3"))
        
        # We must re-structure it to fit our new state:
        # list("Study Type A" = list("Standard" = c("p1"), "Urine" = c("p2")))
        
        for (study in names(selections_override)) {
          if (study %in% names(master_selection_list)) {
            
            override_params <- selections_override[[study]]
            
            # Figure out which group each override param belongs to
            for (group in names(EMPTY_STATE_FOR_ONE_STUDY())) {
              group_params <- ALL_PARAMS_GROUPED()[[group]]$PKNCA
              params_in_this_group <- intersect(override_params, group_params)
              
              if (length(params_in_this_group) > 0) {
                master_selection_list[[study]][[group]] <- params_in_this_group
              }
            }
          }
        }
      }
      
      master_selection_list
    })
    
    # This observer syncs the base state to the live state
    observeEvent(base_selections(), {
      selections_state(base_selections())
    })
    
    
    # --- 4. DYNAMIC UI RENDERING ---
    
    # Get a simple reactive list of study type names
    study_types_list <- reactive(unique(study_types_df()$type))
    
    # This renders the main accordion
    output$dynamic_study_accordion <- renderUI({
      req(study_types_list())
      
      all_main_panels <- map(study_types_list(), ~{
        study_name <- .x
        
        # Use a safe, unique ID for the module
        module_id <- str_replace_all(study_name, "[^A-Za-z0-9]", "_")
        
        bslib::accordion_panel(
          title = study_name,
          # Call the sub-module UI
          study_type_param_selector_ui(ns(module_id))
        )
      })
      
      bslib::accordion(
        !!!all_main_panels,
        id = ns("main_study_accordion"),
        multiple = TRUE, # Allow multiple studies open
        open = FALSE     # Start with all studies closed
      )
    })
    
    # --- 5. DYNAMIC MODULE SERVER INSTANTIATION ---
    
    observeEvent(study_types_list(), {
      req(selections_state(), ALL_PARAMS_GROUPED())
      
      current_types <- study_types_list()
      
      # Get the master list of all parameters for the sub-module
      all_params <- ALL_PARAMS_GROUPED()
      
      # Loop and create servers
      map(current_types, ~{
        study_name <- .x
        module_id <- str_replace_all(study_name, "[^A-Za-z0-9]", "_")
        
        # Get the state for *this specific module*
        module_state <- isolate(selections_state())[[study_name]]
        
        # Call the sub-module server
        module_return_grouped <- study_type_param_selector_server(
          module_id,
          all_params_grouped = all_params,
          initial_selections = module_state
        )
        
        # Create an observer to watch the module's return value
        observeEvent(module_return_grouped(), {
          # Get the full master state
          current_main_state <- isolate(selections_state())
          
          # Update just the part for this study
          current_main_state[[study_name]] <- module_return_grouped()
          
          # Save the new master state
          selections_state(current_main_state)
          
        }, ignoreNULL = FALSE, ignoreInit = TRUE) # ignoreInit is important
      })
    })
    
    # --- 6. REACTABLE for Study Types Summary (from your old code) ---
    reactable_server(
      "study_types",
      study_types_summary,
      height = "28vh" # You can adjust this
    )
    
    # --- 7. RETURN VALUE ---
    
    # Transform the *nested* state list into the *flat* list
    # your app expects as a return value.
    parameter_lists_by_type <- reactive({
      req(selections_state())
      
      # `selections_state()` is:
      # list("Study A" = list("Standard" = c("p1"), "Urine" = c("p2")))
      
      # We want:
      # list("Study A" = c("p1", "p2"))
      
      # purrr::map() over the outer list, unlist() the inner list
      map(selections_state(), ~ unlist(.x, use.names = FALSE))
    })
    
    # On all changes, disable NCA button (from your old code)
    observeEvent(parameter_lists_by_type(), {
      runjs(str_glue(
        "buttonTimeout(
          '#nca-run_nca',
          {1000},
          'Applying settings...',
          'Run NCA'
        );"
      ))
    })
    
    # Return list (same as your old module)
    list(
      selections = parameter_lists_by_type,
      types_df = study_types_df
    )
  })
}
