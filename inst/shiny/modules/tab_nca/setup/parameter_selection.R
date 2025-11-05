#' NCA Settings summary module
#'
#' @details
#' Provides a data table with summary of processed NCA data, based on settings provided
#' by the user.
#'
#' @param id Shiny id for the module
#' @param processed_pknca_data PKNCA data that was processed in accordance to setup rules.
#'
#' @returns A reactive data frame with the parameter selections by study type.
parameter_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("The following study types were detected in the data:"),
    reactable_ui(ns("study_types")),

    br(),
    h5("Parameter Selection"),
    br(),
    p("Select the parameters to calculate for each study type.
      Selections can be overridden by uploading a settings file."),
    reactableOutput(ns("parameter_table"))
  )
}

parameter_selection_server <- function(id, processed_pknca_data, parameter_override) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    study_types_df <- reactive({
      req(processed_pknca_data())

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

      conc_group_columns <- group_vars(processed_pknca_data()$conc)
      dose_group_columns <- group_vars(processed_pknca_data()$dose)
      group_columns <- unique(c(conc_group_columns, dose_group_columns))

      groups <- group_columns %>%
        purrr::keep(\(col) {
          !is.null(col) && col != "USUBJID" &&
            length(unique(processed_pknca_data()$conc$data[[col]])) > 1
        })

      study_types_df()  %>%
        #summarise each unique type and group with number of USUBJID
        group_by(!!!syms(groups), type) %>%
        summarise(USUBJID_Count = n_distinct(USUBJID), .groups = "drop")
    })

    DEFAULT_PARAMS <- c(
      "aucinf.obs", "aucinf.obs.dn",
      "auclast", "auclast.dn",
      "cmax", "cmax.dn",
      "clast.obs", "clast.obs.dn",
      "tlast", "tmax",
      "half.life", "cl.obs", "vss.obs", "vz.obs",
      "mrt.last", "mrt.obs",
      "lambda.z",
      "lambda.z.n.points", "r.squared",
      "adj.r.squared", "lambda.z.time.first",
      "aucpext.obs", "aucpext.pred",
      "ae", "fe"
    )

    # ReactiveVal for parameter selection state
    selection_state <- reactiveVal()

    #  observe to update selection_state when study types or overrides change
    parameter_selections <- reactive({
      req(study_types_df())

      params_data <- metadata_nca_parameters %>%
        filter(!TYPE %in% c("PKNCA-not-covered", "IV")) %>%
        select(TYPE, PKNCA, PPTESTCD, PPTEST,
               can_excretion, can_non_excretion, can_single_dose,
               can_multiple_dose, can_extravascular, can_metabolite)

      study_type_names <- unique(study_types_df()$type)

      selection_df <- params_data

      selections_override <- tryCatch({
        parameter_override()
      }, error = function(e) {
        NULL
      })

      # Call the function with the override list
      selection_df <- apply_parameter_selections(
        selection_df = selection_df,
        study_type_names = study_type_names,
        default_params = DEFAULT_PARAMS,
        selections_override = selections_override
      )

      #update the selection df columns
      parameter_selections <- selection_df %>%
        select(
          -any_of(c("can_excretion", "can_non_excretion", "can_single_dose",
                    "can_multiple_dose", "can_extravascular", "can_metabolite"))
        )
      # return the selected parameters
      parameter_selections

    })
    
    # sync the base data (from override) to the live state
    observe({
      selection_state(parameter_selections())
    })

    output$parameter_table <- renderReactable({
      req(parameter_selections())
      req(study_types_df())

      study_type_names <- unique(study_types_df()$type)

      df <- parameter_selections()

      # Define base columns
      col_defs <- list(
        # Freeze parameter info columns to the left
        PKNCA = colDef(show = FALSE),
        PPTESTCD = colDef(sticky = "left", minWidth = 100, name = "Code"),
        PPTEST = colDef(sticky = "left", minWidth = 200, name = "Label")
      )

      # Dynamically create checkbox columns for each study type
      study_type_cols <- purrr::map(study_type_names, ~ {
        col_name <- .x # Capture the study type name
        
        colDef(
          name = col_name,
          align = "center",
          # Custom cell renderer function
          cell = function(value, index) {
            param_name <- df$PKNCA[index]
            
            # 1. Define the Shiny input ID we will send data to
            shiny_input_id <- ns("checkbox_clicked")

            # 2. Create the JavaScript payload
            js_payload <- sprintf(
              "{ param: %s, type: %s, value: this.checked }",
              jsonlite::toJSON(param_name, auto_unbox = TRUE),
              jsonlite::toJSON(col_name, auto_unbox = TRUE)
            )
            
            # 3. Create the onchange JavaScript call
            js_call <- sprintf(
              "Shiny.setInputValue('%s', %s, { priority: 'event' })",
              shiny_input_id,
              js_payload
            )
            
            # 4. Create the raw HTML checkbox tag
            htmltools::tags$input(
              type = "checkbox",
              onchange = js_call,
              # Use `checked` attribute if value is TRUE
              checked = if (isTRUE(value)) NA else NULL
            )
          }
        )
      })

      # Name the list of colDefs so reactable can match them
      names(study_type_cols) <- study_type_names

      # Combine all column definitions
      all_col_defs <- c(col_defs, study_type_cols)

      reactable(
        df,
        groupBy = "TYPE",
        columns = all_col_defs,
        defaultExpanded = TRUE,
        striped = TRUE,
        filterable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        compact = TRUE,
        style = list(fontSize = "0.75em"),
        class = "reactable-table",
        height = "49vh"
      )
    })
    
    # This observer watches for checkbox clicks and updates the state.
    observeEvent(input$checkbox_clicked, {
      
      click_data <- input$checkbox_clicked # Get the payload
      current_selections <- isolate(selection_state())

      if (is.null(current_selections)) {
        return() # State not initialized yet
      }

      # Find the row and column to update
      row_idx <- which(current_selections$PKNCA == click_data$param)
      col_name <- click_data$type
      new_value <- click_data$value
      
      # Check if this is a valid cell
      if (length(row_idx) > 0 && (col_name %in% names(current_selections))) {
        
        old_value <- current_selections[row_idx, col_name]
        
        # Only update if the value has actually changed
        if (!is.null(old_value) && old_value != new_value) {
          
          current_selections[row_idx, col_name] <- new_value
          
          # Set the new state
          selection_state(current_selections)
        }
      }
    })

    # Ensure module is called when hidden (e.g., in a tab)
    outputOptions(output, "parameter_table", suspendWhenHidden = FALSE)

    reactable_server(
      "study_types",
      study_types_summary,
      height = "28vh"
    )

    # Transform the TRUE/FALSE data frame into a named list
    # of parameter vectors
    parameter_lists_by_type <- reactive({
      req(selection_state())
      # Get base data frame
      df <- selection_state()
      study_type_names <- unique(study_types_df()$type)

      # Convert from wide to long, filter for selected (TRUE) rows,
      # and then split the result into a list by study_type.
      df %>%
        tidyr::pivot_longer(
          cols = all_of(study_type_names),
          names_to = "study_type",
          values_to = "selected"
        ) %>%
        filter(selected == TRUE) %>%
        select(study_type, PKNCA) %>%
        split(.$study_type) %>%
        purrr::map(~ .x$PKNCA)
    })

    # On all changes, disable NCA button for given period of time to prevent the user from running
    # the NCA before settings are applied.
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

    # Return list
    list(
      selections = parameter_lists_by_type,
      types_df = study_types_df
    )
  })
}
