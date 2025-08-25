#' NCA Settings summary module
#'
#' @details
#' Provides a data table with summary of processed NCA data, based on settings provided
#' by the user.
#'
#' @param processed_pknca_data PKNCA data that was processed in accordance to setup rules.
#' @param override A reactive expression containing parameter selection overrides from a settings file.
#'
#' @returns A reactive data frame with the parameter selections by study type.
summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactable_ui(ns("study_types")),
    reactableOutput(ns("nca_parameters")),
    #reactable_ui(ns("nca_intervals_summary"))
  )
}

summary_server <- function(id, processed_pknca_data, override, param_trigger) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # summary_data <- reactive({
    #   req(processed_pknca_data())
    # 
    #   conc_group_columns <- group_vars(processed_pknca_data()$conc)
    # 
    #   data <- processed_pknca_data()$intervals %>%
    #     apply_labels(type = "ADPC") %>%
    #     select(where(~!is.logical(.) | any(. == TRUE))) %>%
    #     arrange(!!!syms(c(conc_group_columns, "type_interval", "start", "end")))
    # })

    study_types_df <- reactive({
      req(processed_pknca_data())
      # Get the concentration data and the final, filtered intervals data
      conc_data <- processed_pknca_data()$conc$data
      intervals_data <- processed_pknca_data()$intervals
      
      conc_group_columns <- group_vars(processed_pknca_data()$conc)
      # Filter the main concentration data to include only the groups
      # that are present in the final intervals list.
      filtered_conc_data <- conc_data %>%
        semi_join(intervals_data, by = conc_group_columns)
      
      detect_study_types(filtered_conc_data,
                         route_column = processed_pknca_data()$dose$columns$route,
                         volume_column = processed_pknca_data()$conc$columns$volume)
    })
    
    study_types_summary <- reactive({
      req(study_types_df())
      study_types_df()  %>%
        #summarise each unique Type and group with number of USUBJID
        group_by(STUDYID, DRUG, PCSPEC, type) %>%
        summarise(USUBJID_Count = n_distinct(USUBJID), .groups = "drop")
    })
    
    # #Update params based on settings override
    # observeEvent(override(), {
    #   uploaded_params <- override()
    #   reset_reactable_memory()
    #   # The full parameter dataset used to build the table
    #   params_data <- metadata_nca_parameters %>%
    #     filter(TYPE != "PKNCA-not-covered")
    #   browser()
    #   # Find the row indices corresponding to the uploaded parameter names
    #   new_selected_indices <- which(params_data$PKNCA %in% uploaded_params)
    #   
    #   # Update the reactable with the new selections
    #   updateReactable("nca_parameters", selected = new_selected_indices)
    # })
    
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
      "aucpext.obs", "aucpext.pred"
    )
    
    # ReactiveVal for paramet selection state
    selection_state <- reactiveVal()
    # Render the new, dynamic reactable
    output$nca_parameters <- renderReactable({
      req(study_types_df())

      params_data <- metadata_nca_parameters %>%
        filter(TYPE != "PKNCA-not-covered") %>%
        select(TYPE, PKNCA, PPTESTCD, PPTEST)
      
      study_type_names <- unique(study_types_df()$type)
      
      selection_df <- params_data
      for (st_name in study_type_names) {
        selection_df[[st_name]] <- selection_df$PKNCA %in% DEFAULT_PARAMS
      }
      
      # Dynamically create column definitions for each study type
      study_type_cols <- lapply(
        study_type_names,
        function(st_name) {
          colDef(
            name = st_name,
            cell = checkbox_extra(ns(st_name), class = "table-check"),
            html = TRUE,
            align = "center",
            width = 150
          )
        }
      )
      names(study_type_cols) <- study_type_names
      
      # Set selection state
      selection_state(selection_df)
      
      # Combine with definitions for parameter info columns
      col_defs <- c(
        list(
          PKNCA = colDef(show = FALSE),
          PPTESTCD = colDef(name = "Code", sticky = "left", minWidth = 120),
          PPTEST = colDef(name = "Label", minWidth = 200)
        ),
        study_type_cols
      )
      
      reactable(
        selection_df,
        columns = col_defs,
        groupBy = "TYPE",
        filterable = TRUE,
        compact = TRUE,
        height = "49vh",
        bordered = TRUE,
        resizable = TRUE
      )
    })

    reactable_server(
      "study_types",
      study_types_summary,
      height = "28vh"
    )

    # reactable_server(
    #   "nca_intervals_summary",
    #   summary_data,
    #   height = "98vh"
    # )
    
    observe({
      study_type_names <- unique(study_types_df()$type)
      req(study_type_names)

      lapply(study_type_names, function(st_name) {
        
        observeEvent(input[[st_name]], {
          info <- input[[st_name]] 
          current_state <- selection_state()
          current_state[[st_name]][info$row] <- info$value
          selection_state(current_state)
        })
      })
    })
    
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
    
    # Return list
    list(
      selections = parameter_lists_by_type,
      types_df = study_types_df
    )
  })
}
