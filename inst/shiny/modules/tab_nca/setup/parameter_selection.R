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
    reactableOutput(ns("nca_parameters"))
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
                         drug_column = "DRUG",
                         analyte_column = processed_pknca_data()$conc$columns$groups$group_analyte,
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
    observe({
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
      # Set selection state
      selection_state(parameter_selections)

    })

    # Render the reactable
    output$nca_parameters <- renderReactable({
      req(study_types_df())

      parameter_selections <- isolate(selection_state())
      req(parameter_selections)
      study_type_names <- unique(study_types_df()$type)
      
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
        parameter_selections,
        columns = col_defs,
        groupBy = "TYPE",
        defaultExpanded = TRUE,
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

    # Ensure module is called when hidden (e.g., in a tab)
    outputOptions(output, "nca_parameters", suspendWhenHidden = FALSE)

    reactable_server(
      "study_types",
      study_types_summary,
      height = "28vh"
    )

    # Observe individual checkbox changes and update selection_state accordingly
    observe({
      study_type_names <- unique(study_types_df()$type)
      req(study_type_names)

      lapply(study_type_names, function(st_name) {

        observeEvent(input[[st_name]], {
          info <- input[[st_name]]
          # Isolate state read to prevent feedback loops
          current_state <- isolate(selection_state())
          current_state[[st_name]][info$row] <- info$value
          selection_state(current_state)
          
          }, ignoreNULL = TRUE, ignoreInit = TRUE)
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
