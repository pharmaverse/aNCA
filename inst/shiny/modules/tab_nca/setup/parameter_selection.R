#' Parameter Selection module
#'
#' This module dynamically generates `accordion_panel`s for each study type
#' detected in the data. It manages the master state of all parameter
#' selections, handles default values, file overrides, and ensures synchronization
#' between the UI and the internal state.
#'
#' @param id A unique namespace ID for the module.
#' @param processed_pknca_data A `reactive` expression returning a
#'   `PKNCAdata` object that has been processed.
#' @param parameter_override A `reactive` expression returning a named list
#'   used to override selections.
#'
#' @returns A `list` containing two reactives:
#'   \item{selections}{A `reactive` list where names are study types and
#'     values are vectors of selected PKNCA parameters, e.g.,
#'     `list("Study Type A" = c("p1", "p2"))`.}
#'   \item{types_df}{A `reactive` data frame containing the study type detection results.}

parameter_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("The following study types were detected in the data:"),
    reactable_ui(ns("study_types")),

    br(),
    p("The following parameters are currently selected:"),
    reactable_ui(ns("selected_parameters_table")),

    br(),
    p("Select the parameters to calculate for each study type.
      Selections can be overridden by uploading a settings file."),

    uiOutput(ns("dynamic_study_accordion"))
  )
}

parameter_selection_server <- function(id, processed_pknca_data, parameter_override) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    initialised_modules <- new.env()

    # Define parameters to be selected by default
    DEFAULT_PARAMS <- c(
      "aucinf.obs", "aucinf.obs.dn",
      "auclast", "auclast.dn",
      "cmax", "cmax.dn",
      "clast.obs", "clast.obs.dn",
      "tlast", "tmax",
      "half.life", "cl.obs", "vss.obs", "vz.obs",
      "mrt.last", "mrt.obs",
      "lambda.z", "lambda.z.n.points",
      "r.squared", "span.ratio",
      "adj.r.squared", "lambda.z.time.first",
      "aucpext.obs", "aucpext.pred",
      "ae", "fe"
    )

    # List of parameter data frames by type
    all_params <- metadata_nca_parameters %>%
      filter(!TYPE %in% c("PKNCA-not-covered", "IV")) %>%
      select(
        TYPE, PKNCA, PPTESTCD, PPTEST,
        can_excretion, can_non_excretion, can_single_dose,
        can_multiple_dose, can_extravascular, can_metabolite
      ) %>%
      mutate(sort_order = row_number())

    # Retrieve study types
    study_types_df <- reactive({
      req(processed_pknca_data())

      conc_group_columns <- group_vars(processed_pknca_data()$conc)
      dose_group_columns <- group_vars(processed_pknca_data()$dose)
      group_columns <- unique(c(conc_group_columns, dose_group_columns))

      groups <- group_columns %>%
        purrr::keep(\(col) {
          !is.null(col) &&
            length(unique(processed_pknca_data()$conc$data[[col]])) > 1
        })

      filtered_intervals <- processed_pknca_data()$intervals %>%
        select(all_of(c(groups, "USUBJID")))

      df <- semi_join(processed_pknca_data()$conc$data, filtered_intervals)

      detect_study_types(
        df,
        groups,
        metabfl_column = "METABFL",
        route_column = processed_pknca_data()$dose$columns$route,
        volume_column = processed_pknca_data()$conc$columns$volume
      )
    })

    # Create summary of study types
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

      study_types_df() %>%
        # summarise each unique type and group with number of USUBJID
        group_by(!!!syms(groups), type) %>%
        summarise(USUBJID_Count = n_distinct(USUBJID), .groups = "drop")
    })

    # ReactiveVal for parameter selection state
    selections_state <- reactiveVal()

    # Build the base state from data or overrides
    base_selections <- reactive({
      req(study_types_df())
      study_type_names <- unique(study_types_df()$type)

      # Get override file
      selections_override <- tryCatch({
        parameter_override()
      }, error = function(e) {
        NULL
      })

      # Update parameter selection using override and considering study types
      apply_parameter_selections(
        selection_df = all_params,
        study_type_names = study_type_names,
        default_params = DEFAULT_PARAMS,
        selections_override = selections_override
      ) %>%
        # Remove unneeded columns
        select(-starts_with("can_"))
    })

    # Sync the base state to the live state
    observeEvent(base_selections(), {
      selections_state(base_selections())
    })

    # Get a simple reactive list of study type names
    study_types_list <- reactive(unique(study_types_df()$type))

    # Render the main accordion
    output$dynamic_study_accordion <- renderUI({
      req(study_types_list())

      all_main_panels <- map(study_types_list(), function(study_type) {
        # Unique ID for each module
        module_id <- str_replace_all(study_type, "[^A-Za-z0-9]", "_")

        accordion_panel(
          title = study_type,
          # Call the sub-module UI
          param_selector_panel_ui(ns(module_id))
        )
      })

      accordion(
        !!!all_main_panels,
        id = ns("main_study_accordion"),
        multiple = TRUE, # Allow multiple studies open
        open = FALSE
      )
    })

    observeEvent(study_types_list(), {
      req(selections_state())

      current_types <- study_types_list()

      # Define grouped structure of parameters for the sub-module
      all_params_grouped <- all_params %>%
        # Split the data frame into a list of data frames, one per TYPE
        split(.$TYPE)

      # Loop and create servers
      map(current_types, function(study_type) {
        # Define module ID
        module_id <- str_replace_all(study_type, "[^A-Za-z0-9]", "_")

        if (exists(module_id, envir = initialised_modules)) {
          # If it exists, exit this iteration
          return()
        }

        # If not, mark it as initialized
        assign(module_id, TRUE, envir = initialised_modules)

        current_type_selections <- reactive({
          state <- selections_state()
          req(state)
          # Return vector of PKNCA codes where this study type is TRUE
          state$PKNCA[state[[study_type]] == TRUE]
        })

        # Call the parameter selector panel server
        selection_single_type_grouped <- param_selector_panel_server(
          module_id,
          all_params_grouped = all_params_grouped,
          current_selection = current_type_selections
        )

        # Watch the module's return value
        observeEvent(selection_single_type_grouped(), {
          # Get the full master state
          selections_df <- selections_state()

          # Get the selected names from the module
          selected_params <- selection_single_type_grouped()

          # If the input is NULL (empty selection), treat it as an empty character vector
          if (is.null(selected_params)) {
            selected_params <- character(0)
          }

          # We compare the current state to the new state to avoid infinite loops
          current_col <- selections_df[[study_type]]
          new_col <- selections_df$PKNCA %in% selected_params

          if (!identical(current_col, new_col)) {
            selections_df[[study_type]] <- new_col
            selections_state(selections_df)
          }
        }, ignoreNULL = FALSE, ignoreInit = TRUE)

        selection_debounce <- debounce(selection_single_type_grouped, 2000)

        observeEvent(selection_debounce(), {
          params <- selection_debounce()
          n_params <- if (is.null(params)) 0 else length(params)

          log_info("Parameter selection for '{study_type}': {n_params} parameters selected.")
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
      })
    })

    # Reactable for summary of study types
    reactable_server(
      "study_types",
      study_types_summary,
      height = "28vh"
    )

    # Transform the TRUE/FALSE data frame into a named list
    # of parameter vectors
    parameter_lists_by_type <- reactive({
      req(selections_state())
      # Get base data frame
      df <- selections_state()
      study_type_names <- unique(study_types_df()$type)

      if (length(study_type_names) == 0) return(list())

      # Convert from wide to long, filter for selected rows,
      # and then split the result into a list by study_type.
      df %>%
        tidyr::pivot_longer(
          cols = any_of(study_type_names),
          names_to = "study_type",
          values_to = "selected"
        ) %>%
        filter(selected == TRUE) %>%
        select(study_type, PKNCA) %>%
        split(.$study_type) %>%
        purrr::map(~ .x$PKNCA)
    })

    # Generate dataset of unique parameters across study types
    selected_parameters_df <- reactive({
      req(parameter_lists_by_type())

      .prepare_selection_table(
        selections_list = parameter_lists_by_type(),
        all_params = all_params,
        study_types_list = study_types_list()
      )
    })

    # Render the reactable
    reactable_server(
      "selected_parameters_table",
      selected_parameters_df,
      height = "35vh",
      columns = function(data) {
        # Handle empty table
        if ("Message" %in% names(data)) {
          return(list(Message = colDef(name = "")))
        }

        # Define Fixed Sticky Columns
        fixed_cols <- list(
          TYPE = colDef(sticky = "left", minWidth = 100),
          PPTESTCD = colDef(sticky = "left", minWidth = 100),
          PPTEST = colDef(sticky = "left", minWidth = 200),
          PKNCA = colDef(show = FALSE)
        )

        # Define Dynamic Columns for Study Types
        study_type_cols <- setdiff(names(data), names(fixed_cols))

        dynamic_cols <- purrr::map(study_type_cols, function(col_name) {
          colDef(
            name = col_name,
            align = "center",
            cell = function(value) if (isTRUE(value)) "✔" else "",
            minWidth = 150
          )
        })

        names(dynamic_cols) <- study_type_cols

        # Combine
        c(fixed_cols, dynamic_cols)
      }
    )

    # On all changes, disable NCA button for given period of time to prevent the
    # user from running the NCA before settings are applied
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


#' Helper: Transforms the selection list into a formatted, sorted, wide dataframe
#' for the Reactable display.
#'
#' @param selections_list List of selected parameters by study type
#' @param all_params Metadata dataframe for parameters
#' @param study_types_list Vector of study type names
#' @return A formatted dataframe or a dataframe with a "Message" column
.prepare_selection_table <- function(selections_list, all_params, study_types_list) {

  # Get all unique parameters selected
  all_params_selected <- unique(unlist(selections_list))

  # Guard clauses for empty states
  if (length(all_params_selected) == 0) {
    return(data.frame(Message = "No parameters are selected."))
  }
  if (length(study_types_list) == 0) {
    return(data.frame(Message = "No study types found."))
  }

  # Create wide data frame (True/False columns)
  wide_df <- map_dfc(study_types_list, ~ {
    setNames(data.frame(all_params_selected %in% selections_list[[.x]]), .x)
  })
  wide_df$PKNCA <- all_params_selected

  if (nrow(wide_df) == 0) {
    return(data.frame(Message = "No parameters are selected."))
  }

  # Join metadata and sort
  metadata_df <- all_params %>%
    select(TYPE, PKNCA, PPTESTCD, PPTEST, sort_order) %>%
    distinct(PKNCA, .keep_all = TRUE)

  wide_df %>%
    left_join(metadata_df, by = "PKNCA") %>%
    arrange(sort_order) %>%
    # Reorder columns
    select(TYPE, PPTESTCD, PPTEST, PKNCA, any_of(study_types_list))

}
