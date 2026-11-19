#' NCA Excretion Analysis Module
#'
#' This module handles logic for excretion analysis in NCA.
#' It allows users to select matrices, map end time columns,
#' adjust for body weight, and select parameters for analysis.
#'
#' @param id A character string used to uniquely identify the module.
#' @param input_pknca_data the input data, which should be a PKNCAdata object
#'
excretion_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Header row with help button
    div(
      style = "display: flex; gap: 0.5em; align-items: center; margin-bottom: 1.2em;",
      tags$h2(
        "Excretion Analysis",
        style = "font-size:1.2em; margin-bottom:0.6em; margin-right:1em;"
      ),
      excretion_help_ui()
    ),
    div(
      class = "excretion-section-container",
      id = ns("excretion_section_container"),
      div(
        class = "excretion-overlay",
        "Excretion analysis is disabled: 'VOLUME' column required."
      ),
      div(style = "position: relative;",  # Wrapping makes overlay work
        card(
          card_body(
            selectInput(ns("matrix_select"), "Select Matrices:", choices = NULL, multiple = TRUE),
            uiOutput(ns("map_end_col_ui_wrapper")),
            checkboxInput(ns("adjust_bw"), "Adjust for Body Weight", value = TRUE),
            checkboxInput(ns("pooled_excreta"), "Pooled excreta", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s']", ns("pooled_excreta")),
              selectizeInput(
                ns("pool_vars"),
                "Pool samples by:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Select one or more variables"
                )
              )
            ),
            uiOutput(ns("param_select_ui_wrapper")),
            checkboxGroupInput(
              ns("interval_types"),
              "Select Interval Types:",
              c("Samples" = "sample", "Profiles" = "profile"),
              selected = c("sample", "profile")
            ),
            actionButton(ns("submit_btn"), "Submit")
          )
        ),
        card(reactable_ui(ns("results_table")), class = "border-0 shadow-none")
      )
    )
  )
}


# Server function
excretion_server <- function(id, input_pknca_data) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Update choices dynamically
    observe({
      req(input_pknca_data())

      conc_data <- input_pknca_data()$conc$data
      volume_missing <- !all(c("VOLUME", "VOLUMEU") %in% names(conc_data))

      shinyjs::toggle(selector = ".excretion-overlay", condition = volume_missing)

      available_cols <- names(conc_data)
      # Check if VOLUME exists before trying to filter
      pcspecs <- {
        if (!volume_missing) {
          conc_data %>%
            filter(!is.na(VOLUME)) %>%
            distinct(PCSPEC) %>%
            pull(PCSPEC)
        } else {
          character(0)
        }
      }

      updateSelectInput(session, "matrix_select", choices = pcspecs,
                        selected = if ("Urine" %in% pcspecs) "Urine" else NULL)

      pool_vars <- .excretion_pooling_variables(
        conc_data,
        input_pknca_data()$dose$data
      )
      selected_pool_vars <- intersect(isolate(input$pool_vars), pool_vars)
      if (length(selected_pool_vars) == 0 && "GROUP" %in% pool_vars) {
        selected_pool_vars <- "GROUP"
      }
      updateSelectizeInput(
        session,
        "pool_vars",
        choices = pool_vars,
        selected = selected_pool_vars,
        server = TRUE
      )

      # Rendering the map end time column selector
      selector_label(input = input,
                     output = output,
                     session = session,
                     choices = available_cols,
                     initial_selection = if ("AEFRLT" %in% available_cols) "AEFRLT" else NULL,
                     selector_ui_wrapper = "map_end_col_ui_wrapper",
                     id = "end_time_col",
                     label = "Map End Time Column:",
                     metadata_type = "variable",
                     pknca_data = NULL,
                     multiple = FALSE)

      urine_params_to_select <- metadata_nca_parameters %>%
        filter(TYPE == "Urine")

      parameters_to_select <- urine_params_to_select$PPTESTCD

      # Rendering the parameters to select selector
      selector_label(input = input,
                     output = output,
                     session = session,
                     choices = parameters_to_select,
                     initial_selection = c("RCAMINT", "FREXINT"),
                     selector_ui_wrapper = "param_select_ui_wrapper",
                     id = "param_select",
                     label = "Select Parameters:",
                     metadata_type = "parameter")
    })

    # Perform calculations
    analysis_result <- reactive({
      req(input_pknca_data())
      data <- input_pknca_data()

      dose_col <- data$dose$columns$dose
      doseu <- data$dose$columns$doseu
      weight_col <- "WTBL"
      weightu <- "WTBLU"

      # Adjust dose by bodyweight if selected
      if (input$adjust_bw) {
        # Check if weight columns exist
        if (!(weight_col %in% names(data$dose$data)) || !(weightu %in% names(data$dose$data))) {
          showNotification(
            glue::glue("Please ensure {weight_col} and {weightu} columns exist in the dose data.
                      No adjustments can be made."), type = "warning"
          )
          return(NULL)
        }

        # mutate dose_col and doseu to be dose * weight
        data$dose$data <- data$dose$data %>%
          mutate(
            # Create units objects per row
            dose_with_unit = pmap(list(.d = !!sym(dose_col), .u = !!sym(doseu)),
                                  ~ set_units(..1, ..2, mode = "standard")),
            weight_with_unit = pmap(list(.w = !!sym(weight_col), .wu = !!sym(weightu)),
                                    ~ set_units(..1, ..2, mode = "standard")),

            # Multiply units objects
            dose_total_unit = map2(dose_with_unit, weight_with_unit, ~ .x * .y),

            # Assign numeric result and unit string to original columns
            !!dose_col := map_dbl(dose_total_unit, drop_units),
            !!doseu := map_chr(dose_total_unit, deparse_unit)
          )

        # Update units
        data$units <- PKNCA_build_units_table(data$conc, data$dose)
      }

      if (isTRUE(input$pooled_excreta)) {
        data$dose$data <- tryCatch(
          .excretion_pool_doses(
            dose_data = data$dose$data,
            dose_col = dose_col,
            dose_group_columns = data$dose$columns$groups,
            pool_vars = input$pool_vars
          ),
          error = function(e) {
            showNotification(conditionMessage(e), type = "warning")
            NULL
          }
        )

        if (is.null(data$dose$data)) {
          return(NULL)
        }

        data$units <- PKNCA_build_units_table(data$conc, data$dose)
      }

      # Update intervals
      # Translate user-selected PPTESTCDs to PKNCA column names
      pknca_params <- translate_terms(input$param_select, "PPTESTCD", "PKNCA")

      # dose profile intervals
      data$intervals <- format_pkncadata_intervals(data$conc,
                                                   data$dose) %>%
        mutate(across(any_of(pknca_params), ~ TRUE, .names = "{.col}"),
               type_interval = "profile") %>%
        filter(PCSPEC %in% input$matrix_select)

      # excretion sample intervals
      # add one row with start AFRLT and end_time_col
      end_time_col <- input$end_time_col

      if (is.null(end_time_col) || end_time_col == "") {
        showNotification("Please select an end time column.", type = "error")
        return(NULL)
      }

      # Obtain all possible pknca parameters
      all_pknca_params <- setdiff(names(PKNCA::get.interval.cols()),
                                  c("start", "end"))

      conc_groups <- unname(unlist(data$conc$columns$groups))

      excretion_intervals <- data$conc$data %>%
        group_by(!!!syms(conc_groups)) %>%
        select(any_of(c(conc_groups, end_time_col, "AFRLT"))) %>%
        mutate(PCSPEC = as.character(PCSPEC)) %>%
        rename(start = AFRLT, end = !!sym(end_time_col)) %>%
        filter(PCSPEC %in% input$matrix_select) %>%
        distinct() %>%
        # Create logical columns with only TRUE for the NCA parameters requested by the user
        mutate(!!!setNames(rep(FALSE, length(all_pknca_params)), all_pknca_params)) %>%
        mutate(across(any_of(pknca_params), ~ TRUE, .names = "{.col}"),
               type_interval = "sample")

      # Combine dose profile intervals and excretion sample intervals
      data$intervals <- bind_rows(data$intervals, excretion_intervals) %>%
        filter(type_interval %in% input$interval_types) %>%
        arrange(PCSPEC, start, end)

      data$units <- data$units  %>%
        mutate(PPSTRESU = ifelse(PPTESTCD == "fe", "%", PPSTRESU),
               conversion_factor = get_conversion_factor(PPORRESU, PPSTRESU))

      data$options$keep_interval_cols <- c("ATPTREF", "type_interval")
      # Run PKNCA analysis
      suppressWarnings(PKNCA::pk.nca(data, verbose = FALSE)) %>%
        # Apply standard CDISC names
        mutate(PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD"))
    }) %>%
      bindEvent(input$submit_btn)

    results_output <- reactive({
      req(analysis_result())
      conc_groups <- unname(unlist(input_pknca_data()$conc$columns$groups))
      #pivot wider
      analysis_result()$result %>%
        mutate(PPSTRESU = ifelse(PPSTRESU %in% c("unitless", "fraction"), "", PPSTRESU)) %>%
        mutate(PPTESTCD = ifelse(PPSTRESU != "",
                                 paste0(PPTESTCD, "[", PPSTRESU, "]"),
                                 PPTESTCD)) %>%
        select(-PPSTRESU, -PPORRES, -PPORRESU, -exclude) %>%
        pivot_wider(names_from = PPTESTCD, values_from = PPSTRES) %>%
        # Add "label" attribute to columns
        add_label_attribute(analysis_result())
    })

    # Render results
    reactable_server(
      "results_table",
      results_output,
      defaultPageSize = 10,
      pageSizeOptions = reactive(c(10, 25, 50, 100, nrow(results_output())))
    )

    # Save the results in the output folder
    observeEvent(results_output(), {
      session$userData$results$additional_analysis$excretion_results <- results_output()
    })
  })
}

.excretion_pooling_variables <- function(conc_data, dose_data) {
  excluded_vars <- c(
    "AFRLT", "ARRLT", "ATPTREF", "AVAL", "AVALU", "CONCDUR", "DOSEA",
    "DOSETRT", "DOSEU", "DOSNOA", "EVID", "NFRLT", "PARAM", "PCSPEC",
    "ROUTE", "RRLTU", "STUDYID", "USUBJID", "VOLUME", "VOLUMEU", "WTBL",
    "WTBLU", "std_route"
  )

  setdiff(intersect(names(conc_data), names(dose_data)), excluded_vars)
}

.excretion_pool_doses <- function(dose_data,
                                  dose_col,
                                  dose_group_columns,
                                  pool_vars) {
  pool_vars <- unique(pool_vars[!is.na(pool_vars) & nzchar(pool_vars)])
  if (length(pool_vars) == 0) {
    stop("Select at least one variable that identifies each pooled sample.")
  }

  missing_vars <- setdiff(pool_vars, names(dose_data))
  if (length(missing_vars) > 0) {
    stop(
      "Pooling variable(s) not found in dose data: ",
      paste(missing_vars, collapse = ", "), "."
    )
  }

  missing_pool_values <- pool_vars[vapply(pool_vars, function(var) {
    any(is.na(dose_data[[var]]) | dose_data[[var]] == "")
  }, logical(1))]
  if (length(missing_pool_values) > 0) {
    stop(
      "Pooling variable(s) contain missing values: ",
      paste(missing_pool_values, collapse = ", "), "."
    )
  }

  dose_groups <- setdiff(unname(unlist(dose_group_columns)), "USUBJID")
  pool_keys <- unique(c(dose_groups, "DOSNOA", pool_vars))
  missing_keys <- setdiff(pool_keys, names(dose_data))
  if (length(missing_keys) > 0) {
    stop(
      "Cannot calculate pooled doses; missing dose grouping variable(s): ",
      paste(missing_keys, collapse = ", "), "."
    )
  }

  dose_values <- .excretion_unit_value(dose_data[[dose_col]])
  invalid_doses <- is.na(dose_values) & !is.na(dose_data[[dose_col]])
  if (any(invalid_doses)) {
    stop("Dose values must be numeric to calculate a pooled dose denominator.")
  }
  dose_data[[dose_col]] <- dose_values

  dose_data %>%
    group_by(!!!syms(pool_keys)) %>%
    mutate(!!dose_col := sum(!!sym(dose_col))) %>%
    ungroup()
}
               
# Excretion analysis help button dropdown
excretion_help_ui <- function() {
  dropdown(
    div(
      class = "anca-help-dropdown",
      style = "min-width:22em; max-width:30em;",
      tags$h2("Excretion Analysis Help"),
      p("Analyze excretion parameters for selected matrices (e.g., URINE) and intervals."),
      tags$ul(
        tags$li(
          tags$b("Select Matrices"), ": Choose specimen types for excretion analysis."
        ),
        tags$li(
          tags$b("Map End Time"), ": Select the column marking the end of sample collection."
        ),
        tags$li(
          tags$b("Adjust for Body Weight"),
          ": Optionally adjust dose by body weight if available."
        ),
        tags$li(
          tags$b("Pooled Excreta"),
          ": Select the variable(s) identifying each pool. The dose denominator is the ",
          "cumulative dose for the animals in that pool."
        ),
        tags$li(
          tags$b("Select Parameters"), ": Pick excretion PK parameters to calculate."
        ),
        tags$li(
          tags$b("Interval Types"), ": Choose to analyze by samples, dose profiles or both."
        ),
        tags$li(
          tags$b("Results Table"), ": View and download calculated excretion results."
        )
      ),
      p(
        "Complete selections and click Submit. Results update below. ",
        "If VOLUME is missing, analysis is disabled."
      )
    ),
    style = "unite",
    icon = icon("question"),
    status = "primary"
  )
}
