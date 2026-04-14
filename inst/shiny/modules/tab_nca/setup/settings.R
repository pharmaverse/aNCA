#' NCA Settings Server Module
#'
#' This module handles logic for basic / general settings for the NCA run. Generates appropriate
#' widgets on the page and gathers all user input into single reactive list. The output is
#' debounced by 2500ms in order not to trigger too frequent invalidations upstream.
#'
#' @param id ID of the module.
#' @param data Reactive with data table containing raw data uploaded to the app.
#' @param adnca_data Reactive with `PKNCAdata` object including the `adnca_data`.
#' @param settings_override Reactive expression with compatible list with settings override. When
#'                          changes are detected, current settings will be overwritten with the
#'                          values in this reactive expression.
#'
#' @returns A reactive with a list with all settings.

settings_ui <- function(id) {
  ns <- NS(id)

  tagList(
    accordion(
      accordion_panel(
        title = "General Settings",
        # Selection of analyte, dose number and specimen
        fluidRow(
          column(
            4,
            pickerInput(
              ns("select_analyte"),
              "Choose the Analyte:",
              multiple = TRUE,
              choices = NULL,
              options = list(`actions-box` = TRUE)
            )
          ),
          column(
            4,
            pickerInput(
              ns("select_pcspec"),
              "Choose the Specimen:",
              multiple = TRUE,
              choices = NULL,
              options = list(`actions-box` = TRUE)
            )
          ),
          column(
            4,
            pickerInput(
              ns("select_profile"),
              "Choose the NCA Profile:",
              multiple = TRUE,
              choices = NULL,
              options = list(`actions-box` = TRUE)
            )
          )
        ),
        # Method, NCA parameters, and units table
        fluidRow(
          column(
            4,
            selectInput(
              ns("method"),
              "Extrapolation Method:",
              choices = c("lin-log", "lin up/log down", "linear"),
              selected = "lin up/log down"
            )
          ),
          column(
            4,
            numericInput(
              ns("min_hl_points"),
              "Min. Points for Half-life:",
              value = 3,
              min = 2,
              max = 10,
              step = 1
            )
          ),
          column(
            4, # pickerinput only enabled when IV and EX data present
            shinyjs::hidden(
              div(
                class = "bioavailability-picker",
                pickerInput(
                  ns("bioavailability"),
                  "Calculate Bioavailability:",
                  choices = metadata_nca_parameters %>%
                    filter(startsWith(PPTESTCD, "FABS_") | startsWith(PPTESTCD, "FREL_")) %>%
                    pull(PKNCA, PPTESTCD),
                  multiple = TRUE,
                  selected = NULL
                )
              )
            )
          )
        )
      ),
      accordion_panel(
        title = "Data Imputation",

        # Moved input_switch for should_impute_c0 into the data_imputation module
        data_imputation_ui(ns("data_imputation"))
      ),
      accordion_panel(
        title = "Partial Interval Calculations",
        fluidRow(
          column(
            width = 10,
            actionButton(ns("addRow"), "(+) Add Row", class = "btn-success"),
          ),
          column(
            width = 2,
            dropdown(
              div(
                tags$h2("Partial Interval Calculations Help"),
                p(
                  "Define custom time intervals for calculating partial area",
                  "and related parameters. Add a row for each interval you need."
                ),
                p("For each row, specify:"),
                tags$ul(
                  tags$li(
                    tags$b("Parameter"),
                    ": The interval calculation to perform (e.g., AUCINT, AUCINTA, CAVGINT)."
                  ),
                  tags$li(
                    tags$b("Start"),
                    ": Start time of the interval."
                  ),
                  tags$li(
                    tags$b("End"),
                    ": End time of the interval."
                  )
                ),
                p(
                  tags$b("Note:"),
                  " Rows with missing Start or End values will be ignored."
                ),
                tags$table(
                  class = "imputation-help-table",
                  tags$thead(
                    tags$tr(
                      tags$th("Parameter"),
                      tags$th("Description")
                    )
                  ),
                  tags$tbody(
                    tr("AUCINT", "AUC from T1 to T2 (based on AUClast extrapolation)"),
                    tr("AUCINTD", "AUC from T1 to T2 Normalized by Dose"),
                    tr("AUCINTA", "AUCint (based on AUCall extrapolation)"),
                    tr("AUCINTAD", "AUCint (based on AUCall extrapolation, dose-aware)"),
                    tr("AUCINTIS", "AUCint (based on AUCinf,obs extrapolation)"),
                    tr("AUCINTID", "AUCint (based on AUCinf,obs extrapolation, dose-aware)"),
                    tr("AUCINTIP", "AUCint (based on AUCinf,pred extrapolation)"),
                    tr("AUCINTPD", "AUCint (based on AUCinf,pred extrapolation, dose-aware)"),
                    tr("CAVGINT", "Average Concentration from T1 to T2"),
                    tr("RCAMINT", "Amount Recovered from T1 to T2"),
                    tr("FREXINT", "Fraction Excreted from T1 to T2")
                  )
                )
              ),
              style = "unite",
              right = TRUE,
              icon = icon("question"),
              status = "primary",
              width = "600px"
            )
          )
        ),
        reactableOutput(ns("int_parameters_table"))
      ),
      accordion_panel(
        title = "Flag Rule Sets",
        fluidRow(
          column(
            width = 10
          ),
          column(
            width = 2,
            dropdown(
              div(
                tags$h2("Flag Rule Sets Help"),
                p(
                  "Flag rules define quality thresholds for NCA results.",
                  "Parameters that violate a checked rule are flagged in the",
                  "results table and excluded from descriptive statistics."
                ),
                p(
                  "Each checked rule also generates criterion columns",
                  "(CRITy / CRITyFL) and a summary analysis flag (PPSUMFL)",
                  "in the ADPP dataset."
                )
              ),
              style = "unite",
              right = TRUE,
              icon = icon("question"),
              status = "primary",
              width = "400px"
            )
          )
        ),
        .rule_input(
          ns("R2ADJ"), "R2ADJ >=", 0.7, 0.05, 0, 1,
          tooltip = "Minimum adjusted R-squared threshold for lambda-z related parameters"
        ),
        .rule_input(
          ns("R2"), "R2 >=", 0.7, 0.05, 0, 1,
          tooltip = "Minimum R-squared threshold for lambda-z related parameters",
          checked = FALSE
        ),
        .rule_input(
          ns("AUCPEO"), "AUCPEO (% ext.observed) <=", 20, 1, 0, 100,
          tooltip = "Maximum allowed percent extrapolated (observed) for AUC related parameters"
        ),
        .rule_input(
          ns("AUCPEP"), "AUCPEP (% ext.predicted) <=", 20, 5, 0, 100,
          tooltip = "Maximum allowed percent extrapolated (predicted) for AUC related parameters"
        ),
        .rule_input(
          ns("LAMZSPN"), "LAMZSPN >=", 2, 1, 0,
          tooltip = "Minimum required half-life span ratio for lambda-z related parameters"
        )
      ),
      open = c("General Settings", "Parameter Selection")
    )
  )
}

settings_server <- function(id, data, adnca_data, settings_override) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    conc_data <- reactive(adnca_data()$conc$data)

    # Modules for Data Imputation
    data_imputation <- data_imputation_server("data_imputation", settings_override)

    # File Upload Handling
    observeEvent(c(data(), settings_override()), {
      req(data())
      choices <- unique(data()$PARAM) %>% na.omit()
      settings <- settings_override()

      restore <- .restore_settings(
        settings, choices, data(),
        adnca_data, session, int_parameters,
        refresh_reactable
      )

      updatePickerInput(
        session, "select_analyte",
        choices = choices, selected = restore$selected
      )
    })

    # Cascading filter logic for analyte, pcspec, and profile (atptref).
    # Analyte and pcspec are bidirectional primary filters; profile is
    # a dependent filter that never triggers updates on the others.
    # A guard flag prevents infinite observer loops.
    updating_filters <- reactiveVal(FALSE)

    # settings_override is consumed once during the first cascade after
    # settings upload. After that, pending_settings is set to NULL so
    # subsequent user-driven changes are not overridden by stale values.
    pending_settings <- reactiveVal(NULL)

    observeEvent(settings_override(), {
      pending_settings(settings_override())
    })

    # Helper: consume pending settings (returns them once, then clears)
    .consume_settings <- function() {
      s <- pending_settings()
      pending_settings(NULL)
      s
    }

    # Helper: update profile choices based on current analyte + pcspec
    .update_profile <- function(settings = NULL) {
      filtered <- data() %>%
        filter(
          PARAM %in% input$select_analyte,
          PCSPEC %in% input$select_pcspec,
          !is.na(ATPTREF)
        )
      profile_choices <- sort(unique(filtered$ATPTREF))
      target_profile <- .get_target_selection(
        current_val = isolate(input$select_profile),
        available_choices = profile_choices,
        override_val = settings$profile
      )
      log_info("NCA profile selection changed: ", paste(target_profile, collapse = ", "))
      updatePickerInput(
        session, "select_profile",
        choices = profile_choices, selected = target_profile
      )
    }

    # When analyte changes: unselect unavailable pcspec items, then update profile
    observeEvent(c(input$select_analyte, data()), {
      req(data(), input$select_analyte)
      if (updating_filters()) return()
      updating_filters(TRUE)
      on.exit(updating_filters(FALSE))

      log_info("Analyte selection changed: ", paste(input$select_analyte, collapse = ", "))

      settings <- .consume_settings()

      all_pcspec <- unique(data()$PCSPEC) %>% na.omit()
      available_pcspec <- data() %>%
        filter(PARAM %in% input$select_analyte, !is.na(PCSPEC)) %>%
        pull(PCSPEC) %>%
        unique()

      target_pcspec <- .get_target_selection(
        current_val = isolate(input$select_pcspec),
        available_choices = available_pcspec,
        override_val = settings$pcspec,
        default_logic = function(choices) {
          grep(
            "^plasma$|^serum$", choices,
            value = TRUE, ignore.case = TRUE
          )
        }
      )
      updatePickerInput(
        session, "select_pcspec",
        choices = all_pcspec, selected = target_pcspec
      )

      .update_profile(settings)
    })

    # When pcspec changes: unselect unavailable analyte items, then update profile
    observeEvent(input$select_pcspec, {
      req(data(), input$select_pcspec)
      if (updating_filters()) return()
      updating_filters(TRUE)
      on.exit(updating_filters(FALSE))

      log_info("Specimen selection changed: ", paste(input$select_pcspec, collapse = ", "))

      settings <- .consume_settings()

      all_analyte <- unique(data()$PARAM) %>% na.omit()
      available_analyte <- data() %>%
        filter(PCSPEC %in% input$select_pcspec, !is.na(PARAM)) %>%
        pull(PARAM) %>%
        unique()

      target_analyte <- .get_target_selection(
        current_val = isolate(input$select_analyte),
        available_choices = available_analyte,
        override_val = settings$analyte
      )
      updatePickerInput(
        session, "select_analyte",
        choices = all_analyte, selected = target_analyte
      )

      .update_profile(settings)
    })

    # Log method and min half-life points changes
    observeEvent(input$method, {
      log_info("Extrapolation method changed: {input$method}")
    }, ignoreInit = TRUE)

    observeEvent(input$min_hl_points, {
      log_info("Min. half-life points changed: {input$min_hl_points}")
    }, ignoreInit = TRUE)

    # Include keyboard limits for the settings GUI display

    # Keyboard limits for the setting thresholds
    limit_input_value(input, session, "R2ADJ_threshold", max = 1, min = 0, lab = "RSQADJ")
    limit_input_value(input, session, "AUCPEO_threshold", max = 100, min = 0, lab = "AUCPEO")
    limit_input_value(input, session, "AUCPEP_threshold", max = 100, min = 0, lab = "AUCPEP")
    limit_input_value(input, session, "LAMZSPN_threshold", min = 0, lab = "LAMZSPN")
    limit_input_value(input, session, "min_hl_points", max = 10, min = 2, lab = "Min. HL Points")

    # Log flag rule changes
    .flag_names <- c("R2ADJ", "R2", "AUCPEO", "AUCPEP", "LAMZSPN")
    lapply(.flag_names, function(flag) {
      observeEvent(input[[paste0(flag, "_rule")]], {
        state <- if (input[[paste0(flag, "_rule")]]) "enabled" else "disabled"
        log_info("Flag rule {flag} {state}")
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0(flag, "_threshold")]], {
        log_info("Flag rule {flag} threshold changed: {input[[paste0(flag, '_threshold')]]}")
      }, ignoreInit = TRUE)
    })

    # Reactive value to store the partial intervals data table
    # Define the parameters that can be used for partial area calculations
    PARTIAL_INT_PARAMS <- metadata_nca_parameters %>%
      filter(
        grepl("INT", PPTESTCD),
        TYPE != "PKNCA-not-covered"
      ) %>%
      arrange(PPTESTCD)

    int_parameters <- reactiveVal(
      tibble(
        parameter = PARTIAL_INT_PARAMS$PPTESTCD[1],
        start_auc = rep(NA_real_, 2),
        end_auc = rep(NA_real_, 2)
      )
    )

    # Render the editable reactable table
    refresh_reactable <- reactiveVal(1)
    output$int_parameters_table <- renderReactable({
      reactable(
        int_parameters(),
        columns = list(
          parameter = colDef(
            name = "Parameter",
            cell = dropdown_extra(
              id = ns("edit_parameter"),
              choices = PARTIAL_INT_PARAMS$PPTESTCD,
              class = "table-dropdown"
            ),
            align = "center"
          ),
          start_auc = colDef(
            name = "Start", # Display name
            cell = text_extra(id = ns("edit_start_auc")),
            align = "center"
          ),
          end_auc = colDef(
            name = "End", # Display name
            cell = text_extra(id = ns("edit_end_auc")),
            align = "center"
          )
        )
      )
    }) %>%
      shiny::bindEvent(refresh_reactable())

    # Add a blank row on button click
    observeEvent(input$addRow, {
      int_parameters() %>%
        bind_rows(
          tibble(
            parameter = PARTIAL_INT_PARAMS$PPTESTCD[2],
            start_auc = NA_real_,
            end_auc = NA_real_
          )
        ) %>%
        int_parameters()
      reset_reactable_memory()
      refresh_reactable(refresh_reactable() + 1)
    })

    #' For each of the columns in partial aucs data frame, attach an event that will read
    #' edits for that column made in the reactable.
    observe({
      req(int_parameters())
      # Dynamically attach observers for each column
      edit_inputs <- intersect(names(input), paste0("edit_", names(int_parameters())))
      purrr::walk(edit_inputs, function(edit_input) {
        observeEvent(input[[edit_input]], {
          edit <- input[[edit_input]]
          partial_aucs <- int_parameters()
          val <- if (edit$column != "parameter") as.numeric(edit$value) else edit$value
          partial_aucs[edit$row, edit$column] <- val
          int_parameters(partial_aucs)
        })
      })
    })

    settings <- reactive({
      req(input$select_analyte) # Wait on general settings UI to be loaded

      list(
        analyte = input$select_analyte,
        profile = input$select_profile,
        pcspec = input$select_pcspec,
        method = input$method,
        min_hl_points = input$min_hl_points,
        bioavailability = input$bioavailability,
        data_imputation = list(
          impute_c0 = data_imputation$should_impute_c0(),
          blq_strategy = data_imputation$blq_strategy(),
          blq_imputation_rule = data_imputation$blq_imputation_rule()
        ),
        int_parameters = int_parameters(),
        flags = list(
          R2ADJ = list(
            is.checked = input$R2ADJ_rule,
            threshold = input$R2ADJ_threshold
          ),
          R2 = list(
            is.checked = input$R2_rule,
            threshold = input$R2_threshold
          ),
          AUCPEO = list(
            is.checked = input$AUCPEO_rule,
            threshold = input$AUCPEO_threshold
          ),
          AUCPEP = list(
            is.checked = input$AUCPEP_rule,
            threshold = input$AUCPEP_threshold
          ),
          LAMZSPN = list(
            is.checked = input$LAMZSPN_rule,
            threshold = input$LAMZSPN_threshold
          )
        ),
        units = session$userData$units_table()
      )
    })

    # Debounce the trigger, so the data is not updated too often.
    settings_debounce <- 2500
    settings_debounced <- debounce(settings, settings_debounce)

    # On all changes, disable NCA button for given period of time to prevent the user from running
    # the NCA before settings are applied.
    observeEvent(settings(), {
      runjs(glue::glue(
        "buttonTimeout(
          '#nca-run_nca',
          {settings_debounce + 100},
          'Applying settings...',
          'Run NCA'
        );"
      ))
    })

    list(
      all = settings_debounced,
      analyte = reactive(input$select_analyte),
      pcspec = reactive(input$select_pcspec),
      profile = reactive(input$select_profile)
    )
  })
}

#' Apply uploaded settings to the NCA setup inputs.
#'
#' Validates compatibility, updates non-filter inputs (method, flags, etc.),
#' and returns the selected analyte values. Filter cascade (pcspec, profile)
#' is handled by the analyte observer.
#'
#' @param settings Settings list from upload or NULL.
#' @param choices Available analyte choices from data.
#' @param raw_data The raw data frame.
#' @param adnca_data Reactive returning PKNCAdata object.
#' @param session Shiny session.
#' @param int_parameters ReactiveVal for partial AUC parameters.
#' @param refresh_reactable ReactiveVal counter for table refresh.
#' @returns List with `selected` (analyte selection to apply).
.restore_settings <- function(settings, choices, raw_data,
                              adnca_data, session, int_parameters,
                              refresh_reactable) {
  selected <- choices
  not_compatible <- c()

  if (!is.null(settings)) {
    if (!is.null(settings$analyte) && all(settings$analyte %in% choices)) {
      selected <- settings$analyte
    } else {
      not_compatible <- c(not_compatible, "Analyte")
    }

    if (!all(settings$profile %in% raw_data$ATPTREF)) {
      not_compatible <- c(not_compatible, "Profile")
    }
    if (!all(settings$pcspec %in% raw_data$PCSPEC)) {
      not_compatible <- c(not_compatible, "Dose Specimen")
    }

    .restore_non_filter_settings(
      settings, adnca_data, session, int_parameters, refresh_reactable
    )

    if (!is.null(settings$analyte) && length(not_compatible) > 0) {
      msg <- paste0(
        paste0(not_compatible, collapse = ", "),
        " settings not found in data. Reverting to defaults."
      )
      log_warn(msg)
      showNotification(msg, type = "warning", duration = 10)
    }
  }

  list(selected = selected)
}

#' Restore non-filter settings (method, bioavailability, flags, etc.)
#' @param settings Settings list.
#' @param adnca_data Reactive returning PKNCAdata object.
#' @param session Shiny session.
#' @param int_parameters ReactiveVal for partial AUC parameters.
#' @param refresh_reactable ReactiveVal counter for table refresh.
.restore_non_filter_settings <- function(settings, adnca_data, session,
                                         int_parameters, refresh_reactable) {
  updateSelectInput(session, inputId = "method", selected = settings$method)

  if (!is.null(settings$min_hl_points)) {
    updateNumericInput(
      session, inputId = "min_hl_points",
      value = settings$min_hl_points
    )
  }

  dose_routes <- unique(adnca_data()$dose$data$std_route)
  if (!is.null(settings$bioavailability) && length(dose_routes) > 1) {
    updateSelectInput(
      session, inputId = "bioavailability",
      selected = settings$bioavailability
    )
  }

  # Data imputation is restored by data_imputation_server via settings_override

  if (!is.null(settings$int_parameters)) {
    int_parameters(settings$int_parameters)
    refresh_reactable(refresh_reactable() + 1)
  }

  for (flag in c("R2ADJ", "R2", "AUCPEO", "AUCPEP", "LAMZSPN")) {
    .update_rule_input(
      session, flag,
      settings$flags[[flag]]$is.checked,
      settings$flags[[flag]]$threshold
    )
  }
}

#' Generates rule input widget with a checkbox and conditional numeric input for the value.
#'
#' @param id ID for the widget as a whole.
#' @param label Character string with the label for the widget.
#' @param default Numeric with default value for the `shiny::numericInput` widget.
#' @param step    Step for the `shiny::numericInput` widget.
#' @param min     Min value for the `shiny::numericInput` widget.
#' @param max     Max value for the `shiny::numericInput` widget.
#' @param tooltip Optional tooltip function to add a tooltip to the label.
#' @param checked Logical, default state of the checkbox (default TRUE).
#' @returns `shiny::fluidRow` containing html elements of the widget.
.rule_input <- function(id, label, tooltip = NULL, default, step, min, max = NULL, checked = TRUE) {
  threshold_id <- paste0(id, "_threshold")
  rule_id <- paste0(id, "_rule")
  numeric_args <- list(
    inputId = threshold_id,
    label = "",
    value = default,
    step = step,
    min = min
  )

  # Only include `max` if not NULL
  if (!is.null(max)) {
    numeric_args$max <- max
  }

  label_tag <- if (!is.null(tooltip)) {
    tooltip(tags$span(label), tooltip)
  } else {
    id
  }

  fluidRow(
    column(
      width = 6,
      checkboxInput(rule_id, label_tag, value = checked)
    ),
    column(
      width = 6,
      conditionalPanel(
        condition = paste0("input['", rule_id, "'] == true"),
        div(
          class = "nca-numeric-container",
          do.call(numericInput, numeric_args)
        )
      )
    )
  )
}

#' Updates rule input.
#'
#' @param session Shiny session object.
#' @param id      ID of the widget as a whole.
#' @param checked Boolean, whether the checkbox should be selected or not.
#' @param value   Numeric value for the related `shiny::numericInput` widget.
.update_rule_input <- function(session, id, checked, value) {
  threshold_id <- paste0(id, "_threshold")
  rule_id <- paste0(id, "_rule")

  # If checked is NULL do nothing
  if (is.null(checked)) return()

  updateCheckboxInput(session = session, inputId = rule_id, value = checked)
  if (checked) {
    updateNumericInput(session = session, inputId = threshold_id, value = value)
  }
}

#'Helper to get target selection
#' @param current_val Current selected value(s).
#' @param available_choices Available choices to select from.
#' @param override_val Override value(s) to use if valid.
#' @param default_logic Optional function to determine default selection from available choices.
.get_target_selection <- function(
  current_val,
  available_choices,
  override_val,
  default_logic = NULL
) {
  # Check Settings Override
  if (!is.null(override_val) && all(override_val %in% available_choices)) {
    return(override_val)
  }

  # Maintain Selection — keep only items that are still available
  kept <- intersect(current_val, available_choices)
  if (length(kept) > 0) {
    return(kept)
  }

  # Fallback to Default Logic or First Choice
  if (!is.null(default_logic)) {
    fallback <- default_logic(available_choices)
    if (length(fallback) > 0) return(fallback)
  }
  available_choices[1]
}
