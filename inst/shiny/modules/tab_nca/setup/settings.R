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
          column(4,
            pickerInput(
              ns("select_analyte"),
              "Choose the Analyte:",
              multiple = TRUE,
              choices = NULL,
              options = list(`actions-box` = TRUE)
            )
          ),
          column(4,
            pickerInput(
              ns("select_profile"),
              "Choose the NCA Profile:",
              multiple = TRUE,
              choices = NULL,
              options = list(`actions-box` = TRUE)
            )
          ),
          column(4,
            pickerInput(
              ns("select_pcspec"),
              "Choose the Specimen:",
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
        title = "Partial AUCs",
        reactableOutput(ns("auc_table")),
        actionButton(ns("addRow"), "Add Row")
      ),
      accordion_panel(
        title = "Flag Rule Sets",
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
    data_imputation <- data_imputation_server("data_imputation")

    # File Upload Handling
    observeEvent(c(data(), settings_override()), {
      req(data())
      # Initialize Analyte + Global Validation
      choices <- unique(data()$PARAM) %>% na.omit()
      settings <- settings_override()

      # Defaults
      selected <- choices
      not_compatible <- c()

      # if settings exist, update analyte picker input and check compatibility
      if (!is.null(settings)) {

        if (all(settings$analyte %in% choices)) {
          selected <- settings$analyte
        } else {
          not_compatible <- c(not_compatible, "Analyte")
        }

        # We check raw data here just to see if the settings exist
        if (!all(settings$profile %in% data()$ATPTREF)) {
          not_compatible <- c(not_compatible, "Profile")
        }
        if (!all(settings$pcspec %in% data()$PCSPEC)) {
          not_compatible <- c(not_compatible, "Dose Specimen")
        }

        # Additional settings (PCSPEC and ATPTREF handled later)
        updateSelectInput(session, inputId = "method", selected = settings$method)

        if (!is.null(settings$bioavailability)) {
          updateSelectInput(session, inputId = "bioavailability",
                            selected = settings$bioavailability)
        }

        # Data imputation #
        update_switch("should_impute_c0", value = settings$data_imputation$impute_c0)

        # Partial AUCs #
        if(!is.null(settings$partial_aucs)) {
          auc_data(settings$partial_aucs)
          refresh_reactable(refresh_reactable() + 1)
        }

        # Flags #
        .update_rule_input(
          session,
          "R2ADJ",
          settings$flags$R2ADJ$is.checked,
          settings$flags$R2ADJ$threshold
        )

        .update_rule_input(
          session,
          "R2",
          settings$flags$R2$is.checked,
          settings$flags$R2$threshold
        )

        .update_rule_input(
          session,
          "AUCPEO",
          settings$flags$AUCPEO$is.checked,
          settings$flags$AUCPEO$threshold
        )

        .update_rule_input(
          session,
          "AUCPEP",
          settings$flags$AUCPEP$is.checked,
          settings$flags$AUCPEP$threshold
        )

        .update_rule_input(
          session,
          "LAMZSPN",
          settings$flags$LAMZSPN$is.checked,
          settings$flags$LAMZSPN$threshold
        )
      }

      if (!is.null(settings$analyte) && length(not_compatible) > 0) {
        msg <- paste0(paste0(not_compatible, collapse = ", "),
                      " settings not found in data. Reverting to defaults.")
        log_warn(msg)
        showNotification(msg, type = "warning", duration = 10)
      }

      updatePickerInput(session, "select_analyte", choices = choices, selected = selected)
    })

    # Update Downstream Inputs (Profile & Specimen)
    observeEvent(input$select_analyte, {
      req(data())

      settings <- settings_override()

      # Filter data based on Analyte
      filtered_data <- data() %>%
        filter(PARAM %in% input$select_analyte, !is.na(PCSPEC), !is.na(ATPTREF))

      profile_choices <- sort(unique(filtered_data$ATPTREF))
      pcspec_choices  <- unique(filtered_data$PCSPEC)

      target_profile <- .get_target_selection(
        current_val = isolate(input$select_profile),
        available_choices = profile_choices,
        override_val = settings$profile
      )

      target_pcspec <- .get_target_selection(
        current_val = isolate(input$select_pcspec),
        available_choices = pcspec_choices,
        override_val = settings$pcspec,
        default_logic = function(choices) {
          grep("^plasma$|^serum$", choices, value = TRUE, ignore.case = TRUE)
        }
      )

      updatePickerInput(
        session,
        "select_profile",
        choices = profile_choices,
        selected = target_profile
      )
      updatePickerInput(
        session,
        "select_pcspec",
        choices = pcspec_choices,
        selected = target_pcspec
      )
    })

    # Include keyboard limits for the settings GUI display

    # Keyboard limits for the setting thresholds
    limit_input_value(input, session, "R2ADJ_threshold", max = 1, min = 0, lab = "RSQADJ")
    limit_input_value(input, session, "AUCPEO_threshold", max = 100, min = 0, lab = "AUCPEO")
    limit_input_value(input, session, "AUCPEP_threshold", max = 100, min = 0, lab = "AUCPEP")
    limit_input_value(input, session, "LAMZSPN_threshold", min = 0, lab = "LAMZSPN")

    # Reactive value to store the AUC data table
    auc_data <- reactiveVal(
      tibble(start_auc = rep(NA_real_, 2), end_auc = rep(NA_real_, 2))
    )

    # Render the editable reactable table
    refresh_reactable <- reactiveVal(1)
    output$auc_table <- renderReactable({
      reactable(
        auc_data(),
        columns = list(
          start_auc = colDef(
            name = "Start",  # Display name
            cell = text_extra(id = ns("edit_start_auc")),
            align = "center"
          ),
          end_auc = colDef(
            name = "End",    # Display name
            cell = text_extra(id = ns("edit_end_auc")),
            align = "center"
          )
        )
      )
    }) %>%
      shiny::bindEvent(refresh_reactable())

    # Add a blank row on button click
    observeEvent(input$addRow, {
      df <- auc_data()
      auc_data(bind_rows(df, tibble(start_auc = NA_real_, end_auc = NA_real_)))
      reset_reactable_memory()
      refresh_reactable(refresh_reactable() + 1)
    })

    #' For each of the columns in partial aucs data frame, attach an event that will read
    #' edits for that column made in the reactable.
    observe({
      req(auc_data())
      # Dynamically attach observers for each column
      purrr::walk(c("start_auc", "end_auc"), function(colname) {
        observeEvent(input[[paste0("edit_", colname)]], {
          edit <- input[[paste0("edit_", colname)]]
          partial_aucs <- auc_data()
          partial_aucs[edit$row, edit$column] <- as.numeric(edit$value)
          auc_data(partial_aucs)
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
        bioavailability = input$bioavailability,
        data_imputation = list(
          impute_c0 = data_imputation$should_impute_c0(),
          blq_imputation_rule = data_imputation$blq_imputation_rule()
        ),
        partial_aucs = auc_data(),
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
      runjs(str_glue(
        "buttonTimeout(
          '#nca-run_nca',
          {settings_debounce + 100},
          'Applying settings...',
          'Run NCA'
        );"
      ))
    })

    settings_debounced
  })
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

  updateCheckboxInput(session = session, inputId = rule_id, value = checked)
  if (checked)
    updateNumericInput(session = session, inputId = threshold_id, value = value)
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

  # Maintain Selection
  if (length(intersect(current_val, available_choices)) > 0) {
    return(current_val)
  }

  # Fallback to Default Logic or First Choice
  if (!is.null(default_logic)) {
    fallback <- default_logic(available_choices)
    if (length(fallback) > 0) return(fallback)
  }

  available_choices[1]
}
