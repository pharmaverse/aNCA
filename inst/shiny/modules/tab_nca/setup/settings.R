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
          column(4,
            selectInput(
              ns("method"),
              "Extrapolation Method:",
              choices = c("lin-log", "lin up/log down", "linear"),
              selected = "lin up/log down"
            )
          ),
          column(4, # pickerinput only enabled when IV and EX data present
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
        title = "Parameter Selection",
        reactableOutput(ns("nca_parameters")),
        card(
          full_screen = FALSE,
          style = "margin-top: 2em;",
          card_header("Selected NCA Parameters"),
          card_body(uiOutput(ns("nca_param_display"))
          )
        )
      ),
      accordion_panel(
        title = "Data Imputation",
        input_switch(
          id = ns("should_impute_c0"),
          label = "Impute Concentration",
          value = TRUE
        ),
        br(),
        helpText(HTML(paste(
          "Imputes a start-of-interval concentration to calculate non-observational parameters:",
          "- If DOSNO = 1 & IV infusion: C0 = 0",
          "- If DOSNO > 1 & not IV bolus: C0 = predose",
          "- If IV bolus & monoexponential data: logslope",
          "- If IV bolus & not monoexponential data: C0 = C1",
          sep = "<br>"
        )))
      ),
      accordion_panel(
        title = "Partial AUCs",
        reactableOutput(ns("auc_table")),
        actionButton(ns("addRow"), "Add Row")
      ),
      accordion_panel(
        title = "Flag Rule Sets",
        .rule_input(ns("adj.r.squared"), "RSQADJ:", 0.7, 0.05, 0, 1),
        .rule_input(ns("aucpext.obs"), "AUCPEO (% ext.observed):", 20, 1, 0, 100),
        .rule_input(ns("aucpext.pred"), "AUCPEP (% ext.predicted):", 20, 5, 0, 100),
        .rule_input(ns("span.ratio"), "SPAN:", 2, 1, 0)
      ),
      id = "acc",
      open = c("General Settings", "Parameter Selection")
    )
  )
}

settings_server <- function(id, data, adnca_data, settings_override) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    conc_data <- reactive(adnca_data()$conc$data)

    # File Upload Handling
    observeEvent(settings_override(), {
      settings <- settings_override()

      log_debug_list("User settings override:", settings)

      not_compatible <- c()

      # General #
      if (all(settings$analyte %in% unique(data()$PARAM))) {
        updatePickerInput(inputId = "select_analyte", selected = settings$analyte)
      } else {
        not_compatible <- append(not_compatible, "Analyte")
      }

      if (all(settings$profile %in% unique(data()$NCA_PROFILE))) {
        updatePickerInput(inputId = "select_profile", selected = settings$profile)
      } else {
        not_compatible <- append(not_compatible, "NCA Profile")
      }

      if (all(settings$pcspec %in% unique(data()$PCSPEC))) {
        updatePickerInput(inputId = "select_pcspec", selected = settings$pcspec)
      } else {
        not_compatible <- append(not_compatible, "Dose Specimen")
      }

      if (length(not_compatible) != 0) {
        msg <- paste0(
          paste0(not_compatible, collapse = ", "),
          " not compatible with current data, leaving as default."
        )
        log_warn(msg)
        showNotification(msg, type = "warning", duration = 10)
      }

      updateSelectInput(inputId = "method", selected = settings$method)

      if (!is.null(settings$bioavailability))
        updateSelectInput(inputId = "bioavailability", selected = settings$bioavailability)

      # Parmeter selection #
      reset_reactable_memory()

      params_data <- metadata_nca_parameters %>%
        filter(TYPE != "PKNCA-not-covered") %>%
        pull("PKNCA")

      updateReactable(
        "nca_parameters",
        selected = which(params_data %in% settings$parameter_selection)
      )

      # Data imputation #
      update_switch("should_impute_c0", value = settings$data_imputation$impute_c0)

      # Partial AUCs #
      auc_data(settings$partial_aucs)
      refresh_reactable(refresh_reactable() + 1)

      # Flags #
      .update_rule_input(
        session,
        "adj.r.squared",
        settings$flags$adj.r.squared$is.checked,
        settings$flags$adj.r.squared$threshold
      )

      .update_rule_input(
        session,
        "aucpext.obs",
        settings$flags$aucpext.obs$is.checked,
        settings$flags$aucpext.obs$threshold
      )

      .update_rule_input(
        session,
        "aucpext.pred",
        settings$flags$aucpext.pred$is.checked,
        settings$flags$aucpext.pred$threshold
      )

      .update_rule_input(
        session,
        "span.ratio",
        settings$flags$span.ratio$is.checked,
        settings$flags$span.ratio$threshold
      )
    })

    # Include keyboard limits for the settings GUI display

    # Keyboard limits for the setting thresholds
    limit_input_value(input, session, "adj.r.squared_threshold", max = 1, min = 0, lab = "RSQADJ")
    limit_input_value(input, session, "aucpext.obs_threshold", max = 100, min = 0, lab = "AUCPEO")
    limit_input_value(input, session, "aucpext.pred_threshold", max = 100, min = 0, lab = "AUCPEP")
    limit_input_value(input, session, "span.ratio_threshold", min = 0, lab = "SPAN")


    # Choose data to be analyzed
    observeEvent(data(), priority = -1, {
      req(data())

      choices <- unique(data()$PARAM) %>%
        na.omit()

      updatePickerInput(
        session,
        inputId = "select_analyte",
        choices = choices,
        selected = choices
      )

    })

    observeEvent(input$select_analyte, {
      req(data(), input$select_analyte)

      filtered_data <- data() %>%
        filter(PARAM %in% input$select_analyte,
               !is.na(PCSPEC),
               !is.na(NCA_PROFILE)) # Filter together so there's no combinations of NAs

      profile_choices <- unique(filtered_data$NCA_PROFILE) %>%
        sort()

      pcspec_choices <- unique(filtered_data$PCSPEC)

      updatePickerInput(
        session,
        inputId = "select_profile",
        choices = profile_choices,
        selected = profile_choices[1]
      )

      updatePickerInput(
        session,
        inputId = "select_pcspec",
        choices = pcspec_choices,
        selected = pcspec_choices
      )
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

    output$nca_parameters <- renderReactable({
      #remove parameters that are currently unavailable in PKNCA
      params_data <- metadata_nca_parameters %>%
        filter(TYPE != "PKNCA-not-covered")

      default_row_indices <- which(params_data$PKNCA %in% DEFAULT_PARAMS)

      reactable(
        params_data %>%
          select(TYPE, PPTESTCD, PPTEST, CAT),
        groupBy = c("TYPE"),
        pagination = FALSE,
        filterable = TRUE,
        compact = TRUE,
        onClick = "select",
        height = "49vh",
        selection = "multiple",
        defaultSelected = default_row_indices
      )
    })

    nca_params <- reactive({
      selected_rows <- getReactableState("nca_parameters", "selected")
      if (is.null(selected_rows) || length(selected_rows) == 0) return(NULL)

      params_data <- metadata_nca_parameters %>%
        filter(TYPE != "PKNCA-not-covered")
      selected_terms <- params_data[selected_rows, , drop = FALSE]

      # Return PKNCA column names
      selected_terms$PKNCA
    })

    output$nca_param_display <- renderUI({
      req(nca_params())

      div(
        class = "nca-pill-grid",
        lapply(nca_params(), function(param) {
          tags$span(class = "nca-pill", param)
        })
      )
    })

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
      purrr::walk(c("start_auc", "end_auc"), \(colname) {
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
        parameter_selection = nca_params(),
        data_imputation = list(
          impute_c0 = input$should_impute_c0
        ),
        partial_aucs = auc_data(),
        flags = list(
          adj.r.squared = list(
            is.checked = input$adj.r.squared_rule,
            threshold = input$adj.r.squared_threshold
          ),
          aucpext.obs = list(
            is.checked = input$aucpext.obs_rule,
            threshold = input$aucpext.obs_threshold
          ),
          aucpext.pred = list(
            is.checked = input$aucpext.pred_rule,
            threshold = input$aucpext.pred_threshold
          ),
          span.ratio = list(
            is.checked = input$span.ratio_rule,
            threshold = input$span.ratio_threshold
          )
        )
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
          '.run-nca-btn',
          {settings_debounce + 250},
          'Applying<br>settings...',
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
#' @returns `shiny::fluidRow` containing html elements of the widget.
.rule_input <- function(id, label, default, step, min, max = NULL) {
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

  fluidRow(
    column(
      width = 6,
      checkboxInput(rule_id, label, value = TRUE)
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
