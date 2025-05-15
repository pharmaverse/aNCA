# Rule input helper ui

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
      checkboxInput(rule_id, label)
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

nca_setup_ui <- function(id) {
  ns <- NS(id)

  navset_tab(
    id = ns("setup_tabs"),
    nav_panel(
      "Setup",
      # Local upload option
      fileInput(
        ns("settings_upload"),
        width = "60%",
        label = "Upload Settings",
        buttonLabel = list(icon("folder"), "Browse"),
        accept = c(".csv", ".xpt")
      ),
      accordion(
        accordion_panel(
          title = "General Settings",
          # Selection of analyte, dose number and specimen
          fluidRow(
            column(4, selectInput(ns("select_analyte"), "Choose the Analyte :", multiple = TRUE,
                                  choices = NULL)),
            column(4, selectInput(ns("select_dosno"), "Choose the Dose Number:", multiple = TRUE,
                                  choices = NULL)),
            column(4, selectInput(ns("select_pcspec"), "Choose the Specimen:", multiple = TRUE,
                                  choices = NULL))
          ),
          # Method, NCA parameters, and units table
          fluidRow(
            column(4, selectInput(
              ns("method"),
              "Extrapolation Method:",
              choices = c(
                "lin-log", "lin up/log down", "linear"
              ),
              selected = "lin up/log down"
            )),
            column(4, #pickerinput only enabled when IV and EX data present
                   shinyjs::hidden(
                     pickerInput(
                       ns("bioavailability"),
                       "Calculate Bioavailability:",
                       choices = pknca_cdisc_terms$PPTESTCD %>%
                         subset(startsWith(., "FABS_") | startsWith(., "FREL_")),
                       multiple = TRUE,
                       selected = NULL
                     )
                   )),
            column(4, units_table_ui(ns("units_table")))
          ),
        ),
        accordion_panel(
          title = "Parameter Selection",
          reactableOutput(ns("nca_parameters")),
          card(
            full_screen = FALSE,
            style = "margin-top: 2em;",
            card_header("Selected NCA Parameters"),
            card_body(
              uiOutput(ns("nca_param_display"))
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
            "- If DOSNO = 1 & IV bolus: C0 = 0",
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
    ),
    nav_panel(
      "Summary",
      reactableOutput(ns("nca_intervals"))
    )
  )
}

#' NCA Settings Server Module
#'
#' This module handles the server-side logic for the NCA settings, including file uploads,
#' analyte/dose/specimen selection, AUC intervals, NCA parameters and their unit specifications.
#' Then it integrates them to adnca_data and updates the object.
#'
#' - id The module's ID.
#' - data A reactive expression containing the read and mapped data from the app.
#'        It is only used for the file uploads and the analyte/dose/specimen selection.
#' - adnca_data A reactive expression of the PKNCAdata object,
#'  which contains data and NCA specifications.
nca_setup_server <- function(id, data, adnca_data) { # nolint : TODO: complexity / needs further modularization

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    conc_data <- reactive(adnca_data()$conc$data)

    # File Upload Handling
    observeEvent(input$settings_upload, {
      setts <- read.csv(input$settings_upload$datapath, na = c("", "NA"))
      param <- setts$PARAM[1]
      doses_selected <- as.numeric(strsplit(as.character(setts$doses_selected), split = ",")[[1]])

      if (!param %in% unique(data()$PARAM) || !all(doses_selected %in% unique(data()$DOSNO))) {
        showNotification(
          validate("The analyte selected in the settings file is not present in the data. Please, if
                    you want to use these settings for a different file, make sure all meaningful
                    variables in the file are in the data (PARAM, DOSNO...)"),
          type = "error"
        )
      }

      new_data <- data() %>%
        filter(
          PARAM == param,
          if ("EVID" %in% names(data())) EVID == 0 else TRUE
        ) %>%
        mutate(groups = paste0(USUBJID, ", ", DOSNO)) %>%
        filter(TIME >= 0) %>%
        arrange(STUDYID, USUBJID, PCSPEC, DOSNO, TIME) %>%
        group_by(STUDYID, USUBJID, PCSPEC, DOSNO) %>%
        mutate(IX = seq_len(n())) %>%
        select(STUDYID, USUBJID, AVAL, DOSNO, TIME, IX)

      setts_lambda <- setts %>%
        select(STUDYID, USUBJID, DOSNO, IX, AVAL, TIME) %>%
        na.omit()

      mismatched_points <- setts_lambda %>%
        anti_join(new_data, by = c("USUBJID", "DOSNO", "IX", "AVAL", "TIME"))

      if (nrow(mismatched_points) > 0) {
        showModal(modalDialog(
          title = "Mismatched Data Points",
          tags$h4("The following data points in the settings file do not match the uploaded dataset.
                  The slopes for these profiles will be reset to best slope:"),
          DTOutput(ns("mismatched_table")),
          easyClose = TRUE,
          footer = NULL
        ))

        output$mismatched_table <- DT::renderDT({
          datatable(mismatched_points %>% select(-IX),
                    fillContainer = TRUE)
        })

        setts <- setts %>%
          anti_join(mismatched_points, by = c("USUBJID", "DOSNO"))
      }

      rows_for_selected_analytes <- data() %>%
        filter(PARAM %in% setts$PARAM) %>%
        select(PARAM, DOSNO, PCSPEC) %>%
        unique()

      updateSelectInput(
        session,
        inputId = "select_analyte",
        label = "Choose the analyte:",
        choices = data()$PARAM[1],
        selected = setts$PARAM[1]
      )

      updateSelectInput(
        session,
        inputId = "select_dosno",
        label = "Choose the Dose Number:",
        choices = rows_for_selected_analytes$DOSNO,
        selected = doses_selected
      )

      updateSelectInput(
        session,
        inputId = "select_pcspec",
        label = "Choose the Specimen/Matrix:",
        choices = rows_for_selected_analytes$PCSPEC,
        selected = rows_for_selected_analytes$PCSPEC
      )

      updateSelectInput(
        session,
        inputId = "method",
        label = "Extrapolation Method:",
        choices = c("lin-log", "lin up/log down", "linear"),
        selected = setts$method
      )


      if (!is.na(setts$adj.r.squared_threshold[1])) {
        updateCheckboxInput(session,
                            inputId = ns("rule_adj_r_squared"),
                            label = "RSQADJ:",
                            value = TRUE)
        updateNumericInput(
          session,
          ns("adj.r.squared_threshold"),
          "",
          value = setts$adj.r.squared_threshold[1]
        )
      } else {
        updateCheckboxInput(session,
                            inputId = ns("rule_adj_r_squared"),
                            label = "RSQADJ:",
                            value = FALSE)
      }

      if (!is.na(setts$aucpext.obs_threshold[1])) {
        updateCheckboxInput(session,
                            inputId = ns("rule_aucpext_obs"),
                            value = TRUE)
        updateNumericInput(session,
                           ns("aucpext.obs_threshold"),
                           value = setts$aucpext.obs_threshold[1])
      } else {
        updateCheckboxInput(session,
                            inputId = ns("rule_aucpext_obs"),
                            label = "",
                            value = FALSE)
      }

      if (!is.na(setts$aucpext.pred_threshold[1])) {
        updateCheckboxInput(session,
                            inputId = ns("rule_aucpext_pred"),
                            value = TRUE)
        updateNumericInput(session,
                           ns("aucpext.pred_threshold"),
                           value = setts$aucpext.pred_threshold[1])
      } else {
        updateCheckboxInput(session,
                            inputId = ns("rule_aucpext_pred"),
                            value = FALSE)
      }

      if (!is.na(setts$span.ratio_threshold[1])) {
        updateCheckboxInput(session,
                            inputId = ns("rule_span_ratio"),
                            label = "SPAN: ",
                            value = TRUE)
        updateNumericInput(session,
                           ns("span.ratio_threshold"),
                           "",
                           value = setts$span.ratio_threshold[1])
      } else {
        updateCheckboxInput(session,
                            inputId = ns("rule_span_ratio"),
                            label = "SPAN:",
                            value = FALSE)
      }
    })

    # Include keyboard limits for the settings GUI display

    # Keyboard limits for the setting thresholds
    limit_input_value(input, session, "adj.r.squared_threshold", max = 1, min = 0, lab = "RSQADJ")
    limit_input_value(input, session, "aucpext.obs_threshold", max = 100, min = 0, lab = "AUCPEO")
    limit_input_value(input, session, "aucpext.pred_threshold", max = 100, min = 0, lab = "AUCPEP")
    limit_input_value(input, session, "span.ratio_threshold", min = 0, lab = "SPAN")


    # Choose dosenumbers to be analyzed
    observeEvent(data()$DOSNO, priority = -1, {
      req(data())

      updateSelectInput(
        session,
        inputId = "select_dosno",
        choices = unique(data()$DOSNO),
        selected = unique(data()$DOSNO)[1]
      )

      updateSelectInput(
        session,
        inputId = "select_analyte",
        choices = unique(data()$PARAM),
        selected = unique(data()$PARAM)[1]
      )

      updateSelectInput(
        session,
        inputId = "select_pcspec",
        choices = unique(data()$PCSPEC),
        selected = unique(data()$PCSPEC)[1]
      )
    })

    DEFAULT_PARAMS <- c(
      "aucinf.obs", "aucinf.obs.dn",
      "aucint.last",
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

    output$nca_parameters <- renderReactable({
      #remove parameters that are currently unavailable in PKNCA
      params_data <- pknca_cdisc_terms %>%
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

      params_data <- pknca_cdisc_terms %>%
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

    # Updating Checkbox and Numeric Inputs
    observeEvent(list(input$adj.r.squared_rule, input$aucpext.obs_rule,
                      input$aucpext.pred_rule, nca_params()), {

                   nca_params <- nca_params()
                   if (input$adj.r.squared_rule) nca_params <- c(nca_params, "adj.r.squared")
                   if (input$aucpext.obs_rule) nca_params <- c(nca_params, "aucpext.obs")
                   if (input$aucpext.pred_rule) nca_params <- c(nca_params, "aucpext.pred")

                   updatePickerInput(session = session,
                                     inputId = "nca_params",
                                     selected = nca_params)
                 })

    # Create trigger for modifications to the user setup
    setup_trigger <- reactive({
      paste(
        adnca_data(),
        auc_data(),
        input$method,
        nca_params(),
        input$should_impute_c0,
        input$select_analyte,
        input$select_dosno,
        input$select_pcspec
      )
    })

    # Debounce the trigger, so the data is not updated too often.
    setup_debounce <- 2500
    setup_trigger_debounced <- debounce(setup_trigger, setup_debounce)

    # On all changes, disable NCA button for given period of time to prevent the user from running
    # the NCA before settings are applied.
    observeEvent(setup_trigger(), {
      runjs(str_glue(
        "buttonTimeout(
          '.run-nca-btn',
          {setup_debounce + 250},
          'Applying<br>settings...',
          'Run NCA'
        );"
      ))
    })

    # Create version for slope plots
    # Only parameters required for the slope plots are set in intervals
    # NCA dynamic changes/filters based on user selections
    slopes_pknca_data <- eventReactive(setup_trigger_debounced(), {
      req(adnca_data(), input$method, input$select_analyte,
          input$select_dosno, input$select_pcspec)
      log_trace("Updating PKNCA::data object for slopes.")
      slopes_pknca_data <- PKNCA_update_data_object(
        adnca_data = adnca_data(),
        auc_data = auc_data(),
        method = input$method,
        selected_analytes = input$select_analyte,
        selected_dosno = input$select_dosno,
        selected_pcspec = input$select_pcspec,
        params = c("lambda.z.n.points", "lambda.z.time.first",
                   "r.squared", "adj.r.squared", "tmax"),
        should_impute_c0 = input$should_impute_c0
      )

      slopes_pknca_data
    })

    # Create version for NCA results
    # NCA dynamic changes/filters based on user selections
    processed_pknca_data <- eventReactive(setup_trigger_debounced(), {
      req(adnca_data(), input$method, input$select_analyte,
          input$select_dosno, input$select_pcspec, auc_data())
      log_trace("Updating PKNCA::data object.")

      processed_pknca_data <- PKNCA_update_data_object(
        adnca_data = adnca_data(),
        auc_data = auc_data(),
        method = input$method,
        selected_analytes = input$select_analyte,
        selected_dosno = input$select_dosno,
        selected_pcspec = input$select_pcspec,
        params = nca_params(),
        should_impute_c0 = input$should_impute_c0
      )

      # Add picker input if bioavailability calculations are possible
      if (processed_pknca_data$dose$data$std_route %>% unique() %>% length() == 2) {
        shinyjs::show("bioavailability")

        f_options <- pknca_cdisc_terms %>%
          filter(startsWith(PPTESTCD, "FABS_") | startsWith(PPTESTCD, "FREL_")) %>%
          pull(PKNCA, PPTESTCD)

        updatePickerInput(
          session,
          inputId = "bioavailability",
          "Calculate Bioavailability:",
          choices = f_options,
          selected = f_options[1]
        )
      }

      if (nrow(processed_pknca_data$intervals) == 0) {
        showNotification(
          "All intervals were filtered. Please revise your settings.",
          type = "warning", duration = 10
        )
      }

      processed_pknca_data
    })

    # Parameter unit changes option: Opens a modal message with a units table to edit
    units_table_server("units_table", processed_pknca_data)

    # Rendering Reactable Output
    output$nca_intervals <- renderReactable({
      req(processed_pknca_data())

      data <- processed_pknca_data()$intervals %>%
        apply_labels(LABELS, "ADPC") %>%
        select(where(~!is.logical(.) | any(. == TRUE)))

      route_column <- "ROUTE"
      std_route_column <- "std_route"
      col_groups <- unname(unlist(processed_pknca_data()$dose$columns$groups))

      data <- data %>%
        left_join(processed_pknca_data()$dose$data %>%
                    select(all_of(c(col_groups, route_column, std_route_column,
                                    "TIME_DOSE", "DOSNO", "DOSNOA"))),
                  by = c(col_groups, "TIME_DOSE", "DOSNO", "DOSNOA")) %>%
        group_by(across(all_of(unname(unlist(processed_pknca_data()$dose$columns$groups))))) %>%
        arrange(!!!syms(unname(unlist(processed_pknca_data()$conc$columns$groups))), TIME_DOSE) %>%
        mutate(start = start - TIME_DOSE, end = end - TIME_DOSE) %>%
        select(!!!syms(colnames(data)),
               all_of(c(route_column, std_route_column)))

      reactable(
        data,
        columns = generate_col_defs(data),
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = TRUE,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        bordered = TRUE,
        height = "98vh"
      )
    })

    list(
      processed_pknca_data = processed_pknca_data,
      slopes_pknca_data = slopes_pknca_data,
      rules = reactive(list(
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
      )),
      bioavailability = reactive(input$bioavailability)
    )
  })
}
