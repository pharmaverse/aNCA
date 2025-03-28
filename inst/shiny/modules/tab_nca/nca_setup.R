nca_setup_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Local upload option
    fluidRow(
      fileInput(
        ns("settings_upload"),
        width = "60%",
        label = "Upload Settings",
        buttonLabel = list(icon("folder"), "Browse"),
        accept = c(".csv", ".xpt")
      )
    ),
    accordion(
      accordion_panel(
        title = "General Settings",
        # Selection of analyte, dose number and specimen
        fluidRow(
          column(4, selectInput(ns("select_analyte"), "Choose the analyte :", multiple = TRUE,
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
              "lin-log", "lin up/log down", "linear", "Linear LinearLogInterpolation"
            ),
            selected = "lin up/log down"
          )),
          column(4, pickerInput(
            inputId = ns("nca_params"),
            label = "NCA parameters to calculate:",
            choices = {
              params <- sort(setdiff(names(PKNCA::get.interval.cols()),
                                     c("start", "end")))
              group_params <- case_when(
                grepl("((auc|aum))", params) ~ "Exposure",
                grepl("((lambda|half|thalf|cl\\.|clr))", params) ~ "Clearance",
                grepl("^(cm|tm|clast|ceoi|cth|tl)", params) ~ "Concentration-Time",
                grepl("^(vz|vs)", params) ~ "Volume-Distribution",
                TRUE ~ "Miscellaneous"
              )
              grouped_params <- split(params, group_params)
              grouped_params[order(names(grouped_params))]
            },
            options = list(
              `live-search` = TRUE,
              `dropup-auto` = FALSE,
              `size` = 10,
              `noneSelectedText` = "No parameters selected",
              `windowPadding` = 10,
              `dropdownAlignRight` = TRUE
            ),
            multiple = TRUE,
            selected = c("cmax", "tmax", "half.life", "cl.obs", "auclast",
                         "aucinf.pred", "aucinf.obs", "aucinf.obs.dn",
                         "adj.r.squared", "lambda.z", "lambda.z.n.points",
                         "cav", "cl.all", "cl.obs",
                         "clast", "tlast")
          )),
          column(4, units_table_ui(ns("units_table")))
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
        fluidRow(
          column(
            width = 6,
            checkboxInput(ns("rule_adj_r_squared"), "RSQADJ:")
          ),
          column(
            width = 6,
            conditionalPanel(
              condition = paste0("input['", ns("rule_adj_r_squared"), "'] == true"),
              div(
                class = "nca-numeric-container",
                numericInput(
                  ns("adj.r.squared_threshold"),
                  "",
                  value = 0.7,
                  step = 0.05,
                  min = 0,
                  max = 1
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            checkboxInput(ns("rule_aucpext_obs"), "AUCPEO (% ext.observed): ")
          ),
          column(
            width = 6,
            conditionalPanel(
              condition = paste0("input['", ns("rule_aucpext_obs"), "'] == true"),
              div(
                class = "nca-numeric-container",
                numericInput(
                  ns("aucpext.obs_threshold"),
                  "",
                  value = 20,
                  step = 1,
                  min = 0,
                  max = 100
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            checkboxInput(ns("rule_aucpext_pred"), "AUCPEP (% ext.predicted): ")
          ),
          column(
            width = 6,
            conditionalPanel(
              condition = paste0("input['", ns("rule_aucpext_pred"), "'] == true"),
              div(
                class = "nca-numeric-container",
                numericInput(
                  ns("aucpext.pred_threshold"),
                  "",
                  value = 20,
                  step = 5,
                  min = 0,
                  max = 100
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            checkboxInput(ns("rule_span_ratio"), "SPAN: ")
          ),
          column(
            width = 6,
            conditionalPanel(
              condition = paste0("input['", ns("rule_span_ratio"), "'] == true"),
              div(
                class = "nca-numeric-container",
                numericInput(
                  ns("span.ratio_threshold"),
                  "",
                  value = 2,
                  step = 1,
                  min = 0
                )
              )
            )
          )
        ),
      ),
      id = "acc",
      open = "General Settings"
    ),

    h4("View of NCA settings"),
    reactableOutput(ns("nca_intervals"))
  )
}

#' NCA Settings Server Module
#'
#' This module handles the server-side logic for the NCA settings, including file uploads,
#' analyte/dose/specimen selection, AUC intervals, NCA parameters and their unit specifications.
#' Then it integrates them to mydata and updates the object.
#'
#' - id The module's ID.
#' - data A reactive expression containing the read and mapped data from the app.
#'        It is only used for the file uploads and the analyte/dose/specimen selection.
#' - mydata A reactive expression of the PKNCAdata object, containing data and NCA specifications.
#'
nca_setup_server <- function(id, data, mydata) { # nolint : TODO: complexity / needs further modularization

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    conc_data <- reactive(mydata()$conc$data)

    # File Upload Handling
    observeEvent(input$settings_upload, {
      setts <- read.csv(input$settings_upload$datapath, na = c("", "NA"))
      analyte <- setts$ANALYTE[1]
      doses_selected <- as.numeric(strsplit(as.character(setts$doses_selected), split = ",")[[1]])

      if (!analyte %in% unique(data()$ANALYTE) || !all(doses_selected %in% unique(data()$DOSNO))) {
        showNotification(
          validate("The analyte selected in the settings file is not present in the data. Please, if
                    you want to use this settings for a different file, make sure all meaningful
                    variables in the file are in the data (ANALYTE, DOSNO...)"),
          type = "error"
        )
      }

      new_data <- data() %>%
        filter(
          ANALYTE == analyte,
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
        filter(ANALYTE %in% setts$ANALYTE) %>%
        select(ANALYTE, DOSNO, PCSPEC) %>%
        unique()

      updateSelectInput(
        session,
        inputId = "select_analyte",
        label = "Choose the analyte:",
        choices = data()$ANALYTE[1],
        selected = setts$ANALYTE[1]
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
    limit_input_value(input, session, "adj.r.squared_threshold", max = 1, min = 0, lab = "R.SQ.ADJ")
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
        choices = unique(data()$ANALYTE),
        selected = unique(data()$ANALYTE)[1]
      )

      updateSelectInput(
        session,
        inputId = "select_pcspec",
        choices = unique(data()$PCSPEC),
        selected = unique(data()$PCSPEC)[1]
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
    observeEvent(list(input$rule_adj_r_squared, input$rule_aucpext_obs,
                      input$rule_aucpext_pred, input$nca_params), {

                   nca_params <- input$nca_params
                   if (input$rule_adj_r_squared) nca_params <- c(nca_params, "adj.r.squared")
                   if (input$rule_aucpext_obs) nca_params <- c(nca_params, "aucpext.obs")
                   if (input$rule_aucpext_pred) nca_params <- c(nca_params, "aucpext.pred")

                   updatePickerInput(session = session,
                                     inputId = "nca_params",
                                     selected = nca_params)
                 })


    # NCA settings dynamic changes
    processed_pknca_data <- eventReactive(list(
      input$method, input$nca_params, input$should_impute_c0,
      input$select_analyte, input$select_dosno, input$select_pcspec,
      mydata(), session$userData$units_table(), auc_data()
    ), {
      req(mydata())
      req(
        input$method, input$nca_params, input$select_analyte,
        input$select_dosno, input$select_pcspec, auc_data()
      )

      # Load mydata reactive and modify it accordingly to user's request
      processed_pknca_data <- mydata()

      analyte_column <- processed_pknca_data$conc$columns$groups$group_analyte
      unique_analytes <- unique(processed_pknca_data$conc$data[[analyte_column]])

      if (!is.null(session$userData$units_table())) {
        processed_pknca_data$units <- session$userData$units_table()
      }
      processed_pknca_data$units <- processed_pknca_data$units %>%
        select(-!!sym(analyte_column)) %>%
        tidyr::crossing(!!sym(analyte_column) := unique_analytes)  %>%
        mutate(PPSTRESU = PPORRESU, conversion_factor = 1)  %>%
        filter(ANALYTE %in% input$select_analyte)

      processed_pknca_data$options <- list(
        auc.method = input$method,
        keep_interval_cols = c("DOSNO", "type_interval"),
        # Make sure the standard options do not prohibit results
        min.hl.r.squared = 0.01
      )

      # Include main intervals as specified by the user
      intervals <- format_pkncadata_intervals(
        pknca_conc = processed_pknca_data$conc,
        pknca_dose = processed_pknca_data$dose,
        # Compute only parameters specified
        params = input$nca_params,
        # Start at t0 when requested by the user
        start_from_last_dose = input$should_impute_c0
      )

      # Collect non-NA min/max values from reactable
      auc_ranges <- auc_data() %>%
        filter(!is.na(start_auc), !is.na(end_auc), start_auc >= 0, end_auc > start_auc)

      # Make a list of intervals from valid AUC ranges
      intervals_list <- pmap(auc_ranges, function(start_auc, end_auc) {
        intervals %>%
          mutate(
            start = start + start_auc,
            end = start + (end_auc - start_auc),
            across(where(is.logical), ~FALSE),
            aucint.last = TRUE,
            type_interval = "manual"
          )
      })

      # Join custom AUC partial intervals specified by the user

      processed_pknca_data$intervals <- bind_rows(
        intervals,
        intervals_list
      ) %>%
        unique()

      processed_pknca_data$impute <- NA_character_

      # Filter only the analytes and doses requested for intervals and units
      processed_pknca_data$intervals <- processed_pknca_data$intervals %>%
        dplyr::filter(DOSNO %in% input$select_dosno,
                      ANALYTE %in% input$select_analyte,
                      PCSPEC %in% input$select_pcspec)

      # Define start imputations on intervals if specified by the user
      if (input$should_impute_c0) {
        processed_pknca_data <- create_start_impute(processed_pknca_data)

        # Make sure observed parameters (cmax, tmax) are not imputed
        # ToDo: It would make sense to vectorize the fun so target_impute accepts a vector
        processed_pknca_data$intervals <- Reduce(function(data, ti_arg) {
          interval_remove_impute(data,
                                 target_impute = ti_arg,
                                 target_params = c("cmax", "tmax"))
        }, unique(processed_pknca_data$intervals$impute), init = processed_pknca_data$intervals)
      }

      # If the user filtered all intervals or there are not left, show a notification
      if (nrow(processed_pknca_data$intervals) == 0) {
        showNotification(paste0("All intervals have been filtered. Please reconsider ",
                                "populating the last input modified or report a bug"),
                         duration = 20,
                         type = "warning")
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

      data <- data %>%
        left_join(y = processed_pknca_data()$dose$data) %>%
        group_by(across(all_of(unname(unlist(processed_pknca_data()$dose$columns$groups))))) %>%
        arrange(!!!syms(unname(unlist(processed_pknca_data()$conc$columns$groups))), TIME) %>%
        mutate(start = start - first(TIME), end = end - first(TIME)) %>%
        select(!!!syms(colnames(data)), conc_groups,
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
      rules = reactive(list(
        rule_adj_r_squared = input$rule_adj_r_squared,
        adj.r.squared_threshold = input$adj.r.squared_threshold,

        rule_aucpext_obs = input$rule_aucpext_obs,
        aucpext.obs_threshold = input$aucpext.obs_threshold,

        rule_aucpext_pred = input$rule_aucpext_pred,
        aucpext.pred_threshold = input$aucpext.pred_threshold,

        rule_span_ratio = input$rule_span_ratio,
        span.ratio_threshold = input$span.ratio_threshold
      ))
    )
  })
}
