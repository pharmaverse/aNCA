nca_settings_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Local upload option
    fileInput(
      ns("settings_upload"),
      width = "60%",
      label = "Upload Settings",
      buttonLabel = list(icon("folder"), "Browse"),
      accept = c(".xlsx", ".rds")
    ),
    br(),

    # Selection of analyte, dose number and specimen
    fluidRow(
      column(4, selectInput(ns("select_analyte"), "Choose the analyte :", multiple = TRUE,
                            choices = NULL)),
      column(4, selectInput(ns("select_dosno"), "Choose the Dose Number:", multiple = TRUE,
                            choices = NULL)),
      column(4, selectInput(ns("select_pcspec"), "Choose the Specimen:", multiple = TRUE,
                            choices = NULL))
    ),
    br(),

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
        selected = c("cmax", "tmax", "half.life", "cl.obs",
                     "aucinf.pred", "aucinf.obs", "aucinf.obs.dn",
                     "adj.r.squared", "lambda.z", "lambda.z.n.points",
                     "cav", "cl.all", "cl.obs", "aucpext.obs",
                     "clast", "tlast")
      )),
      column(4, units_table_ui(ns("units_table_preNCA")))
    ),
    br(),

    h4("Data imputation"),
    tags$div(
      checkboxInput(
        inputId = ns("should_impute_c0"),
        label = "Impute concentration at t0 when missing",
        value = TRUE
      ),
      id = ns("checkbox_id"),
      title = paste(
        "Imputes a start-of-interval concentration",
        "to calculate non-observational parameters:",
        "If DOSNO = 1 & IV bolus: C0 = 0",
        "If DOSNO > 1 & not IV bolus: C0 = predose",
        "If IV bolus & monoexponential data: logslope",
        "If IV bolus & not monoexponential data: C0 = C1",
        sep = "\n"
      )
    ),
    br(),
    checkboxInput(ns("AUCoptions"), "Select Partial AUC"),
    conditionalPanel(
      condition = paste0("input['", ns("AUCoptions"), "'] == true"),
      fluidRow(
        column(
          width = 12,
          actionButton(ns("addAUC"), "+"),
          actionButton(ns("removeAUC"), "-")
        )
      ),
      tags$div(id = ns("AUCInputs")) # Container for dynamic partial AUC inputs
    ),
    h4("Flag Rule Sets:"),
    fluidRow(
      column(
        width = 6,
        checkboxInput(ns("rule_adj.r.squared"), "RSQADJ:")
      ),
      column(
        width = 6,
        conditionalPanel(
          condition = paste0("input['", ns("rule_adj.r.squared"), "'] == true"),
          div(
            style = "display: flex; align-items: center;",
            span(">=", style = "margin-right: 5px;"),
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
        checkboxInput(ns("rule_aucpext.obs"), "AUCPEO (% ext.observed): ")
      ),
      column(
        width = 6,
        conditionalPanel(
          condition = paste0("input['", ns("rule_aucpext.obs"), "'] == true"),
          div(
            style = "display: flex; align-items: center;",
            span(">=", style = "margin-right: 5px;"),
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
        checkboxInput(ns("rule_aucpext.pred"), "AUCPEP (% ext.predicted): ")
      ),
      column(
        width = 6,
        conditionalPanel(
          condition = paste0("input['", ns("rule_aucpext.pred"), "'] == true"),
          div(
            style = "display: flex; align-items: center;",
            span(">=", style = "margin-right: 5px;"),
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
        checkboxInput(ns("rule_span.ratio"), "SPAN: ")
      ),
      column(
        width = 6,
        conditionalPanel(
          condition = paste0("input['", ns("rule_span.ratio"), "'] == true"),
          div(
            style = "display: flex; align-items: center;",
            span(">=", style = "margin-right: 5px;"),
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
    h4("View of NCA settings"),
    reactableOutput(ns("nca_intervals"))
  )
}

nca_settings_server <- function(id, data, mydata, res_nca) { # nolint : complexity / needs further modularization

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # File Upload Handling
    observeEvent(input$settings_upload, {
      
      file_path <- input$settings_upload$datapath

      if (tools::file_ext(file_path) == "rds") {
        setts <- readRDS(input$settings_upload$datapath)

      } else if (tools::file_ext(file_path) == "xlsx") {

        # Get sheet names and read all into a list
        sheets <- openxlsx::getSheetNames(file_path)
        setts <- lapply(sheets, function(sheet) openxlsx::read.xlsx(file_path, sheet = sheet))
        names(setts) <- sheets

        # Rearrange the list as in a PKNCAdata object
        setts <- list(
          conc = list(
            data = setts$conc_data,
            columns = list(
              groups = list(
                group_analyte = setts$conc_columns  %>% filter(ind == "groups.group_analyte")  %>% pull(values),
                group_group_vars = setts$conc_columns  %>% filter(ind == "groups.group_vars")  %>% pull(values)
              ),
              time = setts$conc_columns  %>% filter(ind == "time")  %>% pull(values),
              concentration = setts$conc_columns  %>% filter(ind == "concentration")  %>% pull(values),
              subject = setts$conc_columns  %>% filter(ind == "subject")  %>% pull(values),
              time.nominal = setts$conc_columns  %>% filter(ind == "time.nominal")  %>% pull(values),
              exclude = setts$conc_columns  %>% filter(ind == "exclude")  %>% pull(values),
              duration = setts$conc_columns  %>% filter(ind == "duration")  %>% pull(values),
              exclude_half.life = setts$conc_columns  %>% filter(ind == "exclude_half.life")  %>% pull(values)
            )
          ),
          dose = list(
            data = setts$dose_data,
            columns = list(
              groups = list(
                group_vars = setts$dose_columns  %>% filter(ind == "groups.group_vars")  %>% pull(values)
              ),
              time = setts$dose_columns  %>% filter(ind == "time")  %>% pull(values),
              dose = setts$dose_columns  %>% filter(ind == "dose")  %>% pull(values),
              time.nominal = setts$dose_columns  %>% filter(ind == "time.nominal")  %>% pull(values),
              exclude = setts$dose_columns  %>% filter(ind == "exclude")  %>% pull(values),
              duration = setts$dose_columns  %>% filter(ind == "duration")  %>% pull(values)
            )
          ),
          intervals = setts$intervals,
          options = setts$options,
          units = setts$units,
          flag_rules = setts$flag_rules
        )
      }

      update_with_setts_selector_selected <- function(var_setts_col, var_data_col, setts.df, data.df, session, inputId) {
        if (!is.null(var_data_col) && !is.null(var_setts_col)) {
          vals_data <- unique(data.df[[var_data_col]])
          vals_setts <- unique(setts.df[[var_setts_col]])

          selected_vals <- intersect(vals_setts, vals_data)

          # Produce a warning if any value in settings is not in data
          if (length(selected_vals) < length(vals_setts)) {
            showNotification(
              validate("The ", attr(data.df[[var_data_col]], "label"), ":",
                      setdiff(analytes_setts, selected_vals),
                      "selected in the settings file is not present in the data."),
              type = "warning"
            )
          }

          # Update the selectInput with the selected values from the settings
          updateSelectInput(
            session,
            inputId = inputId,
            choices = data.df[[var_data_col]],
            selected = selected_vals
          )
        } else return()
      }

      data <- mydata()

      # Update selected values for the data selectors
      update_with_setts_selector_selected("DOSNO", "DOSNO", setts.df = setts, data.df = mydata()$conc$data, session = session, inputId = "select_dosno")
      update_with_setts_selector_selected(var_setts_col = setts$conc$columns$groups$group_analyte,
                                          var_data_col = mydata()$conc$columns$groups$group_analyte,
                                          setts.df = setts$intervals, data.df = mydata()$conc$data, session, "select_analyte")
      update_with_setts_selector_selected("PCSPEC", "PCSPEC", setts, data, session, "select_pcspec")

      # Update the AUC method
      updateSelectInput(
        session,
        inputId = "method",
        label = "Extrapolation Method:",
        choices = c("lin-log", "lin up/log down", "linear"),
        selected = setts$options$auc.method
      )

      # Update the NCA parameters 
      all_param_options <- setdiff(names(PKNCA::get.interval.cols()), c("start", "end"))
      
      params_setts <- setts$intervals  %>%
        filter(type_interval == "main") %>%
        mutate(across(all_of(all_param_options), ~ifelse(. == FALSE & is.logical(.), NA, .))) %>%
        select(any_of(all_param_options)) %>%
        select_if(~any(!is.na(.))) %>%
        names()

      updatePickerInput(
        session,
        inputId = ns("nca_params"),
        selected = params_setts
      )
      
      updatePickerInput(
        session,
        inputId = "nca_params",
        selected = params_setts
      )

      # Update the units table
      data$units <- dplyr::left_join(data$units, setts$units, 
                                     by = c("ANALYTE", "PPTESTCD"),
                                     suffix = c(".data", ".setts")) %>%
      mutate(across(ends_with(".setts"), ~coalesce(., get(str_replace(cur_column(), ".setts", ".data"))))) %>%
      select(-ends_with(".data"))  %>%
      # Eliminate all suffix from column names
      rename_with(~str_remove(., ".setts"))

      # Update the t0 imputation checkbox
      if (any(grepl("start.*", setts$intervals$impute))) {
        updateCheckboxInput(session, inputId = "should_impute_c0", value = TRUE)
      } else {
        updateCheckboxInput(session, inputId = "should_impute_c0", value = FALSE)
      }

      # Update when present manual intervals defined
      dose_time_col <- setts$dose$columns$time
      intervals_userinput_setts <- setts$intervals %>%
        ungroup() %>%
        filter(type_interval == "manual")  %>%
        left_join(setts$dose$data, by = c(unlist(unname(setts$dose$columns$groups)),"DOSNO")) %>%
        mutate(start = start - !!sym(dose_time_col),
               end = end - !!sym(dose_time_col)) %>%
        select(start, end) %>%
        filter(!duplicated(paste0(start, end)))

      if ((nrow(intervals_userinput_setts) > 0)) {

        updateCheckboxInput(session, inputId = "AUCoptions",
                            label = "Select Partial AUC", value = TRUE)

        for (i in seq_along(nrow(intervals_userinput_setts))) {

          if (auc_counter() >= i){
            removeUI(selector = paste0("#", ns(paste0("AUC_", i))))
          }

          insertUI(
            selector = paste0("#", ns("AUCInputs")),
            where = "beforeEnd",
            ui = partial_auc_input(
              id = paste0("AUC_", auc_counter()),
              ns = ns,
              min_sel_value = as.numeric(intervals_userinput_setts$start[i]),
              max_sel_value = as.numeric(intervals_userinput_setts$end[i])
            )
          )
        }
        # Update the auc_counter reactive variable
        auc_counter(nrow(intervals_userinput_setts))
      }

      # Update the flag rules
      if (!is.na(setts$flag_rules$adj.r.squared)) {
        updateCheckboxInput(session, inputId = "rule_adj.r.squared", value = TRUE)
        updateNumericInput(session = session, inputId = "adj.r.squared_threshold", value = setts$flag_rules$adj.r.squared)
      } else {
        updateCheckboxInput(session, inputId = "rule_adj.r.squared", value = FALSE)
      }

      if (!is.null(setts$options$flag_rules$aucpext.obs)) {
        updateCheckboxInput(session, inputId = "rule_aucpext.obs", value = TRUE)
        updateNumericInput(session, inputId = "aucpext.obs_threshold", value = setts$lag_rules$aucpext.obs)
      } else {
        updateCheckboxInput(session, inputId = "rule_aucpext.obs", value = FALSE)
      }

      if (!is.null(setts$options$flag_rules$aucpext.pred)) {
        updateCheckboxInput(session, inputId = "rule_aucpext.pred", value = TRUE)
        updateNumericInput(session, inputId = "aucpext.pred_threshold", value = setts$flag_rules$aucpext.pred)
      } else {
        updateCheckboxInput(session, inputId = "rule_aucpext.pred", value = FALSE)
      }

      if (!is.null(setts$options$flag_rules$span.ratio)) {
        updateCheckboxInput(session, inputId = "rule_span.ratio", value = TRUE)
        updateNumericInput(session, inputId = "span.ratio_threshold", value = setts$flag_rules$span.ratio)
      } else {
        updateCheckboxInput(session, inputId = "rule_span.ratio", value = FALSE)
      }

      # Update the exclusions and inclusions (when matching with data)
      # ToDo: Make corrections after #177 merged
      conc_setts_columns <- unname(unlist(setts$conc$columns[c("groups", "time", "concentration")]))

      if (nrow(setts$conc$data) > 0) {
        data$conc$data <- dplyr::left_join(data$conc$data,
                           setts$conc$data  %>% select(any_of(c(conc_setts_columns, "is.included.hl", "is.excluded.hl", "exclude_half.life"))), 
                           by = conc_setts_columns,
                           suffix = c(".data", ".setts")) %>%
        mutate(across(ends_with(".setts"), ~coalesce(., get(str_replace(cur_column(), ".setts", ".data"))))) %>%
        select(-ends_with(".data"))  %>%
        rename_with(~str_remove(., ".setts"))
  
        mismatched_rows <- setts$conc$data %>%
          anti_join(data$conc$data, by = conc_setts_columns)
        
        if (nrow(mismatched_rows) > 0) {
          showModal(modalDialog(
            title = "Mismatched Data Points",
            tags$h4("The following data points in the settings file do not match the uploaded dataset.
                    The slopes for these profiles will be reset to best slope:"),
            DTOutput(ns("mismatched_table")),
            easyClose = TRUE,
            footer = NULL
          ))
  
          output$mismatched_table <- DT::renderDT({
            datatable(mismatched_points %>% select(-IX))
          })
        }
      }
    })

    # Include keyboard limits for the settings GUI display

    # Define a function that simplifies the action
    input_limit <- function(input_id, max = Inf, min = -Inf, update_fun = updateNumericInput) {
      observeEvent(input[[input_id]], {
        if (input[[input_id]] < min & !is.na(input[[input_id]])) {
          update_fun(session, input_id, "", value = min)
        }
        if (input[[input_id]] > max & !is.na(input[[input_id]])) {
          update_fun(session, input_id, "", value = max)
        }
      })
    }

    # Keyboard limits for the setting thresholds
    input_limit("adj.r.squared_threshold", max = 1, min = 0)
    input_limit("aucpext.obs_threshold", max = 100, min = 0)
    input_limit("aucpext.pred_threshold", max = 100, min = 0)
    input_limit("span.ratio_threshold", min = 0)

    # Keyboard limits for the dynamically created partial AUC ranges
    observe({
      inputs_list <- reactiveValuesToList(input)
      for (input_id in names(inputs_list)) {
        if (startsWith(input_id, "timeInput")) {
          local({
            my_input_id <- input_id
            observeEvent(input[[my_input_id]], {
              if (is.numeric(input[[my_input_id]]) && input[[my_input_id]] < 0) {
                input_limit(my_input_id, min = 0)
              }
            })
          })
        }
      }
    })


    # Choose dosenumbers to be analyzed
    observeEvent(data(), priority = -1, {
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

    # Handling AUC Intervals
    intervals_userinput <- reactiveVal(NULL)
    auc_counter <- reactiveVal(0)
    observeEvent(input$addAUC, {
      auc_counter(auc_counter() + 1)
      id <- paste0("AUC_", auc_counter())
      insertUI(selector = paste0("#", ns("AUCInputs")),
               where = "beforeEnd", ui = partial_auc_input(id, ns = ns))
      print(sort(names(input)))
    })

    observeEvent(input$removeAUC, {
      if (auc_counter() > 0) {
        removeUI(selector = paste0("#", paste0("AUC_", auc_counter())))
        auc_counter(auc_counter() - 1)
      }
    })

    observe({
      # Collect all inputs for the AUC intervals
      input_names_aucmin <- grep("^timeInputMin_", names(input), value = TRUE)
      input_names_aucmax <- grep("^timeInputMax_", names(input), value = TRUE)

      starts <- unlist(lapply(input_names_aucmin, \(name) input[[name]]))
      ends <- unlist(lapply(input_names_aucmax, \(name) input[[name]]))

      # Make a list of dataframes with each of the intervals requested
      intervals_list <- lapply(seq_along(starts), function(i) {
        mydata()$intervals %>%
          mutate(
            start = start + as.numeric(starts[i]),
            end = start + as.numeric(ends[i])
          ) %>%
          # only TRUE for columns specified in params
          mutate(across(where(is.logical), ~FALSE)) %>%
          # Intervals will always only compute AUC values
          mutate(across(c("aucint.last", "aucint.inf.obs",
                          "aucint.inf.pred", "aucint.all"), ~TRUE)) %>%
          # Identify the intervals as the manual ones created by the user
          mutate(type_interval = "manual")
      })

      # Make sure NAs were not left by the user
      intervals_list <- intervals_list[!is.na(starts) & !is.na(ends)]
      print(intervals_list)

      intervals_userinput(intervals_list)
    })


    # Updating Checkbox and Numeric Inputs
    observeEvent(list(input$rule_adj.r.squared, input$rule_aucpext.obs,
                      input$rule_aucpext.pred, input$AUCoptions, input$nca_params), {

                   nca_params <- input$nca_params
                   if (input$rule_adj.r.squared) nca_params <- c(nca_params, "adj.r.squared")
                   if (input$rule_aucpext.obs) nca_params <- c(nca_params, "aucpext.obs")
                   if (input$rule_aucpext.pred) nca_params <- c(nca_params, "aucpext.pred")
                   if (input$AUCoptions) nca_params <- c(nca_params,
                                                         "aucint.last",
                                                         "aucint.inf.obs",
                                                         "aucint.inf.pred",
                                                         "aucint.all")

                   updatePickerInput(session = session,
                                     inputId = ns("nca_params"),
                                     selected = nca_params)
                 })




    # NCA settings dynamic changes
    observeEvent(list(
      input$method, input$nca_params, input$should_impute_c0,
      input$select_analyte, input$select_dosno, input$select_pcspec,
      intervals_userinput()
    ), {

      # Load mydata reactive and modify it accordingly to user's request
      req(mydata())
      mydata <- mydata()

      mydata$options <- list(
        auc.method = input$method,
        allow.tmax.in.half.life = TRUE,
        keep_interval_cols = c("DOSNO", "type_interval"),
        # Make sure the standard options do not prohibit results
        min.hl.r.squared = 0.001,
        min.span.ratio = Inf,
        min.hl.points = 3
      )

      # Include main intervals as specified by the user
      mydata$intervals <- format_pkncadata_intervals(pknca_dose = mydata$dose,
                                                     # Compute only parameters specified
                                                     params = input$nca_params,
                                                     # Start at t0 when requested by the user
                                                     start_from_last_dose = input$should_impute_c0)

      # Join custom AUC partial intervals specified by the user
      mydata$intervals <- bind_rows(mydata$intervals, intervals_userinput()) %>%
        unique()
      mydata$impute <- NA

      # Filter only the analytes and doses requested
      mydata$intervals <- mydata$intervals %>%
        dplyr::filter(DOSNO %in% input$select_dosno,
                      ANALYTE %in% input$select_analyte,
                      PCSPEC %in% input$select_pcspec)

      # Define start imputations on intervals if specified by the user
      if (input$should_impute_c0) {
        mydata <- create_start_impute(mydata = mydata)
        mydata$impute <- "impute"
      }

      mydata(mydata)
    })

    # Parameter unit changes option: Opens a modal message with a units table to edit
    # It updates $units table of mydata & res_nca when the user saves their changes
    units_table_server("units_table_preNCA", mydata, res_nca)
    units_table_server("units_table_postNCA", mydata, res_nca)

    # Rendering Reactable Output
    output$nca_intervals <- renderReactable({
      req(mydata())

      data <- mydata()$intervals %>%
        apply_labels(LABELS, "ADPC") %>%
        select(where(~!is.logical(.) | any(. == TRUE)))

      data <- data %>%
        left_join(y = mydata()$dose$data) %>%
        group_by(across(all_of(unname(unlist(mydata()$conc$columns$groups))))) %>%
        arrange(!!!syms(unname(unlist(mydata()$conc$columns$groups))), TIME) %>%
        mutate(start = start - first(TIME), end = end - first(TIME)) %>%
        select(!!!syms(colnames(data)))

      reactable(
        data,
        columns = generate_col_defs(data),
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        bordered = TRUE,
        height = "60vh"
      )
    })
  })
}