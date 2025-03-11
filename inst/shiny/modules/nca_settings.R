nca_settings_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Local upload option
    load_settings_ui(ns("load_settings")),
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
    load_settings_server("load_settings", mydata)

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