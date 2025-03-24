nca_setup_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Local upload option
    fluidRow(
      upload_settings_ui(ns("upload_settings"))
    ),

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
    h4("Flag Rule Sets"),
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
#' - mydata A reactive expression of the PKNCAdata object, which contains data and NCA specications.
#'
nca_setup_server <- function(id, data, mydata) { # nolint : TODO: complexity / needs further modularization

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    conc_data <- reactive(mydata()$conc$data)

    # File Upload Handling
    # The server handles the logic for loading project settings from a file & update UI elements
    # from NCA setup (all inputs) & slope adjustments (manual_slopes table & concentration dataset)
    upload_settings_server("upload_settings", mydata, session, auc_counter, manual_slopes)

    # Include keyboard limits for the settings GUI display

    # Keyboard limits for the setting thresholds
    limit_input_value(input, session, "adj.r.squared_threshold", max = 1, min = 0, lab = "R.SQ.ADJ")
    limit_input_value(input, session, "aucpext.obs_threshold", max = 100, min = 0, lab = "AUCPEO")
    limit_input_value(input, session, "aucpext.pred_threshold", max = 100, min = 0, lab = "AUCPEP")
    limit_input_value(input, session, "span.ratio_threshold", min = 0, lab = "SPAN")

    # Keyboard limits for the dynamically created partial AUC ranges
    observeEvent(auc_counter(), {
      for (i in auc_counter()) {
        limit_input_value(input, session, paste0("timeInputMin_AUC_", i), min = 0, lab = "AUC")
        limit_input_value(input, session, paste0("timeInputMax_AUC_", i), min = 0, lab = "AUC")
      }
    })

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

    auc_counter <- reactiveVal(0)
    observeEvent(input$addAUC, {
      auc_counter(auc_counter() + 1)
      id <- paste0("AUC_", auc_counter())
      insertUI(selector = paste0("#", ns("AUCInputs")),
               where = "beforeEnd", ui = partial_auc_input(id, ns = ns))
    })

    observeEvent(input$removeAUC, {
      if (auc_counter() > 0) {
        removeUI(selector = paste0("#", paste0("AUC_", auc_counter())))
        auc_counter(auc_counter() - 1)
      }
    })
    # nolint start
    #' TODO(mateusz): I am disabling  this part of the code, as listening on `names(input)`
    #' causes a great deal of issues with the current reactivity schema. This should be fixed
    #' in the scope of #249
    #' https://github.com/pharmaverse/aNCA/issues/249
    # intervals_userinput <- reactive({
    #   # Collect all inputs for the AUC intervals
    #   input_names_aucmin <- grep("^timeInputMin_", names(input), value = TRUE)
    #   input_names_aucmax <- grep("^timeInputMax_", names(input), value = TRUE)

    #   starts <- unlist(lapply(input_names_aucmin, \(name) input[[name]]))
    #   ends <- unlist(lapply(input_names_aucmax, \(name) input[[name]]))

    #   # Make a list of dataframes with each of the intervals requested
    #   intervals_list <- lapply(seq_along(starts), function(i) {
    #     mydata()$intervals %>%
    #       mutate(
    #         start = start + as.numeric(starts[i]),
    #         end = start + as.numeric(ends[i])
    #       ) %>%
    #       # only TRUE for columns specified in params
    #       mutate(across(where(is.logical), ~FALSE)) %>%
    #       # Intervals will always only compute AUC values
    #       mutate(across(c("aucint.last"), ~TRUE)) %>%
    #       # Identify the intervals as the manual ones created by the user
    #       mutate(type_interval = "manual")
    #   })

    #   # Make sure NAs were not left by the user
    #   intervals_list <- intervals_list[!is.na(starts) & !is.na(ends)]
    #   intervals_list
    # })

    intervals_userinput <- reactive(list())

    # nolint end

    # Updating Checkbox and Numeric Inputs
    observeEvent(list(input$rule_adj_r_squared, input$rule_aucpext_obs,
                      input$rule_aucpext_pred, input$AUCoptions, input$nca_params), {

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
      intervals_userinput(), mydata(), session$userData$units_table()
    ), {
      req(mydata())
      req(
        input$method, input$nca_params, input$select_analyte,
        input$select_dosno, input$select_pcspec, intervals_userinput()
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
      processed_pknca_data$intervals <- format_pkncadata_intervals(
        pknca_conc = processed_pknca_data$conc,
        pknca_dose = processed_pknca_data$dose,
        # Compute only parameters specified
        params = input$nca_params,
        # Start at t0 when requested by the user
        start_from_last_dose = input$should_impute_c0
      )

      # Join custom AUC partial intervals specified by the user
      processed_pknca_data$intervals <- bind_rows(
        processed_pknca_data$intervals,
        intervals_userinput()
      ) %>%
        unique()
      processed_pknca_data$impute <- NA

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
        bordered = TRUE
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

# Handling AUC Intervals

# TODO: Modularise all 'Handling AUC Intervals' and its related actions

#' Define the UI function for a single partial AUC input
#'
#' @param id A unique identifier for the input.
#' @param ns The namespace function.
#' @param min_sel_value The default minimum value for the input.
#' @param max_sel_value The default maximum value for the input.
#'
#' @return A Shiny UI component for creating a partial AUC input.
#'
partial_auc_input <- function(id, ns, min_sel_value = 0, max_sel_value = NULL) {
  fluidRow(
    id = id,
    column(
      width = 6, numericInput(ns(paste0("timeInputMin_", id)),
                              "Min:", min = 0, value = min_sel_value)
    ),
    column(
      width = 6, numericInput(ns(paste0("timeInputMax_", id)),
                              "Max:", min = 0, value = max_sel_value)
    )
  )
}
