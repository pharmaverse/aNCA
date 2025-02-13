# SERVER LOGIC OF NAVBAR NCA TAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This tab is the heart of the application. In this navbar tab we can setup, run and view
# results of the NCA. In the setup tabset we select the anaylte to be processed,
# setup the NCA setting and can do manual slope selection or point exclusion. In the result tabset
# we can view the NCA results, slope caclulation und exclusions table.
# TABSET: Setup ================================================================

# In the setup tabset we select the anaylte to be processed,
# setup the NCA setting and can do manual slope selection or point exclusion.

# TAB: Data Selection ----------------------------------------------------------

# In this tab we select the analyte to be analyzed or can upload settings
# Update analyte selection input based on the data

observeEvent(data(), {
  updateSelectInput(
    session,
    inputId = "select_analyte",
    label = "Choose the Analyte(s) :",
    choices = unique(data()$ANALYTE),
    selected = unique(data()$ANALYTE)[1]
  )
})

observeEvent(data(), {
  updateSelectInput(
    session,
    inputId = "select_pcspec",
    label = "Choose the Specimen Type(s) :",
    choices = unique(data()$PCSPEC),
    selected = unique(data()$PCSPEC)[1]
  )
})

# Make GUI change when new settings are uploaded
observeEvent(input$settings_upload, {

  setts <- read.csv(input$settings_upload$datapath, na = c("", "NA"))
  # Set the basic settings
  analyte <- setts$ANALYTE[1]
  doses_selected <- as.numeric(strsplit(as.character(setts$doses_selected), split = ",")[[1]])

  # Check that match with the data currently loaded
  if (!analyte %in% unique(data()$ANALYTE) ||
        !all(doses_selected %in% unique(data()$DOSNO))) {

    showNotification(
      validate("The analyte selected in the settings file is not present in the data. Please, if
                you want to use this settings for a different file, make sure all meaningful
                variables in the file are in the data (ANALYTE, DOSNO...)"),
      type = "error"
    )
  }

  # Compare the dataset with settings for inclusions and exclusions
  new_data <- data() %>%
    filter(
      ANALYTE == analyte,
      if ("EVID" %in% names(data())) EVID == 0 else TRUE
    ) %>%
    mutate(groups = paste0(USUBJID, ", ", DOSNO)) %>%
    filter(TIME >= 0) %>%
    arrange(STUDYID, USUBJID, PCSPEC, DOSNO, TIME) %>%
    group_by(STUDYID, USUBJID, PCSPEC, DOSNO) %>%
    mutate(IX = seq_len(n())) %>% # Assuming data() returns the newly uploaded dataset
    select(STUDYID, USUBJID, AVAL, DOSNO, TIME, IX)

  setts_lambda <- setts %>%
    select(STUDYID, USUBJID, DOSNO, IX, AVAL, TIME) %>%
    na.omit()

  # Identify mismatched data points
  mismatched_points <- setts_lambda %>%
    anti_join(new_data, by = c("USUBJID", "DOSNO", "IX", "AVAL", "TIME"))

  if (nrow(mismatched_points) > 0) {
    showModal(modalDialog(
      title = "Mismatched Data Points",
      tags$h4("The following data points in the settings file do not match the uploaded dataset.
              The slopes for these profiles will be reset to best slope:"),
      DTOutput("mismatched_table"),
      easyClose = TRUE,
      footer = NULL
    ))

    output$mismatched_table <- DT::renderDT({
      datatable(mismatched_points %>%
                  select(-IX))
    })

    setts <- setts %>%
      anti_join(mismatched_points, by = c("USUBJID", "DOSNO"))
  }

  # Analyte
  updateSelectInput(
    session,
    inputId = "analyte",
    label = "Choose the analyte:",
    choices = data()$ANALYTE[1],
    selected = setts$ANALYTE[1]
  )

  # Dose number
  updateSelectInput(
    session,
    inputId = "select_dosno",
    label = "Choose the Dose Number:",
    choices = sort(unique(data() %>% filter(ANALYTE == setts$ANALYTE[1]) %>% pull(DOSNO))),
    selected = doses_selected
  )

  # Extrapolation Method
  method_choices <- c("Linear Log", "LinearUp LogDown", "Linear LinearInterpolation")
  updateSelectInput(
    session,
    inputId = "method",
    label = "Extrapolation Method:",
    choices = c("lin-log", "lin up/log down", "linear"),
    selected = setts$method
  )

  # AUC intervals
  if (!is.na(setts$auc_mins[1])) {
    updateCheckboxInput(session, inputId = "AUCoptions", label = "Select Partial AUC", value = TRUE)
    auc_mins <- as.character(setts$auc_mins[1])
    auc_maxs <- as.character(setts$auc_maxs[1])
    auc_mins <- strsplit(auc_mins, split = ",")[[1]]
    auc_maxs <- strsplit(auc_maxs, split = ",")[[1]]

    for (i in seq_along(auc_mins)) {
      auc_counter(auc_counter() + 1)
      insertUI(
        selector = "#AUCInputs",
        where = "beforeEnd",
        ui = partial_auc_input(
          id = paste0("AUC_", auc_counter()),
          min_sel_value = as.numeric(auc_mins[i]),
          max_sel_value = as.numeric(auc_maxs[i])
        )
      )
    }
  }

  # RSADJ
  if (!is.na(setts$adj.r.squared_threshold[1])) {
    updateCheckboxInput(session, inputId = "rule_adj_r_squared", label = "RSQADJ:", value = TRUE)
    updateNumericInput(
      session,
      "adj.r.squared_threshold",
      "",
      value = setts$adj.r.squared_threshold[1]
    )
  } else {
    updateCheckboxInput(session, inputId = "rule_adj_r_squared", label = "RSQADJ:", value = FALSE)
  }

  # AUCPE.Obs
  if (!is.na(setts$aucpext.obs_threshold[1])) {
    updateCheckboxInput(session, inputId = "rule_aucpext_obs", value = TRUE)
    updateNumericInput(session, "aucpext.obs_threshold", value = setts$aucpext.obs_threshold[1])
  } else {
    updateCheckboxInput(session, inputId = "rule_aucpext_obs", label = "", value = FALSE)
  }

  # AUCPE.Pred
  if (!is.na(setts$aucpext.pred_threshold[1])) {
    updateCheckboxInput(session, inputId = "rule_aucpext_pred",  value = TRUE)
    updateNumericInput(session, "aucpext.pred_threshold", value = setts$aucpext.pred_threshold[1])
  } else {
    updateCheckboxInput(session, inputId = "rule_aucpext_pred", value = FALSE)
  }

  # SPAN
  if (!is.na(setts$span.ratio_threshold[1])) {
    updateCheckboxInput(session, inputId = "rule_span_ratio", label = "SPAN: ", value = TRUE)
    updateNumericInput(session, "span.ratio_threshold", "", value = setts$span.ratio_threshold[1])
  } else {
    updateCheckboxInput(session, inputId = "rule_span_ratio", label = "SPAN:", value = FALSE)
  }
})

# When an analyte is selected and the user clicks the "Submit" button,
# create the PKNCA data object
mydata <- reactiveVal(NULL)
observeEvent(input$submit_analyte, priority = 2, {

  # Define explicetely input columns until there are input definitions
  group_columns <- intersect(colnames(data()), c("STUDYID", "PCSPEC", "ROUTE", "DRUG"))
  usubjid_column <- "USUBJID"
  time_column <- "AFRLT"
  dosno_column <- "DOSNO"
  route_column <- "ROUTE"
  analyte_column <- "ANALYTE"
  matrix_column <- "PCSPEC"

  # Segregate the data into concentration and dose records
  df_conc <- format_pkncaconc_data(ADNCA = data(),
                                   group_columns = c(group_columns, usubjid_column, analyte_column),
                                   time_column = time_column) %>%
    dplyr::arrange(across(all_of(c(usubjid_column, time_column)))) %>%
    # Consider only the analytes and matrix requested by the user
    dplyr::filter(!!sym(analyte_column) %in% input$select_analyte,
                  !!sym(matrix_column) %in% input$select_pcspec)

  df_dose <- format_pkncadose_data(pkncaconc_data = df_conc,
                                   group_columns = c(group_columns, usubjid_column),
                                   time_column = time_column,
                                   dosno_column = dosno_column,
                                   since_lastdose_time_column = "ARRLT")

  # Define initially a inclusions/exclusions for lambda slope estimation (with no input)
  df_conc$is.excluded.hl <- FALSE
  df_conc$is.included.hl <- FALSE
  df_conc$REASON <- NA  # Exclusions will have preferential reason statements than inclusions
  df_conc$exclude_half.life <- FALSE

  # Make the PKNCA concentration and dose objects
  myconc <- PKNCA::PKNCAconc(
    df_conc,
    formula = AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / ANALYTE,
    exclude_half.life = "exclude_half.life",
    time.nominal = "NFRLT"
  )

  mydose <- PKNCA::PKNCAdose(
    data = df_dose,
    formula = DOSEA ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID,
    route = route_column,
    time.nominal = "NFRLT",
    duration = "ADOSEDUR"
  )

  myintervals <- format_pkncadata_intervals(mydose) %>%
    # Filter only the doses requested by the user
    dplyr::filter(
      !!sym(dosno_column) %in% input$select_dosno
    )

  # Combine the PKNCA objects into the PKNCAdata object
  # TODO think of case with different units for different analytes
  mydata <- PKNCA::PKNCAdata(
    data.conc = myconc,
    data.dose = mydose,
    intervals = myintervals,
    units = PKNCA::pknca_units_table(
      concu = myconc$data$AVALU[1],
      doseu = myconc$data$DOSEU[1],
      amountu = myconc$data$AVALU[1],
      timeu = myconc$data$RRLTU[1]
    )
  )

  # Redefine units for each analyte and for potential customizations
  unique_analytes <- unique(mydata$conc$data[[mydata$conc$columns$groups$group_analyte]])
  analyte_column <- mydata$conc$columns$groups$group_analyte
  mydata$units <- tidyr::crossing(mydata$units,
                                  !!sym(analyte_column) := unique_analytes)  %>%
    dplyr::mutate(PPSTRESU = PPORRESU, conversion_factor = 1)

  mydata(mydata)
})

# Display the PKNCA data object for the user (concentration records)
output$datatable <- renderReactable({
  req(mydata())
  data <- mydata()$conc$data %>%
    filter(DOSNO %in% input$select_dosno,
           ANALYTE %in% input$select_analyte,
           PCSPEC %in% input$select_pcspec)
  # Generate column definitions
  col_defs <- generate_col_defs(data)

  reactable(
    data,
    columns = col_defs,
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

# TAB: Settings ----------------------------------------------------------------

# IN this tab we can set the dose number to be analyzed, the extrapolation
# method, potenital partial AUC and all the flag rule sets

# Define a profiles per patient
profiles_per_patient <- reactive({
  req(mydata())
  # Check if res_nca() is available and valid
  if (!is.null(res_nca())) {
    res_nca()$result %>%
      mutate(USUBJID = as.character(USUBJID),
             DOSNO = as.character(DOSNO)) %>%
      group_by(USUBJID, ANALYTE, PCSPEC) %>%
      summarise(DOSNO = unique(DOSNO), .groups = "drop") %>%
      unnest(DOSNO)  # Convert lists into individual rows
  } else {
    mydata()$conc$data %>%
      mutate(USUBJID = as.character(USUBJID)) %>%
      group_by(USUBJID, ANALYTE, PCSPEC) %>%
      summarise(DOSNO = list(unique(DOSNO)), .groups = "drop")
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

observeEvent(input$select_analyte, priority = -1, {
  req(data())
  doses_options <- data() %>%
    filter(ANALYTE %in% input$select_analyte) %>%
    pull(DOSNO) %>%
    sort() %>%
    unique()

  updateSelectInput(
    session,
    inputId = "select_dosno",
    label = "Choose the Dose Number:",
    choices = doses_options,
    selected = doses_options[1]
  )
})

# Partial AUC Selection
auc_counter <- reactiveVal(0) # Initialize a counter for the number of partial AUC inputs
intervals_userinput <- reactiveVal(NULL)
pk_nca_trigger <- reactiveVal(0)

# Add a new partial AUC input
observeEvent(input$addAUC, {
  auc_counter(auc_counter() + 1)
  id <- paste0("AUC_", auc_counter())
  insertUI(selector = "#AUCInputs", where = "beforeEnd", ui = partial_auc_input(id))
})

# Remove the last partial AUC input
observeEvent(input$removeAUC, {
  if (auc_counter() > 0) {
    removeUI(selector = paste0("#", paste0("AUC_", auc_counter())))
    auc_counter(auc_counter() - 1)
  }
})

# NCA button object
myres <- reactiveVal(NULL)

observeEvent(input$nca, {
  req(mydata())

  # Use the intervals defined by the user if so
  if (input$AUCoptions && auc_counter() > 0) {

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

    # Save intervals as a dataframe
    intervals_userinput(intervals_list)
  }


  # Use the user inputs to determine the NCA settings to apply
  PKNCA::PKNCA.options(
    auc.method = input$method,
    allow.tmax.in.half.life = TRUE,
    keep_interval_cols = c("DOSNO", "type_interval"),
    # Make sure the standard options do not prohibit results
    min.hl.r.squared = 0.001,
    min.span.ratio = Inf,
    min.hl.points = 3
  )

  # Load mydata reactive
  mydata <- mydata()

  # Include manual intervals if specified by the user
  mydata$intervals <- bind_rows(mydata$intervals, intervals_userinput())

  # Define start imputations on intervals if specified by the user
  if (input$should_impute_c0) {
    mydata <- create_start_impute(mydata = mydata)
    mydata$impute <- "impute"

  } else {
    # Otherwise, the original intervals should start at C1 for all calculations
    mydata <- PKNCA::PKNCAdata(
      data.conc = mydata$conc,
      data.dose = mydata$dose,
      intervals = bind_rows(
        format_pkncadata_intervals(
          mydata$dose,
          start_from_last_dose = FALSE
        ),
        intervals_userinput()
      ) %>%
        # Filter only the analytes and doses requested by the user
        dplyr::filter(
          DOSNO %in% input$select_dosno,
          if ("EVID" %in% names(data())) EVID == 0 else TRUE
        ),
      units = PKNCA::pknca_units_table(
        concu = mydata$conc$data$PCSTRESU[1],
        doseu = mydata$dose$data$DOSEU[1],
        amountu = mydata$conc$data$PCSTRESU[1],
        timeu = mydata$conc$data$RRLTU[1]
      )
    )
  }
  mydata(mydata)

  # Perform NCA on the profiles selected
  pk_nca_trigger(pk_nca_trigger() + 1)

  # Update panel to show results page
  updateTabsetPanel(session, "ncapanel", selected = "Results")
})

res_nca <- reactiveVal(NULL)
observeEvent(pk_nca_trigger(), {
  req(mydata())
  withProgress(message = "Calculating NCA...", value = 0, {
    myres <- PKNCA::pk.nca(data = mydata(), verbose = FALSE)

    # Increment progress to 100% after NCA calculations are complete
    incProgress(0.5, detail = "NCA calculations complete!")

    # Make the starts and ends of results relative to last dose using the dose data
    myres$result <- myres$result %>%
      inner_join(select(mydata()$dose$data, -exclude)) %>%
      mutate(start = start - !!sym(mydata()$dose$columns$time),
             end = end - !!sym(mydata()$dose$columns$time)) %>%
      select(names(myres$result))

    # Return the result
    res_nca(myres)
  })
})

# Parameter unit changes option: Opens a modal message with a units table to edit
# It updates $units table of mydata & res_nca when the user saves their changes
units_table_server("units_table_preNCA", mydata, res_nca)
units_table_server("units_table_postNCA", mydata, res_nca)

# TABSET: Results ==============================================================

# In the result tabset we can view the NCA results, slope caclulation und exclusions table.

# TAB: NCA Results -------------------------------------------------------------

# Create a reactive expression to store the reshaped form of the results that
# will be displayed in the UI
final_res_nca <- reactiveVal(NULL)

# creative final_res_nca, aiming to present the results in a more comprehensive way
observeEvent(res_nca(), {
  req(res_nca())
  # Create a reshaped object that will be used to display the results in the UI
  final_res_nca <- pivot_wider_pknca_results(res_nca())

  # Get all inputs which are TRUE and start with 'rule_'
  for (rule_input in grep("^rule_", names(input), value = TRUE)) {
    if (!input[[rule_input]]) next

    pptestcd <- rule_input |>
      gsub("^rule_", "", x = _) |>
      gsub("_", ".", x = _, fixed = TRUE)
    if (startsWith(pptestcd, "auc")) {
      final_res_nca[[paste0("flag_", pptestcd)]] <- {
        final_res_nca[[pptestcd]] >= input[[paste0(pptestcd, "_threshold")]]
      }
    } else {
      final_res_nca[[paste0("flag_", pptestcd)]] <- {
        final_res_nca[[pptestcd]] <= input[[paste0(pptestcd, "_threshold")]]
      }
    }
  }

  # Include units for all column names
  dict_pttestcd_with_units <- res_nca()$result %>%
    select(PPTESTCD, PPSTRESU) %>%
    unique() %>%
    pull(PPSTRESU, PPTESTCD)

  final_res_nca <- final_res_nca %>%
    rename_with(~ifelse(
      gsub("_.*", "", .x) %in% names(dict_pttestcd_with_units),
      paste0(.x, "[", dict_pttestcd_with_units[gsub("_.*", "", .x)], "]"),
      .x
    ))

  # Sort alphabetically all columns but the grouping and the exclude columns
  group_cols <- c(unname(unique(c(unlist(res_nca()$data$conc$columns$groups),
                                  unlist(res_nca()$data$dose$columns$groups)))))
  int_cols <- c("DOSNO", "start", "end")
  exclude_cols <- names(final_res_nca)[startsWith(names(final_res_nca), "exclude.")]
  param_cols <- names(final_res_nca)[endsWith(names(final_res_nca), "]")]
  other_cols <- setdiff(names(final_res_nca), c(group_cols, int_cols, exclude_cols, param_cols))
  final_res_nca <- final_res_nca %>%
    dplyr::select(any_of(c(group_cols, int_cols, param_cols, exclude_cols, other_cols)))

  # Create a reshaped object
  final_res_nca(
    final_res_nca %>%
      # Create a flag_ column when input$rule_r2adj is TRUE, and make it TRUE when the only column
      # that starts with r2adj is below the threshold input$r2adj_threshold
      mutate(flag_onlyFalse = FALSE)  %>%
      # Create a flagged column that informs if any of the variables starting as 'flag_' are TRUE
      # Only if there is at least one column that stats with flag_
      mutate(
        flagged = case_when(
          rowSums(is.na(select(., starts_with("flag_")))) > 0 ~ "MISSING",
          rowSums(select(., starts_with("flag_")), na.rm = TRUE) > 0 ~ "FLAGGED",
          TRUE ~ "ACCEPTED"
        )
      )
  )

  # Allow user to choose the parameters to display of final_res_nca
  updatePickerInput(
    session = session,
    inputId = "params",
    label = "Select Parameters :",
    choices = sort(colnames(final_res_nca())),
    selected = sort(colnames(final_res_nca()))
  )
})

# Render the reshaped results as a DT datatable
output$myresults <- DT::renderDataTable({
  req(final_res_nca())
  DT::datatable(
    data = final_res_nca(),
    extensions = "FixedHeader",
    options = list(
      scrollX = TRUE,
      scrollY = TRUE,
      lengthMenu = list(c(10, 25, -1), c("10", "25", "All")),
      fixedHeader = TRUE,
      columnDefs = list(list(
        visible = FALSE, targets = setdiff(colnames(final_res_nca()), input$params)
      ))
    )
  ) %>%
    formatStyle(
      "flagged",
      target = "row",
      backgroundColor = styleEqual(c("FLAGGED", "MISSING"), c("#f5b4b4", "#cbaddd"))
    )
})


# Download final results
observeEvent(input$download, {
  showModal(modalDialog(
    title = "Please enter the path to the folder on Improve for your results:",
    textInput("pathresults", "Path:"),
    actionButton("go", "GO"),
    footer = modalButton("Close")
  ))
})

## Add download locally option

output$local_download_NCAres <- downloadHandler(
  filename = function() {
    paste0(mydata()$conc$data$STUDYID[1], "PK_Parameters.csv")
  },
  content = function(file) {
    old_wd <- getwd()  # save old working directory
    tempdir <- tempdir()  # create a temporary directory
    setwd(tempdir)  # change working directory to temporary directory

    write.csv(final_res_nca(), file, row.names = FALSE)

    setwd(old_wd)  # change working directory back to original
  }
)

# Save the project settings
output$settings_save <- downloadHandler(
  filename = function() {
    paste(mydata()$conc$data$STUDYID[1], "NCA_settings.csv", sep = "_")
  },
  content = function(file) {

    # Get the data settings from the NCA results (data run)
    myconc <- res_nca()$data$conc

    # Create a settings file that the user can download/upload
    #for establishing the same configuration
    setts_lambda <- myconc$data %>%
      # Identify the points that the user has manually selected for the half-life calculation
      mutate(
        TYPE = case_when(is.excluded.hl ~ "Exclusion", is.included.hl ~ "Selection", TRUE ~ NA)
      ) %>%
      filter(is.excluded.hl | is.included.hl)  %>%
      select(any_of(c(
        unname(unlist(myconc$columns$groups)),
        "IX",
        myconc$columns$time,
        myconc$columns$concentration,
        "TYPE",
        "REASON"
      )))

    # Make sure that there is at least one row so the settings can be considered
    if (nrow(setts_lambda) == 0) {
      setts_lambda <- setts_lambda %>%
        add_row()
    }

    # Consider the intervals defined by the user for the AUC calculation
    input_names_aucmin <- grep("^timeInputMin_", names(input), value = TRUE)
    input_names_aucmax <- grep("^timeInputMax_", names(input), value = TRUE)
    auc_mins <- unlist(lapply(input_names_aucmin, function(name) input[[name]]))
    auc_maxs <- unlist(lapply(input_names_aucmax, function(name) input[[name]]))

    # Include the rule settings as additional columns
    setts <- setts_lambda %>%
      mutate(
        ANALYTE %in% input$select_analyte,
        doses_selected = ifelse(
          !is.null(input$select_dosno),
          paste0(input$select_dosno, collapse = ","),
          unique(mydata()$conc$data$DOSNO)
        ),
        method = input$method,
        adj.r.squared_threshold = ifelse(
          input$rule_adj_r_squared, input$adj.r.squared_threshold, NA
        ),
        aucpext.obs_threshold = ifelse(
          input$rule_aucpext_obs, input$aucpext.obs_threshold, NA
        ),
        aucpext.pred_threshold = ifelse(
          input$rule_aucpext_pred, input$aucpext.pred_threshold, NA
        ),
        span.ratio_threshold = ifelse(
          input$rule_span_ratio, input$span.ratio_threshold, NA
        ),
        auc_mins = if (is.null(auc_mins)) NA else paste(auc_mins, collapse = ","),
        auc_maxs = if (is.null(auc_maxs)) NA else paste(auc_maxs, collapse = ",")
      )

    write.csv(setts, file, row.names = FALSE)
  },
  contentType = "text/csv"
)

# Keep the UI table constantly actively updated
observe({
  for (input_name in grep(
    "(TYPE|PATIENT|PROFILE|IXrange|REASON)_Ex\\d+$", names((input)), value = TRUE
  )) {
    observeEvent(input[[input_name]], {
      # Get the ID of the exclusion
      id <- gsub("_(Ex\\d+)$", "", input_name)

      # Update the reactive list of exclusion IDs
      manual_slopes <- manual_slopes()
      set_selected_value(
        manual_slopes[manual_slopes$id == id, ], paste0(input[[input_name]])
      ) <- manual_slopes[manual_slopes$id == id, ]
      manual_slopes(manual_slopes)

    })
  }
})

# TAB: Slopes ------------------------------------------------------------------

# Slope settings
output$preslopesettings <- DT::renderDataTable({
  # Reshape results and only choose the columns that are relevant to half life calculation
  preslopesettings <- pivot_wider_pknca_results(res_nca())  %>%
    select(
      any_of(c("USUBJID", "DOSNO", "ANALYTE", "PCSPEC")),
      starts_with("lambda.z"),
      starts_with("span.ratio"),
      starts_with("half.life"),
      "exclude.lambda.z"
    )

  # Render the DT datatable object
  DT::datatable(
    data = preslopesettings,
    extensions = "FixedHeader",
    options = list(
      scrollX = TRUE,
      scrollY = TRUE,
      lengthMenu = list(c(10, 25, -1), c("10", "25", "All")),
      pageLength = -1,
      fixedHeader = TRUE
    )
  ) %>%
    formatStyle(
      "exclude.lambda.z",
      target = "row",
      backgroundColor = styleEqual(NA, NA, default = "#f5b4b4")
    )
})

# Slope selector module #
slope_rules <- slope_selector_server(
  "slope_selector",
  mydata,
  res_nca,
  profiles_per_patient,
  input$select_dosno,
  input$select_analyte,
  input$select_pcspec,
  pk_nca_trigger,
  reactive(input$settings_upload)
)

#' display rules on Results -> rules tab
output$manual_slopes2 <- renderTable({
  slope_rules()
})

# CDISC ------------------------------------------------------------------------

# Parameter datasets ------------------------------------------------------------------------

# Create CDISC parameter datasets (PP, ADPP)
parameter_datasets_server("parameter_datasets", res_nca)
