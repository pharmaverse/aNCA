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
    label = "Choose the analyte :",
    choices = unique(data()$ANALYTE)
  )
})

# Make GUI change when new settings are uploaded
observeEvent(input$settings_upload, {
  setts <- read.csv(input$settings_upload$datapath, na = c("", "NA"))
  # Set the basic settings
  analyte <- setts$ANALYTE[1]
  doses_selected <- as.numeric(strsplit(as.character(setts$doses_selected), split = ",")[[1]])

  # Check that match with the data currently loaded
  if (!setts$ANALYTE[1] %in% unique(data()$ANALYTE) ||
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
  if (!is.na(setts$AUC_mins[1])) {
    updateCheckboxInput(session, inputId = "AUCoptions", label = "Select Partial AUC", value = TRUE)
    auc_mins <- as.character(setts$AUC_mins[1])
    auc_maxs <- as.character(setts$AUC_maxs[1])
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
    updateCheckboxInput(session, inputId = "rule_adj.r.squared", label = "RSQADJ:", value = TRUE)
    updateNumericInput(
      session,
      "adj.r.squared_threshold",
      "",
      value = setts$adj.r.squared_threshold[1]
    )
  } else {
    updateCheckboxInput(session, inputId = "rule_adj.r.squared", label = "RSQADJ:", value = FALSE)
  }

  # AUCPE.Obs
  if (!is.na(setts$aucpext.obs_threshold[1])) {
    updateCheckboxInput(session, inputId = "rule_aucpext.obs", value = TRUE)
    updateNumericInput(session, "aucpext.obs_threshold", value = setts$aucpext.obs_threshold[1])
  } else {
    updateCheckboxInput(session, inputId = "rule_aucpext.obs", label = "", value = FALSE)
  }

  # AUCPE.Pred
  if (!is.na(setts$aucpext.pred_threshold[1])) {
    updateCheckboxInput(session, inputId = "rule_aucpext.pred",  value = TRUE)
    updateNumericInput(session, "aucpext.pred_threshold", value = setts$aucpext.pred_threshold[1])
  } else {
    updateCheckboxInput(session, inputId = "rule_aucpext.pred", value = FALSE)
  }

  # SPAN
  if (!is.na(setts$span.ratio_threshold[1])) {
    updateCheckboxInput(session, inputId = "rule_span.ratio", label = "SPAN: ", value = TRUE)
    updateNumericInput(session, "span.ratio_threshold", "", value = setts$span.ratio_threshold[1])
  } else {
    updateCheckboxInput(session, inputId = "rule_span.ratio", label = "SPAN:", value = FALSE)
  }

  # Lambda slope point selections and exclusions
  slope_manual_nca_data <- setts %>%
    select(TYPE, USUBJID, DOSNO, IX, REASON) %>%
    mutate(PATIENT = as.character(USUBJID), PROFILE = as.character(DOSNO)) %>%
    group_by(TYPE, PATIENT, PROFILE, REASON) %>%
    summarise(IXrange = paste0(min(IX), ":", max(IX))) %>%
    select(TYPE, PATIENT, PROFILE, IXrange, REASON) %>%
    na.omit()

  slope_manual_nca_data(slope_manual_nca_data)
})

# When an analyte is selected and the user clicks the "Submit" button,
# create the PKNCA data object
mydata <- reactiveVal(NULL)
observeEvent(input$submit_analyte, priority = 2, {

  # Define explicetely input columns until there are input definitions
  group_columns <- intersect(colnames(data()), c("STUDYID", "PCSPEC", "DOSNO", "ROUTE", "DRUG"))
  usubjid_column <- "USUBJID"
  time_column <- "AFRLT"
  route_column <- "ROUTE"
  analyte_column <- "ANALYTE"
  
  # Filter data based on selected analyte and dose numbers
  data_filt <- data() %>% 
    dplyr::filter(!!sym(analyte_column) == input$select_analyte,
                  DOSNO %in% input$select_dosno,
                  if ("EVID" %in% names(data())) EVID == 0 else TRUE)
  
  # Segregate the data into concentration and dose records
  df_conc <- create_conc(ADNCA = data_filt, 
                         group_columns = c(group_columns, usubjid_column, analyte_column),
                         time_column = time_column)

  df_dose <- create_dose(df_conc = df_conc, 
                         group_columns = c(group_columns, usubjid_column),
                         time_column = time_column, 
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
    formula = DOSEA ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID + DOSNO,
    route = route_column,
    time.nominal = "NFRLT",
    duration = "ADOSEDUR"
  )

  # Combine the PKNCA objects into the PKNCAdata object
  # TODO think of case with different units for different analytes
  mydata <- PKNCA::PKNCAdata(
    data.conc = myconc,
    data.dose = mydose,
    units = PKNCA::pknca_units_table(
      concu = myconc$data$PCSTRESU[1],
      doseu = myconc$data$DOSEU[1],
      amountu = myconc$data$PCSTRESU[1],
      timeu = myconc$data$RRLTU[1]
    )
  )

  # Keep only the default intervals to infinity
  mydata$intervals <- mydata$intervals %>%
    dplyr::filter(end == Inf) %>%
    dplyr::mutate(auclast = TRUE)
  
  mydata(mydata)
})

# Display the PKNCA data object for the user (concentration records)
output$datatable <- DT::renderDataTable({
  req(mydata())
  DT::datatable(
    data = mydata()$conc$data,
    extensions = "FixedHeader",
    options = list(
      scrollX = TRUE,
      scrollY = TRUE,
      lengthMenu = list(c(10, 25, -1), c("10", "25", "All")),
      fixedHeader = TRUE
    )
  )
})

# TAB: Settings ----------------------------------------------------------------

# IN this tab we can set the dose number to be analyzed, the extrapolation
# method, potenital partial AUC and all the flag rule sets

# Define the profiles (dosno) associated with each patient (usubjid) for the selected analyte
profiles_per_patient <- reactiveVal(NULL)
observeEvent(mydata(), {
  profiles_per_patient(tapply(mydata()$conc$data$DOSNO, mydata()$conc$data$USUBJID, unique))
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
  doses_options <- data() %>% 
    filter(ANALYTE == input$select_analyte) %>% 
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

# Create a reactive values object
rv <- reactiveValues(trigger = 0)
# Update the trigger whenever either button is clicked
observeEvent(input$nca, {
  req(mydata())
  
  # Use the intervals defined by the user if so
  if (input$AUCoptions && auc_counter() > 0) {

    # Collect all inputs for the AUC intervals
    input_names_aucmin <- grep("^timeInputMin_", names(input), value = TRUE)
    input_names_aucmax <- grep("^timeInputMax_", names(input), value = TRUE)
    
    starts <- unlist(lapply(input_names_aucmin, function(name) input[[name]]))
    ends <- unlist(lapply(input_names_aucmax, function(name) input[[name]]))

    # Make a list of dataframes with each of the intervals requested
    intervals_list <- lapply(1:length(starts), function(i){
      mydata()$intervals %>% 
        dplyr::mutate(
          start = start + as.numeric(starts[i]),
          end = start + as.numeric(ends[i])
        ) %>% 
        # only TRUE for columns specified in params
        mutate(across(where(is.logical), ~FALSE)) %>%
        # Intervals will always only compute AUC values
        mutate(across(c("aucint.last", "aucint.inf.obs", "aucint.inf.pred", "aucint.all"), ~TRUE))
    })

    # Save intervals as a dataframe
    intervals_userinput(intervals_list)
  }

  # Make the user aware if it forgot to select at least 1 DOSNO
  if (is.null(input$select_dosno)) {
    showNotification(
      "Please select a profile from the 'Settings' tab to proceed.",
      duration = NULL,
      closeButton = TRUE,
      type = "warning"
    )
  } else {
    # Lead the user to the Results section
    rv$trigger <- rv$trigger + 1
    updateTabsetPanel(session, "ncapanel", selected = "Results")
  }
  
  # Update profiles per patient considering the profiles selected
  mydataconc_new <- mydata()$conc$data %>% filter(DOSNO %in% input$select_dosno)
  profiles_per_patient(tapply(mydataconc_new$DOSNO, mydataconc_new$USUBJID, unique))
})

# run the nca upon button click

res_nca <- eventReactive(rv$trigger, {
  req(!is.null(input$select_dosno))

  withProgress(message = "Calculating NCA...", value = 0, {
    req(mydata())

    # Increment progress to 50% after getting dataNCA
    incProgress(0.5, detail = "Performing NCA calculations...")

    # Use the user inputs to determine the NCA settings to apply
    PKNCA::PKNCA.options(
      auc.method = input$method,
      allow.tmax.in.half.life = TRUE,

      # Make sure the standard options do not prohibit results
      min.hl.r.squared = 0.001,
      min.span.ratio = Inf,
      min.hl.points = 3
    )

    # Load mydata reactive
    mydata <- mydata()

    # Include manual intervals if specified by the user
    mydata$intervals <- bind_rows(mydata$intervals, intervals_userinput())
    browser()
    # Define C0 imputations on intervals starting at dose time if specified by the user
    if (input$should_impute_c0) {
      mydata <- create_c0_impute(mydata = mydata)
      mydata$impute <- "impute"

    # Otherwise if at C0 is not already present, make those intervals start at C1 for auclast
    } 
  
    # Perform NCA on the profiles selected
    myres <- PKNCA::pk.nca(data = mydata, verbose = FALSE)

    # Make the starts and ends of results relative to last dose
    myres$result = merge(myres$result, mydata$dose$data) %>% 
      dplyr::mutate(start = start - !!sym(mydata$dose$columns$time),
                    end = end - !!sym(mydata$dose$columns$time)) %>% 
      dplyr::select(names(myres$result))

    # Increment progress to 100% after NCA calculations are complete
    incProgress(0.5, detail = "NCA calculations complete!")

    # Return the result
    return(myres)
  })
})

# TABSET: Results ==============================================================

# In the result tabset we can view the NCA results, slope caclulation und exclusions table.

# TAB: NCA Results -------------------------------------------------------------

# Create a reactive expression to store the reshaped form of the results that
# will be displayed in the UI
final_res_nca <- reactiveVal(NULL)

# creative final_res_nca, aiming to present the results in a more comprehensive way
observeEvent(res_nca(), {

  # Create a reshaped object that will be used to display the results in the UI
  final_res_nca <- reshape_pknca_results(res_nca())

  # Get all inputs which are TRUE and start with 'rule_'
  for (rule_input in grep("^rule_", names(input), value = TRUE)) {
    if (!input[[rule_input]]) next

    pptestcd <- gsub("rule_", "", rule_input)
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
    select(PPTESTCD, PPORRESU) %>%
    unique() %>%
    pull(PPORRESU, PPTESTCD)

  final_res_nca <- final_res_nca %>%
    rename_with(~ifelse(
      gsub("_.*", "", .x) %in% names(dict_pttestcd_with_units),
      paste0(.x, "[", dict_pttestcd_with_units[gsub("_.*", "", .x)], "]"),
      .x
    ))

  # Sort alphabetically all columns but the grouping and the exclude columns
  group_cols <- c(unname(unique(c(unlist(res_nca()$data$conc$columns$groups),
                                  unlist(res_nca()$data$dose$columns$groups))
                                )), "start", "end")
  exclude_cols <- names(final_res_nca)[startsWith(names(final_res_nca), "exclude.")]
  final_res_nca <- final_res_nca[, c(
    group_cols,
    sort(setdiff(names(final_res_nca), c(group_cols, exclude_cols))),
    sort(exclude_cols)
  )]

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
        ANALYTE = input$select_analyte,
        doses_selected = ifelse(
          !is.null(input$select_dosno),
          paste0(input$select_dosno, collapse = ","),
          unique(mydata()$conc$data$DOSNO)
        ),
        method = input$method,
        adj.r.squared_threshold = ifelse(
          input$rule_adj.r.squared, input$adj.r.squared_threshold, NA
        ),
        aucpext.obs_threshold = ifelse(
          input$rule_aucpext.obs, input$aucpext.obs_threshold, NA
        ),
        aucpext.pred_threshold = ifelse(
          input$rule_aucpext.pred, input$aucpext.pred_threshold, NA
        ),
        span.ratio_threshold = ifelse(
          input$rule_span.ratio, input$span.ratio_threshold, NA
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
      slope_manual_nca_data <- slope_manual_nca_data()
      set_selected_value(
        slope_manual_nca_data[slope_manual_nca_data$id == id, ], paste0(input[[input_name]])
      ) <- slope_manual_nca_data[slope_manual_nca_data$id == id, ]
      slope_manual_nca_data(slope_manual_nca_data)

    })
  }
})

# TAB: Slopes ------------------------------------------------------------------

# Slope settings
output$preslopesettings <- DT::renderDataTable({
  # Reshape results and only choose the columns that are relevant to half life calculation
  preslopesettings <- reshape_pknca_results(res_nca())  %>%
    select(
      any_of(c("USUBJID", "DOSNO")),
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

# Slope Tests

rv$page <- 1
rv$searched_patient <- NULL

# Output slope test plots for each patient
observeEvent(res_nca(), {

  # Define the profiles selected (dosno) that each patient (usubjid) has
  profiles_per_patient(tapply(res_nca()$result$DOSNO, res_nca()$result$USUBJID, unique))

  # Update the patient search input to make available choices for the user
  updatePickerInput(
    session = session,
    inputId = "search_patient",
    label = "Search Patient",
    choices = unique(res_nca()$result$USUBJID)
  )

  # Generate output lambda slope plots for each patient/profile
  for (patient in unique(names(profiles_per_patient()))) {
    for (profile in profiles_per_patient()[[patient]]) {
      local({
        patient <- patient
        profile <- profile

        force(patient)  # Ensure patient is captured correctly
        force(profile)  # Ensure profile is captured correctly

        output_name <- paste0("slopetestplot_", patient, "_", profile)
        output[[output_name]] <- renderPlotly({
          lambda_slope_plot(
            res_nca()$result,
            res_nca()$data$conc$data,
            profile,
            patient,
            0.7
          )
        })
      })
    }
  }
})

# Store all ids from UIs plot-related features
id_inputs <- reactiveValues(
  slope_inputs = NULL,
  ex_inputs = NULL
)

# Settings: Exclusion and Selection for lambda slope calculation (half life) ----
# Define the object to use for organizing the inputs of both
slope_manual_nca_data <- reactiveVal({
  data.frame(
    TYPE = character(),
    PATIENT = character(),
    PROFILE = character(),
    IXrange = character(),
    REASON = character(),
    id = character()
  )
})

# Render as output the table ignoring the Shiny-ID column
output$slope_manual_NCA_data <- DT::renderDataTable({
  datatable(
    data = slope_manual_nca_data()[, c(1:5)],
    escape = FALSE,
    rownames = FALSE,
    editable = TRUE,
    extensions = "FixedHeader",
    options = list(
      paging = FALSE,
      ordering = FALSE,
      searching = FALSE,
      fixedHeader = TRUE,
      preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
      drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } ")
    )
  )
})

# Ensure edits are saved in slope data
observeEvent(input$slope_manual_NCA_data_cell_edit, {
  info <- input$slope_manual_NCA_data_cell_edit

  # Update the reactive data frame with the new value
  slope_manual_nca_data <- slope_manual_nca_data()
  slope_manual_nca_data[info$row, (info$col + 1)] <- info$value
  slope_manual_nca_data(slope_manual_nca_data)
})

output$slope_manual_NCA_data2 <- renderTable({
  update_slope_manual_nca_data(slope_manual_nca_data()[, c(1:5)])
})

row_counter <- reactiveVal(0) # Initialize a counter for the number of exclusions

# Define a function that saves the inputs as values in
# the table once the user has finished inputting them

update_slope_manual_nca_data <- function(slope_manual_nca_data) {
  # Define the columns to be updated
  columns_to_update <- c("TYPE", "PATIENT", "PROFILE", "IXrange", "REASON")

  # Retrieve the last row"s ID for input prefix
  last_id <- slope_manual_nca_data$id[length(slope_manual_nca_data$id)]

  # Update each specified column with the corresponding input value
  for (col in columns_to_update) {
    input_id <- paste0(col, "_", last_id)
    if (!is.null(input[[input_id]])) {
      slope_manual_nca_data[nrow(slope_manual_nca_data), col] <- input[[input_id]]
    }
  }

  # The function returns the object
  return(slope_manual_nca_data)
}

# TAB: Exclusions

# Add a new exclusion to the UI, exclusion data and the reactive list of exclusion IDs
observeEvent(input$add_excsel, {

  # Make previous rows values instead of input widgets
  slope_manual_nca_data <- update_slope_manual_nca_data(slope_manual_nca_data())

  # Create an ID for the new row
  row_counter(row_counter() + 1)
  id <- paste0("Ex_", row_counter())

  # Create the new row as a set of input widgets for the UI
  new_row_slope_manual_nca_data <- data.frame(
    TYPE = as.character(
      selectInput(
        inputId = paste0("TYPE_", id),
        label = "",
        width = "60%",
        choices = c("Selection", "Exclusion"),
        selected = "Selection",
        selectize = TRUE
      )
    ),
    PATIENT = as.character(
      selectInput(
        inputId = paste0("PATIENT_", id),
        label = "",
        width = "60%",
        choices = names(profiles_per_patient()),
        selectize = TRUE
      )
    ),
    PROFILE = as.character(
      selectInput(
        inputId = paste0("PROFILE_", id),
        label = "",
        width = "60%",
        choices = unname(unique(unlist(profiles_per_patient()))),
        selectize = TRUE
      )
    ),
    IXrange = as.character(
      textInput(
        inputId = paste0("IXrange_", id),
        label = "",
        width = "70%",
        value = "1:3"
      )
    ),
    REASON = as.character(
      textInput(
        inputId = paste0("REASON_", id),
        value = "",
        label = "",
        width = "80%"
      )
    ),
    id = id
  )

  # Bind the row to the reactive value displayed in the UI
  slope_manual_nca_data(rbind(slope_manual_nca_data, new_row_slope_manual_nca_data))
})


# Allow the user to delete the rows selected in the UI table
observeEvent(input$remove_excsel, {
  if (!is.null(input$slope_manual_NCA_data_rows_selected)) {
    # Make previous rows values instead of input widgets
    slope_manual_nca_data <- update_slope_manual_nca_data(slope_manual_nca_data())

    # Reset the variable deleting the selected rows
    slope_manual_nca_data(
      slope_manual_nca_data[-as.numeric(input$slope_manual_NCA_data_rows_selected), ]
    )
  }
})


# SERVER LOGIC: connecting exclusion and manual slope selection to rerun NCA function call

# Save the exclusion/selection data to the server data and rerun the NCA results
handle_nca_excsel <- function() {

  if (input$submit_analyte == 0) {
    showNotification(
      "Data issue: Please select an Analyte in the Data Selection tab",
      duration = NULL,
      closeButton = TRUE,
      type = "error"
    )
    return(NULL)
  }
  # Update the data in mydata() to reflect the changes in the exclusion/selection table
  mydata <- mydata()

  # Reset to 0 all previous (if done) changes
  mydata$conc$data$is.included.hl <- FALSE
  mydata$conc$data$is.excluded.hl <- FALSE
  mydata$conc$data$exclude_half.life <- FALSE

  # If there is no specification there is nothing to save
  if (nrow(slope_manual_nca_data()) == 0) {

    # Rerun the NCA with the modified data
    mydata(mydata)

    # Stop the observeEvent
    return()
  }

  # Make previous rows values instead of input widgets
  slope_manual_nca_data <- update_slope_manual_nca_data(slope_manual_nca_data())


  # Eliminate all rows with conflicting or blank values
  slope_manual_nca_data(slope_manual_nca_data  %>%
    filter(
      PATIENT %in% names(profiles_per_patient()),
      PROFILE %in% unname(unlist(profiles_per_patient()[PATIENT])),
      all(!is.na(sapply(IXrange, function(x) eval(parse(text = x))))) &
        all(!is.null(sapply(IXrange, function(x) eval(parse(text = x))))),
    )  %>%
    # Eliminate duplicated records within the same profile
    filter(
      !duplicated(
        paste0(PATIENT, PROFILE, IXrange, fromLast = TRUE),
        !(duplicated(paste0(PATIENT, PROFILE), fromLast = TRUE))
      )
    )
  )

  # Update the exclusion/selection data for Lambda based on the current exc/sel table
  for (i in seq_len(nrow(slope_manual_nca_data()))) {
    #
    #update inclusions
    if (slope_manual_nca_data()$TYPE[i] == "Selection") {
      mydata$conc$data <- mydata$conc$data %>%
        mutate(
          is.included.hl = ifelse(
            USUBJID == slope_manual_nca_data()$PATIENT[i] &
              DOSNO == slope_manual_nca_data()$PROFILE[i] &
              IX %in% eval(parse(text = slope_manual_nca_data()$IXrange[i])),
            TRUE,
            is.included.hl
          ),
          REASON = ifelse(
            USUBJID == slope_manual_nca_data()$PATIENT[i] &
              DOSNO == slope_manual_nca_data()$PROFILE[i] &
              IX %in% eval(parse(text = slope_manual_nca_data()$IXrange[i])),
            slope_manual_nca_data()$REASON[i],
            REASON
          )
        )
    } else {
      #update exclusions
      mydata$conc$data <- mydata$conc$data %>%
        mutate(
          is.excluded.hl = ifelse(
            USUBJID == slope_manual_nca_data()$PATIENT[i] &
              DOSNO == slope_manual_nca_data()$PROFILE[i] &
              IX %in% eval(parse(text = slope_manual_nca_data()$IXrange[i])),
            TRUE,
            is.excluded.hl
          ),
          REASON = ifelse(
            USUBJID == slope_manual_nca_data()$PATIENT[i] &
              DOSNO == slope_manual_nca_data()$PROFILE[i] &
              IX %in% eval(parse(text = slope_manual_nca_data()$IXrange[i])),
            slope_manual_nca_data()$REASON[i],
            REASON
          )
        )
    }
  }
  mydata$conc$data <- mydata$conc$data %>%
    group_by(STUDYID, USUBJID, PCSPEC, DOSNO) %>%
    mutate(
      exclude_half.life = {
        if (any(is.included.hl)) {
          is.excluded.hl | !is.included.hl
        } else {
          is.excluded.hl
        }
      }
    )

  mydata(mydata)

}

# Observe input$nca
observeEvent(input$nca, {
  handle_nca_excsel()
})

# Observe input$save_excsel
observeEvent(input$save_excsel, {
  handle_nca_excsel()
  rv$trigger <- rv$trigger + 1
})


# Define the lambda slope plots for each patient/profile
patient_profile_plotids <- reactiveVal(NULL)
observeEvent(list(input$nca, input$search_patient), {
  req(res_nca())

  # Make sure the search_patient input is not NULL
  if (is.null(input$search_patient) | length(input$search_patient) == 0) {
    search_patient <- unique(res_nca()$result$USUBJID)
  } else {
    search_patient <- input$search_patient
  }

  # Create a vector with each patient/profile lambda slope plot ID
  patient_profile_plotids(
    mydata()$conc$data %>%
      filter(
        DOSNO %in% input$select_dosno,
        USUBJID %in% search_patient
      ) %>%
      select(USUBJID, DOSNO) %>%
      unique() %>%
      arrange(USUBJID, DOSNO) %>%
      mutate(id = paste0("slopetestplot_", USUBJID, "_", DOSNO)) %>%
      pull(id)
  )
})

# To elude confusions between user and UI, changes in the search engine will reset the page to 1
observeEvent(list(input$search_patient, rv$plots_per_page), {
  rv$page <- 1
})

# Update the page number when the user selects a new page
observeEvent(input$page_selector, {
  rv$page <- input$page_selector
})
observeEvent(input$prev_page, {
  rv$page <- rv$page - 1
})
observeEvent(input$next_page, {
  rv$page <- rv$page + 1
})

plot_outputs <- reactiveVal(NULL)
prev_button <- reactiveVal(NULL)
next_button <- reactiveVal(NULL)
page_info <- reactiveVal(NULL)
page_selector <- reactiveVal(NULL)

# Control with the page number, the patient search and the NCA results the plots shown in the App
observeEvent(list(res_nca(), rv$page, patient_profile_plotids(), input$plots_per_page), {
  # Get a list with the number of plots you need to display
  num_plots_per_page <- as.numeric(req(input$plots_per_page))
  num_plots <- length(patient_profile_plotids())
  rv$num_pages <- ceiling(num_plots / num_plots_per_page)

  rv$page_per_plot <- rep(
    1:rv$num_pages, each = num_plots_per_page
  )[seq_along(patient_profile_plotids())]

  # Define the UI pagination interface based on page number
  prev_button(
    if (rv$page > 1) {
      actionButton("prev_page", "Previous Page")
    } else {
      NULL
    }
  )

  next_button(
    if (rv$page < rv$num_pages) {
      actionButton("next_page", "Next Page")
    } else {
      NULL
    }
  )

  page_info(paste("Page", rv$page, "of", rv$num_pages))
  page_selector(
    numericInput(
      "page_selector",
      "Jump to page:",
      value = rv$page,
      min = 1,
      max = rv$num_pages,
      step = 1
    )
  )

  # Keep a list outputing the plots that need to be displayed
  plot_outputs(
    lapply(
      patient_profile_plotids()[rv$page_per_plot == as.numeric(rv$page)],
      function(id) {
        plotlyOutput(id)
      }
    )
  )
})

output$slopetestUI <- renderUI({
  fluidRow(
    plot_outputs(),
    fluidRow(
      column(4, align = "left", prev_button()),
      column(4, align = "center", page_info(), page_selector()),
      column(4, align = "right", next_button())
    )
  )
})

# Make a copy of mydata() to make transitory changes in the plots easily
mydata2 <- reactiveVal()
observeEvent(mydata(), mydata2(mydata()))

# Define the click events for the point exclusion and selection in the slope plots
click_counter <- reactiveVal(0)
firstclick_vals <- reactiveValues(patient = NULL, profile = NULL, idx_pnt = NULL)

observeEvent(event_data("plotly_click", priority = "event"), {
  # Store the information of the last click event
  click_data <- event_data("plotly_click")

  if (!is.null(click_data) & !is.null(click_data$customdata)) {
    # Get identifiers of the clicked plot
    patient <- gsub("(.*)_.*_.*", "\\1",  click_data$customdata)
    profile <- gsub(".*_(.*)_.*", "\\1",  click_data$customdata)
    idx_pnt <- gsub(".*_.*_(.*)", "\\1",  click_data$customdata)

    # Increment the click counter
    click_counter(click_counter() + 1)

    if (click_counter() %% 2 == 1) {
      firstclick_vals$patient <- patient
      firstclick_vals$profile <- profile
      firstclick_vals$idx_pnt <- idx_pnt
    }

    # When second click happens in the plot an event should occur
    if (click_counter() %% 2 == 0 & click_counter() > 0) {
      # If the user clicks another plot after one click, reset everything and start over
      if (patient != firstclick_vals$patient | profile != firstclick_vals$profile) {
        click_counter(1)
        firstclick_vals$patient <- patient
        firstclick_vals$profile <- profile
        firstclick_vals$idx_pnt <- idx_pnt

        # If the user clicked in the same plot, perform an action over the temporary data
      } else {
        # Define a temporary data that does not affect the original until is
        # saved by the user (save_excsel)
        mydata2 <- mydata2()

        # Modify the data for the plot according to the user's clicks
        mydata2$conc$data <- mydata2$conc$data %>%
          # If the user clicked two different points, do their selection
          mutate(
            is.included.hl = case_when(
              idx_pnt == firstclick_vals$idx_pnt ~ is.included.hl,
              USUBJID == patient &
                DOSNO == profile & IX %in% firstclick_vals$idx_pnt:idx_pnt ~ TRUE,
              TRUE ~ FALSE
            ),
            # If the user clicked two times the same point, do its exclusion
            is.excluded.hl = case_when(
              idx_pnt != firstclick_vals$idx_pnt ~ is.excluded.hl,
              USUBJID == patient & DOSNO == profile & IX %in% idx_pnt ~ !is.excluded.hl,
              TRUE ~ is.excluded.hl
            )
          ) %>%
          group_by(STUDYID, USUBJID, PCSPEC, DOSNO) %>%
          mutate(
            exclude_half.life = {
              if (any(is.included.hl)) {
                is.excluded.hl | !is.included.hl
              } else {
                is.excluded.hl
              }
            }
          )
        mydata2(mydata2)

        # Change the plot of only the profile and patient selected
        mydata2$conc$data <- mydata2$conc$data %>% filter(USUBJID == patient, DOSNO == profile)
        mydata2$dose$data <- mydata2$dose$data %>% filter(USUBJID == patient, DOSNO == profile)
        myres2 <- suppressWarnings(PKNCA::pk.nca(data = mydata2, verbose = FALSE))

        # Alter the output with the transitory changes and the new slope plot
        output[[paste0("slopetestplot_", patient, "_", profile)]] <- renderPlotly({
          lambda_slope_plot(
            myres2$result,
            myres2$data$conc$data,
            profile,
            patient,
            ifelse(input$rule_adj.r.squared, input$adj.r.squared_threshold, 0.7)
          )
        })

        ## Make UI changes in the table displayed
        # 1) If the point selected is a exclusion that was already indicated, then remove previous
        #    records from the UI table and stop the observeEvent
        if (idx_pnt == firstclick_vals$idx_pnt &&
          all(!mydata2$conc$data$is.excluded.hl[
            mydata2$conc$data$USUBJID == patient &
              mydata2$conc$data$DOSNO == profile &
              mydata2$conc$data$IX == idx_pnt
          ])) {

          condition_vr <- {
            slope_manual_nca_data()$PATIENT == patient &
              slope_manual_nca_data()$PROFILE == profile &
              sapply(
                slope_manual_nca_data()$IXrange,
                function(x) idx_pnt %in% eval(parse(text = paste0("c(", x, ")")))
              )
          }

          slope_manual_nca_data <- slope_manual_nca_data() %>%
            mutate(
              IXrange = ifelse(
                condition_vr,
                yes = {
                  ixrange <- eval(parse(text = paste0("c(", IXrange, ")")))
                  ixrange <- ixrange[ixrange != idx_pnt]
                  paste(ixrange, collapse = ",")
                },
                no = IXrange
              )
            ) %>%
            # delete all rows where IXrange does not contain a numeric value
            filter(grepl("\\d.*", IXrange))

          slope_manual_nca_data(slope_manual_nca_data)
        } else {
          # 2) If the point selected is a selection or a exclusion that was not indicated
          #    then include it also in the UI table
          row_counter(row_counter() + 1)
          id <- paste0("Ex_", row_counter())

          new_row_slope_manual_nca_data <- data.frame(
            TYPE = ifelse(idx_pnt != firstclick_vals$idx_pnt, "Selection", "Exclusion"),
            PATIENT = patient,
            PROFILE = as.character(profile),
            IXrange = paste0(firstclick_vals$idx_pnt, ":", idx_pnt),
            REASON = "[Graphical selection. Double click here to include a reason]",
            id = id
          )
          slope_manual_nca_data(rbind(slope_manual_nca_data(), new_row_slope_manual_nca_data))
        }
      }
    }
  }
})
# TAB: Summary Tables ----------------------------------------------------------

# Save summary stats to improve
observeEvent(input$downloadsum, {
  showModal(modalDialog(
    title = "Please enter the path to the folder on improve for your results:",
    textInput("pathresults", "Path:"),
    textInput("filename", "Enter filename of choice:", ""),
    actionButton("go2", "GO"),
    footer = modalButton("Close")
  ))
})

# Save to working directory

shinyDirChoose(input, "dir", roots = c(home = "~"))

observeEvent(input$downloadsum_csv, {
  showModal(
    modalDialog(
      title = "Local Download",
      shinyDirButton("dir", "Select directory", "Download"),
      textInput("fileNameInput", "Enter filename", value = "data_summary.csv"),
      textOutput("selectedDirText"),
      footer = tagList(
        actionButton("confirmBtn", "Save"),
        modalButton("Cancel")
      )
    )
  )
})

# path of selected directory
observeEvent(input$dir, {
  output$selectedDirText <- renderText({
    if (!is.null(input$dir)) {
      paste0("Path:", parseDirPath(roots = c(wd = "."), input$dir), "/",  input$fileNameInput)
    } else {
      "No directory selected"
    }
  })
})


# confirm path
observeEvent(input$confirmBtn, {
  req(input$dir)
  req(input$fileNameInput)
  file_path <- file.path(parseDirPath(roots = c(wd = "~"), input$dir), input$fileNameInput)
  write.csv(summary_stats(), file_path)
  session$sendCustomMessage(
    type = "downloadFile",
    message = list(fileName = input$fileNameInput, filePath = file_path)
  )
  removeModal()
})

# alternatively: save to browser
output$downloadsum_browser <- downloadHandler(
  filename = function() {
    paste("NCA_summary.csv", sep = "_")
  },
  content = function(file) {
    write.csv(summary_stats(), file)
  }
)
