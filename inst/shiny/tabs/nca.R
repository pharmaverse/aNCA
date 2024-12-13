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
    inputId = "analyte",
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
    inputId = "cyclenca",
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
  print("trigered")
  # Segregate the data into concentration and dose records
  df_conc <- create_conc(data(), input$analyte, input$proftype)
  df_dose <- create_dose(df_conc)
  
  # Define initially a inclusions/exclusions for lambda slope estimation (with no input)
  df_conc$is.excluded.hl <- FALSE
  df_conc$is.included.hl <- FALSE
  df_conc$REASON <- NA  # Exclusions will have preferential reason statements than inclusions
  df_conc$exclude_half.life <- FALSE
  
  # Make the PKNCA concentration and dose objects
  myconc <- PKNCA::PKNCAconc(
    df_conc,
    formula = AVAL ~ TIME | STUDYID + PCSPEC + USUBJID + DOSNO / ANALYTE,
    exclude_half.life = "exclude_half.life",
    time.nominal = "NFRLT"
  )
  
  mydose <- PKNCA::PKNCAdose(
    data = df_dose,
    formula = DOSEA ~ TIME | STUDYID + PCSPEC + USUBJID + DOSNO,
    route = ifelse(toupper(df_dose$IQROUTE) == "EXTRAVASCULAR", "extravascular", "intravascular"),
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
  print(mydata$conc$columns$groups$group_analyte)
  
  # Redefine units for each analyte and for potential customizations
  unique_analytes <- unique(mydata$conc$data[[mydata$conc$columns$groups$group_analyte]])
  mydata$units <- crossing(
    mydata$units,
    !!sym(mydata$conc$columns$groups$group_analyte) := unique_analytes)  %>%
    dplyr::mutate(PPSTRESU = PPORRESU,
                  conversion_factor = 1)
  
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

observeEvent(input$submit_analyte, priority = -1, {
  print(mydata())
  req(mydata())
  updateSelectInput(
    session,
    inputId = "cyclenca",
    label = "Choose the Dose Number:",
    choices = unique(mydata()$conc$data  %>% filter(ANALYTE == input$analyte) %>% pull(DOSNO))
  )
})

# Parameters units
observeEvent(mydata(),{
             updateActionButton(session = session, 
                                inputId = "open_units_table", 
                                disabled = FALSE)
})
observeEvent(input$open_units_table, {
  showModal(modalDialog(
    title = tagList(
      span("Units of NCA parameter results"),
      tags$button(
        type = "button",
        class = "close",
        `data-dismiss` = "modal",
        `aria-label` = "Close",
        span(`aria-hidden` = "true", HTML("&times;"))
      )
    ),
    selectInput(
      inputId = "select_unitstable_analyte",
      multiple = TRUE,
      label = "Select Analyte:",
      choices = mydata()$conc$data[[mydata()$conc$columns$groups$group_analyte]] %>% unique(),
      selected = mydata()$conc$data[[mydata()$conc$columns$groups$group_analyte]] %>% unique()
    ),
    DTOutput(("modal_units_table")),
    footer = tagList(
      modalButton("Close"),
      actionButton(("save_units_table"), "Save Units Table")
    ),
    size = "l"
  ))
})

params_to_calculate <- reactiveVal(NULL)
observeEvent(mydata()$intervals, {
  params_to_calculate <- names(mydata()$intervals)[sapply(mydata()$intervals, function(x) {
    if (is.logical(x)) {
      any(x)
    } else {
      FALSE
    }
  })]
  params_to_calculate(params_to_calculate)
  print(params_to_calculate())
  print(mydata()$units)
})


modal_units_table <- reactiveVal(NULL)
observeEvent(list(mydata()$units, input$select_unitstable_analyte) ,{
  req(mydata()$units)
  req(input$select_unitstable_analyte)
  modal_units_table <- mydata()$units %>%
    mutate(row_original = 1:n()) %>% 
    dplyr::group_by(PPTESTCD, PPORRESU, PPSTRESU, conversion_factor) %>% 
    dplyr::filter(!!sym(mydata()$conc$columns$groups$group_analyte) == input$select_unitstable_analyte) %>% 
    dplyr::rename(`Parameter` = PPTESTCD,
                  `Default unit` = PPORRESU,
                  `Conversion Factor` = conversion_factor,
                  `Custom unit` = PPSTRESU) %>% 
    dplyr::mutate(Analytes = paste(!!sym(mydata()$conc$columns$groups$group_analyte), collapse = ", "),
                  row_original = paste(row_original, collapse = ",")) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(`Analytes`, `Parameter`, `Default unit`, `Custom unit`, `Conversion Factor`)
  modal_units_table(modal_units_table)
  print(modal_units_table())
})

output$modal_units_table <- DT::renderDT({
  datatable(
    data = modal_units_table(),
    escape = FALSE,
    class = "table table-striped table-bordered",
    editable = list(
      target = "cell",
      disable = list(
        columns = c(1, 2, 3))
    ),
    options = list(
      order = list(2, "desc"),
      paging = FALSE,
      searching = TRUE,
      autoWidth = TRUE,
      dom = "ft",
      rowCallback = JS(
        "function(row, data, index) {",
        "  var paramsToCalculate = ", paste0("['", paste(params_to_calculate(), collapse = "','"), "']"), ";",
        "  if (paramsToCalculate.indexOf(data[2]) === -1) {",
        "    $(row).hide();",
        "  }",
        "}"
      ),
      columnDefs = list(
        list(
          visible = FALSE,
          targets = c(0) # Hides row index, ANALYTE and row_original
        )
      )
    )
  )
})

# Save table changes from the UI into the server
observeEvent(input$selected_modal_units_table_cell_edit, {
  info <- input$selected_modal_units_table_cell_edit
  # 
  # new_tlg_order <- tlg_order()
  # new_tlg_order[new_tlg_order$Selection, ][info$row, info$col] <- info$value
  # tlg_order(new_tlg_order)
})

# observeEvent(input$save_units_table, {
#   
#   removeModal()
# })


# Partial AUC Selection
auc_counter <- reactiveVal(0) # Initialize a counter for the number of partial AUC inputs
intervals_userinput_data <- reactiveVal(NULL)
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
  # If there are intervals defined, update the intervals_userinput reactive value
  if (input$AUCoptions && auc_counter() > 0) {
    # Collect all inputs for the AUC intervals
    input_names_aucmin <- grep("^timeInputMin_", names(input), value = TRUE)
    input_names_aucmax <- grep("^timeInputMax_", names(input), value = TRUE)
    
    auc_mins <- unlist(lapply(input_names_aucmin, function(name) input[[name]]))
    auc_maxs <- unlist(lapply(input_names_aucmax, function(name) input[[name]]))
    
    # Define the intervals specified by the user
    intervals_userinput_data(
      data.frame(start = auc_mins, end = auc_maxs) %>%
        arrange(start, end) %>%
        unique()
    )
    
    # Use the base intervals dataset settings as a reference and cross it with the inputs
    intervals_userinput <- mydata()$intervals %>%
      filter(end == Inf) %>%
      group_by(STUDYID, ANALYTE, USUBJID, DOSNO) %>%
      slice(1) %>%
      ungroup()  %>%
      select(-start) %>%
      select(-end) %>%
      crossing(intervals_userinput_data()) %>%
      # all dataframe columns equal false except
      # aucint.last (without knowing the other column names)
      mutate(auclast = FALSE, aucint.last = TRUE, aucinf.obs = FALSE)
    # Return the output
    intervals_userinput(intervals_userinput)
  }
  
  # Make the user aware if it forgot to select at least 1 DOSNO
  if (is.null(input$cyclenca)) {
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
  mydataconc_new <- mydata()$conc$data %>% filter(DOSNO %in% input$cyclenca)
  profiles_per_patient(tapply(mydataconc_new$DOSNO, mydataconc_new$USUBJID, unique))
})

# run the nca upon button click

res_nca <- eventReactive(rv$trigger, {
  req(!is.null(input$cyclenca))
  
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
      min.hl.points = 2
    )
    
    # Filter the data based on the selected profiles
    mydata <- mydata()
    
    mydata$conc$data <- mydata$conc$data %>%
      filter(DOSNO %in% as.numeric(input$cyclenca))
    
    # Include manually the calculation of AUCpext.obs and AUCpext.pred
    mydata$intervals <- mydata$intervals  %>%
      mutate(aucinf.obs.dn = TRUE,
             cmax.dn = TRUE,
             cav = TRUE,
             ctrough = TRUE,
             vss.iv.obs = TRUE,
             cl.obs = TRUE,
             cl.pred = TRUE,
             f = if (length(unique(mydata$conc$data$PKROUTE)) > 1) TRUE else FALSE,
             vz.obs = TRUE) %>%
      # If so, include the AUC intervals defined by the user
      rbind(intervals_userinput()) %>%
      mutate(
        aucpext.obs = TRUE,
        aucpext.pred = TRUE
      )
    
    # Perform NCA on the profiles selected
    myres <- PKNCA::pk.nca(data = mydata, verbose = FALSE)
    
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
  group_cols <- c(unname(unlist(res_nca()$data$conc$columns$groups)), "start", "end")
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
        ANALYTE = input$analyte,
        doses_selected = ifelse(
          !is.null(input$cyclenca),
          paste0(input$cyclenca, collapse = ","),
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

# Slope selector module #
slope_rules <- slope_selector_server(
  "slope_selector",
  mydata,
  res_nca,
  profiles_per_patient,
  input$cyclenca,
  rv,
  reactive(input$settings_upload)
)

#' display rules on Results -> rules tab
output$manual_slopes2 <- renderTable({
  slope_rules()
})

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
