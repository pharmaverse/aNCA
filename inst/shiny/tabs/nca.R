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

# Make GUI change when new settings are uploaded
observeEvent(input$settings_upload,{
  
  setts = read.csv(input$settings_upload$datapath, na = c("", "NA"))
  
  # Set the basic settings 
  analyte = setts$ANALYTE[1]
  doses_selected = as.numeric(strsplit(as.character(setts$doses_selected), split = ',')[[1]])
  
  # Check that match with the data currently loaded
  if (!setts$ANALYTE[1] %in% unique(data()$ANALYTE) | !all(doses_selected %in% unique(data()$DOSNO))){
    
    showNotification(validate('The analyte selected in the settings file is not present in the data. Please, if you want to use this settings for a different file, make sure all meaningful variables in the file are in the data (ANALYTE, DOSNO...)')
                     , type = "error")
  }
  
  # Analyte
  updateSelectInput(session, inputId = 'analyte', label='Choose the analyte:', choices=ADNCA()$ANALYTE[1],
                    selected = setts$ANALYTE[1])
  
  # Dose number
  updateSelectInput(session, inputId = "cyclenca", label = "Choose the Dose Number:", choices = sort(unique(ADNCA()  %>% filter(ANALYTE==setts$ANALYTE[1])  %>% pull(DOSNO))), 
                    selected = doses_selected)
  
  
  # Extrapolation Method
  method_choices = c("Linear Log", "LinearUp LogDown", "Linear LinearInterpolation", "Linear LinearLogInterpolation")
  updateSelectInput(session, inputId = 'method', label='Extrapolation Method:', choices=c("lin-log", "lin up/log down", "linear", "Linear LinearLogInterpolation"),
                    selected= setts$method)
  
  
  # AUC intervals
  if (!is.na(setts$AUC_mins[1])){
    updateCheckboxInput(session, inputId = 'AUCoptions', label = "Select Partial AUC", value = T)
    AUC_mins = strsplit(setts$AUC_mins[1], split = ',')[[1]]
    AUC_maxs = strsplit(setts$AUC_maxs[1], split = ',')[[1]]
    
    for (i in 1:length(AUC_mins)){
      AUC_counter(AUC_counter() + 1)
      insertUI(selector = "#AUCInputs", where = "beforeEnd", 
               ui = partialAUCInput(id=paste0("AUC_", AUC_counter()),
                                    min_sel_value = as.numeric(AUC_mins[i]),
                                    max_sel_value = as.numeric(AUC_maxs[i])
               )
      )
    }
  }
  
  # RSADJ
  if(!is.na(setts$adj.r.squared_threshold[1])){
    updateCheckboxInput(session, inputId = 'rule_adj.r.squared', label = "RSQADJ:", value = T)
    updateNumericInput(session, "adj.r.squared_threshold", "", value = setts$adj.r.squared_threshold[1]) 
  } else{
    updateCheckboxInput(session, inputId = 'rule_adj.r.squared', label = "RSQADJ:", value = F)
  }
  
  
  # AUCPE.Obs 
  if(!is.na(setts$aucpext.obs_threshold[1])){
    updateCheckboxInput(session, inputId = 'rule_aucpext.obs', value = T)
    updateNumericInput(session, "aucpext.obs_threshold", value = setts$aucpext.obs_threshold[1])
  } else{
    updateCheckboxInput(session, inputId = 'rule_aucpext.obs', label = "", value = F)
  }
  
  
  # AUCPE.Pred
  if(!is.na(setts$aucpext.pred_threshold[1])){
    updateCheckboxInput(session, inputId = 'rule_aucpext.pred',  value = T)
    updateNumericInput(session, "aucpext.pred_threshold", value = setts$aucpext.pred_threshold[1])
  } else{
    updateCheckboxInput(session, inputId = 'rule_aucpext.pred', value = F)
  }
  
  
  # SPAN
  if(!is.na(setts$span.ratio_threshold[1])){
    updateCheckboxInput(session, inputId = 'rule_span.ratio', label = "SPAN: ", value = T)
    updateNumericInput(session, "span.ratio_threshold", "", value = setts$span.ratio_threshold[1]) 
  } else{
    updateCheckboxInput(session, inputId = 'rule_span.ratio', label = "SPAN:", value = F)
  }
  
  
  # Lambda slope point selections and exclusions
  slope_manual_NCA_data = setts  %>% select(TYPE, USUBJID, DOSNO, IX, REASON)  %>% 
    mutate(PATIENT=as.character(USUBJID), PROFILE=as.character(DOSNO))  %>%
    group_by(TYPE, PATIENT, PROFILE, REASON)  %>%
    summarise(IXrange=paste0(IX, collapse=','))  %>% 
    select(TYPE, PATIENT, PROFILE, IXrange, REASON)
  
  slope_manual_NCA_data(slope_manual_NCA_data)
  
})




# Display the PKNCA data object for the user (concentration records)
output$datatable <- renderDataTable({
  req(mydata())
  DT::datatable(data=mydata()$conc$data,
                options=list(scrollX=TRUE,
                             scrollY = TRUE,
                             lengthMenu = list(c(10, 25, -1), c('10', '25', 'All'))
                )
  )
})


# TAB: Settings ----------------------------------------------------------------

# IN this tab we can set the dose number to be analyzed, the extrapolation 
# method, potenital partial AUC and all the flag rule sets


# Define the profiles (dosno) associated with each patient (usubjid) for the selected analyte
profiles_per_patient <- reactiveVal(NULL)
observeEvent(input$submit_analyte, {
  profiles_per_patient(tapply(mydata()$conc$data$DOSNO, mydata()$conc$data$USUBJID, unique))
  
})


# Include keyboard limits for the settings GUI display

# Define a function that simplifies the action
input_limit = function(input_id, max=Inf, min=-Inf, update_fun=updateNumericInput){
  observeEvent(input[[input_id]],{
    if(input[[input_id]]<min & !is.na(input[[input_id]])){update_fun(session,input_id,"", value=min)}
    if(input[[input_id]]>max & !is.na(input[[input_id]])){update_fun(session,input_id,"", value=max)}
  })
}

# Keyboard limits for the setting thresholds
input_limit('adj.r.squared_threshold', max = 1, min = 0)
input_limit('aucpext.obs_threshold', max=100, min=0)
input_limit('aucpext.pred_threshold', max=100, min=0)
input_limit('span.ratio_threshold', min=0)

# Keyboard limits for the dynamically created partial AUC ranges 
observe({
  inputs_list <- reactiveValuesToList(input)
  for(input_id in names(inputs_list)) {
    if (startsWith(input_id, "timeInput")) {
      local({
        my_input_id <- input_id
        observeEvent(input[[my_input_id]], {
          if(is.numeric(input[[my_input_id]]) && input[[my_input_id]] < 0) {
            input_limit(my_input_id, min = 0)
          }
        })
      })
    }
  }
})


# Choose dosenumbers to be analyzed

observeEvent(input$analyte, {
  updateSelectInput(session, inputId = 'cyclenca', label = 'Choose the Dose Number:', 
                    choices = unique(data()  %>% filter(ANALYTE==input$analyte)  %>% pull(DOSNO)))
})

# Partial AUC Selection
AUC_counter <- reactiveVal(0) # Initialize a counter for the number of partial AUC inputs
intervals_userinput_data <- reactiveVal(NULL)
intervals_userinput <- reactiveVal(NULL)

# Add a new partial AUC input
observeEvent(input$addAUC, {
  AUC_counter(AUC_counter() + 1)
  id <- paste0("AUC_", AUC_counter())
  insertUI(selector = "#AUCInputs", where = "beforeEnd", ui = partialAUCInput(id))
})

# Remove the last partial AUC input  
observeEvent(input$removeAUC, {
  if (AUC_counter() > 0) {
    removeUI(selector = paste0("#", paste0("AUC_", AUC_counter())))
    AUC_counter(AUC_counter() - 1)
  }
})


# NCA button object

# Create a reactive values object
rv <- reactiveValues(trigger = 0)

# Update the trigger whenever either button is clicked
observeEvent(input$nca,{
  req(mydata())
  
 # If there are intervals defined, update the intervals_userinput reactive value
  if(input$AUCoptions && AUC_counter()>0){

    # Collect all inputs for the AUC intervals
    input_names_AUCmin <- grep("^timeInputMin_", names(input), value = TRUE)
    input_names_AUCmax <- grep("^timeInputMax_", names(input), value = TRUE)

    AUC_mins <- unlist(lapply(input_names_AUCmin, function(name) input[[name]]))
    AUC_maxs <- unlist(lapply(input_names_AUCmax, function(name) input[[name]]))

  # Define the intervals specified by the user
  intervals_userinput_data(data.frame(start=AUC_mins, end=AUC_maxs)  %>% 
    arrange(start, end)  %>% 
    unique())
  
  print(intervals_userinput_data())

  # Use the base intervals dataset settings as a reference and cross it with the inputs
  intervals_userinput = mydata()$intervals %>%
      filter(end==Inf)  %>% 
      group_by(STUDYID,ANALYTE, USUBJID, DOSNO) %>% 
      slice(1) %>% 
      ungroup()  %>% 
      select(-start)  %>% select(-end)  %>% 
      crossing(intervals_userinput_data()) %>% 
      # all dataframe columns equal false except aucint.last (without knowing the other column names)
    mutate(auclast = FALSE, aucint.last=TRUE, aucinf.obs = FALSE)

  
  
  
  # Return the output
  intervals_userinput(intervals_userinput)
  }

  # Make the user aware if it forgot to select at least 1 DOSNO 
  if (is.null(input$cyclenca)) {
    showNotification("Please select a profile from the 'Settings' tab to proceed.", duration = NULL, closeButton = TRUE, type = "warning")
  }
  # Lead the user to the Results section
  else {
    rv$trigger <- rv$trigger + 1
    updateTabsetPanel(session, "ncapanel", selected = "Results")
  }

  # Update profiles per patient considering the profiles selected
  mydataconc_new = mydata()$conc$data  %>% filter(DOSNO %in% input$cyclenca)
  profiles_per_patient(tapply(mydataconc_new$DOSNO, mydataconc_new$USUBJID, unique))
}
)

# run the nca upon button click

resNCA <- eventReactive(rv$trigger, {
  req(!is.null(input$cyclenca))
  
  withProgress(message = 'Calculating NCA...', value = 0, {
    req(mydata())
    
    # Increment progress to 50% after getting dataNCA
    incProgress(0.5, detail = "Performing NCA calculations...")
    
    # Use the user inputs to determine the NCA settings to apply
    PKNCA::PKNCA.options(
      auc.method=input$method,
      allow.tmax.in.half.life = T,

      # Make sure the standard options do not prohibit results
      min.hl.r.squared = 0.001,
      min.span.ratio = Inf,
      min.hl.points = 2
      # tau.choices=adnca_conc$TAU[1]
    )
    
    # Filter the data based on the selected profiles
    mydata = mydata()

    mydata$conc$data = mydata$conc$data %>% filter(DOSNO %in% as.numeric(input$cyclenca))
    
    
    # Include manually the calculation of AUCpext.obs and AUCpext.pred 
      mydata$intervals = mydata$intervals  %>%
      mutate(aucinf.obs.dn = T,
             cmax.dn = T,
             cav = T,
             ctrough = T,
             vss.iv.obs = T,
             cl.obs = T,
             cl.pred = T,
             f = if (length(unique(mydata$conc$data$PKROUTE))>1) T else F,
             vz.obs = T)  %>% 
        # If so, include the AUC intervals defined by the user
        rbind(intervals_userinput())  %>%
        mutate(aucpext.obs = T, 
               aucpext.pred = T
               )

      

    # Perform NCA on the profiles selected
    myres = PKNCA::pk.nca(data=mydata, verbose=F)
    
    # Access the result component
    nca_results <- myres$result
    
    
    # Optionally, view the entire results in a viewer (if using RStudio)
    
    
    # Increment progress to 100% after NCA calculations are complete
    incProgress(0.5, detail = "NCA calculations complete!")

    # Return the result
    return(myres)
  })
})


# TABSET: Results ==============================================================

# In the result tabset we can view the NCA results, slope caclulation und exclusions table.

# TAB: NCA Results -------------------------------------------------------------

# Create a reactive expression to store the reshaped form of the results that will be displayed in the UI
finalresNCA = reactiveVal(NULL)

# creative finalresNCA, aiming to present the results in a more comprehensive way
observeEvent(resNCA(), {
  
  # Create a reshaped object that will be used to display the results in the UI
  finalresNCA = reshape_PKNCA_results(resNCA())

  # Get all inputs which are TRUE and start with 'rule_'
  for (rule_input in grep('^rule_', names(input), value = TRUE)){
    
    if(!input[[rule_input]]){next}
     pptestcd = gsub('rule_', '', rule_input)
     if(startsWith(pptestcd, 'auc')) finalresNCA[[paste0('flag_',pptestcd)]] = finalresNCA[[pptestcd]] >= input[[paste0(pptestcd, '_threshold')]] 
     else finalresNCA[[paste0('flag_',pptestcd)]] = finalresNCA[[pptestcd]] <= input[[paste0(pptestcd, '_threshold')]] 
  }

  # Include units for all column names
  dict_pttestcd_with_units = resNCA()$result  %>% select(PPTESTCD, PPORRESU)  %>% unique()  %>%  pull(PPORRESU,PPTESTCD) 
  finalresNCA = finalresNCA  %>%
     rename_with(~ifelse(gsub('_.*','',.x) %in% names(dict_pttestcd_with_units), paste0(.x, "[", dict_pttestcd_with_units[gsub('_.*','',.x)],']'), .x))
  
  # Sort alphabetically all columns but the grouping and the exclude columns
  group_cols = c(unname(unlist(resNCA()$data$conc$columns$groups)), 'start', 'end')
  exclude_cols = names(finalresNCA)[startsWith(names(finalresNCA),'exclude.')]
  finalresNCA = finalresNCA[, c(group_cols, 
                                sort(setdiff(names(finalresNCA),c(group_cols, exclude_cols))),
                                sort(exclude_cols)
                                )]
  
  # Create a reshaped object
  
  finalresNCA(finalresNCA   %>% 
    # Create a flag_ column when input$rule_r2adj is TRUE, and make it TRUE when the only column that starts with r2adj is below the threshold input$r2adj_threshold
    # mutate(flag_r2adj = ifelse(PPORRES < input$r2adj_threshold, T, F))  %>%
    mutate(flag_onlyFalse = F)  %>%
    # Create a flagged column that informs if any of the variables starting as 'flag_' are TRUE
    # Only if there is at least one column that stats with flag_
        mutate(flagged = case_when(
      rowSums(is.na(select(., starts_with('flag_')))) > 0 ~ 'MISSING',
      rowSums(select(., starts_with('flag_')), na.rm=T) > 0 ~ 'FLAGGED',
      T ~ 'ACCEPTED'
      ))  # %>%  
    # Deselect all useless columns
    # select(-any_of(c('flag_onlyFalse', names(resNCA()$data$conc$data)))) 
    )

  # Allow user to choose the parameters to display of finalresNCA
  updatePickerInput(session=session, inputId='params', label='Select Parameters :', choices = sort(colnames(finalresNCA())), selected = sort(colnames(finalresNCA())) )
  # 
})

# Render the reshaped results as a DT datatable 
output$myresults <- renderDataTable({
  req(finalresNCA())
  DT::datatable(data = finalresNCA(),
                options = list(scrollX = TRUE,
                               scrollY = TRUE,
                               lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')),
                               columnDefs = list(list(visible = F, targets = setdiff(colnames(finalresNCA()), input$params ) ) )  
                              )
               ) %>% 
               formatStyle('flagged', target='row', backgroundColor = styleEqual(c('FLAGGED', 'MISSING'),c('#f5b4b4', '#cbaddd')))
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

# Save the data frame as a CSV with the generated file name direct to improve
# observeEvent(input$go, {
#   
#   req(finalresults(), df_nca())
# 
#   filename = paste(df_nca()$STUDYID[1], "Pk_Parameters.csv")
#   write.csv(finalresults(), file = filename)
# 
#   #upload new file to improve
# 
#   improveRmodify::createFile(fileName = paste("test",filename),
#                              targetIdent = input$pathresults,
#                              localPath = paste("./", filename, sep = ""))
# })


## Add download locally option

output$local_download_NCAres = downloadHandler(
  filename=function(){
    paste0(mydata()$conc$data$STUDYID[1], 'results.zip')
  },
  content = function(file){
    old_wd <- getwd()  # save old working directory  
    tempdir <- tempdir()  # create a temporary directory  
    setwd(tempdir)  # change working directory to temporary directory  
    
    csv <- 'PK_Parameters.csv'
    cdisc <- 'cdisc'
    
    write.csv(finalresults(), csv)
    # HERE WE NEED TO WRITE ADPP AND ADPC FUNCTION TO WRITE CDISC FILES 
    # zip(zipfile = file, files = c(csv, cdisc))
    
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
    myconc = resNCA()$data$conc

    # Create a settings file that the user can download/upload for establishing the same configuration
    setts_lambda = myconc$data %>% 
      # Identify the points that the user has manually selected for the half-life calculation
      mutate(TYPE = case_when(is.excluded.hl ~ 'Exclusion', is.included.hl ~ 'Inclusion', T ~ NA))  %>%
      filter(is.excluded.hl | is.included.hl)  %>%
      select(any_of(c(unname(unlist(myconc$columns$groups)), 'IX', myconc$columns$time, myconc$columns$concentration, 'TYPE', 'REASON')))  
        

    # Make sure that there is at least one row so the settings can be considered
    if (nrow(setts_lambda) == 0) setts_lambda = setts_lambda  %>% add_row()

    # Consider the intervals defined by the user for the AUC calculation
    input_names_AUCmin <- grep("^timeInputMin_", names(input), value = TRUE)
    input_names_AUCmax <- grep("^timeInputMax_", names(input), value = TRUE)
    AUC_mins <- unlist(lapply(input_names_AUCmin, function(name) input[[name]]))
    AUC_maxs <- unlist(lapply(input_names_AUCmax, function(name) input[[name]]))  

    # Include the rule settings as additional columns
    setts = setts_lambda  %>% 
      mutate(ANALYTE=input$analyte,
             doses_selected = ifelse(!is.null(input$cyclenca), paste0(input$cyclenca, collapse = ','), unique(mydata()$conc$data$DOSNO)),
             method = input$method,
             adj.r.squared_threshold = ifelse(input$rule_adj.r.squared, input$adj.r.squared_threshold, NA),
             aucpext.obs_threshold = ifelse(input$rule_aucpext.obs, input$aucpext.obs_threshold, NA), 
             aucpext.pred_threshold = ifelse(input$rule_aucpext.pred, input$aucpext.pred_threshold, NA),
             span.ratio_threshold = ifelse(input$rule_span.ratio, input$span.ratio_threshold, NA),
             AUC_mins = if(is.null(AUC_mins)) NA else paste(AUC_mins, collapse=','),
             AUC_maxs = if(is.null(AUC_maxs)) NA else paste(AUC_maxs, collapse=',')
             )
    
    write.csv(setts, file)
  },
  contentType = "text/csv"
)


# Keep the UI table constantly actively updated
observe({
for (input_name in grep('(TYPE|PATIENT|PROFILE|IXrange|REASON)_Ex\\d+$', names((input)), value = TRUE)){
  observeEvent(input[[input_name]], {
    
    # Get the ID of the exclusion
    id <- gsub('_(Ex\\d+)$', '', input_name)
    
    # Update the reactive list of exclusion IDs
    slope_manual_NCA_data = slope_manual_NCA_data()
    slope_manual_NCA_data[slope_manual_NCA_data$id == id, ] -> set_selected_value( slope_manual_NCA_data[slope_manual_NCA_data$id == id, ], paste0(input[[input_name]]) )
    slope_manual_NCA_data(slope_manual_NCA_data)

    
  })
}
})

# TAB: Slopes ------------------------------------------------------------------

# Slope settings
output$preslopesettings <- renderDataTable({

  # Reshape results and only choose the columns that are relevant to half life calculation
  preslopesettings = reshape_PKNCA_results(resNCA())  %>% 
    select(any_of(c('USUBJID', 'DOSNO')), starts_with('lambda.z'), starts_with('span.ratio'), starts_with('half.life'), 
    #'lambda.z.ix', 
   'exclude.lambda.z')   


  # Render the DT datatable object
  DT::datatable(data=preslopesettings,
                options=list(scrollX=TRUE,
                             scrollY = TRUE,
                             lengthMenu = list(c(10, 25, -1), c('10', '25', 'All'))
                             #columnDefs = list(list(visible = F, targets = setdiff(colnames(finalresNCA()), input$params ) ) )
                            )
                ) %>% 
                 formatStyle(
    'exclude.lambda.z',
    target = 'row',
    backgroundColor = styleEqual(NA, NA, default = '#f5b4b4')
  )
})

# Slope Tests 

rv$page <- 1 
rv$searched_patient <- NULL

# Output slope test plots for each patient
observeEvent(resNCA(),{

  # Define the profiles selected (dosno) that each patient (usubjid) has
  profiles_per_patient(tapply(resNCA()$result$DOSNO, resNCA()$result$USUBJID, unique))
  
  # Update the patient search input to make available choices for the user
  updatePickerInput(session = session, inputId = "search_patient", label = "Search Patient", 
                    choices = unique(resNCA()$result$USUBJID))
  
  # Generate output lambda slope plots for each patient/profile
  for (patient in unique(names(profiles_per_patient()))) {

    for (profile in profiles_per_patient()[[patient]]) {

      local({
        patient <- patient
        profile <- profile
        
        force(patient)  # Ensure patient is captured correctly
        force(profile)  # Ensure profile is captured correctly
        
        output_name <- paste0("slopetestplot_", patient, '_', profile)
        output[[output_name]] <- renderPlotly({
          
          lambda_slope_plot(PKNCAres_df = resNCA()$result,
                            PKNCAconc_df = resNCA()$data$conc$data,
                            dosno = profile,
                            usubjid = patient,
                            R2ADJTHRESHOL = 0.7
          )
        })
      })
    }
  }
})

# Store all ids from UIs plot-related features 
id_inputs = reactiveValues(
  slope_inputs = NULL,
  ex_inputs = NULL
)


# Settings: Exclusion and Selection for lambda slope calculation (half life) ----


# Define the object to use for organizing the inputs of both
slope_manual_NCA_data <- reactiveVal(data.frame(TYPE=character(), PATIENT=character(), PROFILE=character(), 
                                                IXrange=character(), REASON=character(), id=character()))

# Render as output the table ignoring the Shiny-ID column
output$slope_manual_NCA_data <- renderDataTable(datatable(data=slope_manual_NCA_data()[,c(1:5)], escape=F, 
                                         rownames= FALSE, editable=TRUE,
                                         options = list(paging = FALSE,ordering=FALSE,searching=FALSE, 
                                         preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                         drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } ')
                                         )))

output$slope_manual_NCA_data2 <- renderTable(update_slope_manual_NCA_data(slope_manual_NCA_data()[,c(1:5)]))

rowCounter <- reactiveVal(0) # Initialize a counter for the number of exclusions  

# Define a function that saves the inputs as values in the table once the user has finished inputting them
update_slope_manual_NCA_data <- function(slope_manual_NCA_data){
      # Define the columns to be updated
  columns_to_update <- c('TYPE', 'PATIENT', 'PROFILE', 'IXrange', 'REASON')
  
  # Retrieve the last row's ID for input prefix
  last_id <- slope_manual_NCA_data$id[length(slope_manual_NCA_data$id)]
  
  # Update each specified column with the corresponding input value
  for (col in columns_to_update) {
    input_id <- paste0(col, "_", last_id)
    if (!is.null(input[[input_id]])) {
      slope_manual_NCA_data[nrow(slope_manual_NCA_data), col] <- input[[input_id]]
    }
  }

    # The function returns the object
    return(slope_manual_NCA_data)
}

# TAB: Exclusions


# Add a new exclusion to the UI, exclusion data and the reactive list of exclusion IDs
observeEvent(input$add_excsel, {
  # req(input$submit_analyte)
  # req(input$cyclenca)

  # Make previous rows values instead of input widgets
  slope_manual_NCA_data = update_slope_manual_NCA_data(slope_manual_NCA_data())
    
  # Create an ID for the new row
  rowCounter(rowCounter() + 1)
  id <- paste0("Ex_", rowCounter())

  # Create the new row as a set of input widgets for the UI
  new.row.slope_manual_NCA_data <- data.frame(
    TYPE=as.character(selectInput(
      inputId = paste0("TYPE_",id), label='', width='60%', choices=c('Selection', 'Exclusion'), 
      selected='Selection', selectize=T,
    )),
    PATIENT = as.character(selectInput(
      inputId = paste0("PATIENT_",id), label='', width='60%', choices=names(profiles_per_patient()), selectize=T,
    )),
    PROFILE = as.character(selectInput(
      inputId = paste0("PROFILE_",id), label='', width='60%', choices=unname(unique(unlist(profiles_per_patient()))), selectize=T,
    )),
    IXrange = as.character(textInput(
      inputId = paste0("IXrange_",id), label='', width='70%',
      value = '1:3'
    )),
    REASON = as.character(textInput(
      inputId = paste0("REASON_",id), value='', label='', width='80%'
    )),
    id=id
  )

  
  

  # Bind the row to the reactive value displayed in the UI
  slope_manual_NCA_data(rbind(slope_manual_NCA_data, new.row.slope_manual_NCA_data))
})


# Allow the user to delete the rows selected in the UI table
observeEvent(input$remove_excsel, {
  if(!is.null(input$slope_manual_NCA_data_rows_selected)){

    # Make previous rows values instead of input widgets
    slope_manual_NCA_data = update_slope_manual_NCA_data(slope_manual_NCA_data())

    # Reset the variable deleting the selected rows
    slope_manual_NCA_data(slope_manual_NCA_data[-as.numeric(input$slope_manual_NCA_data_rows_selected),])

  }
})


# SERVER LOGIC: connecting exclusion and manula slope selection to rerun NCA function call


# Save the exclusion/selection data to the server data and rerun the NCA results
observeEvent(input$save_excsel, {
  
    # Update the data in mydata() to reflect the changes in the exclusion/selection table
  mydata = mydata()

  # Reset to 0 all previous (if done) changes
  mydata$conc$data$is.included.hl = FALSE
  mydata$conc$data$is.excluded.hl = FALSE
  mydata$conc$data$exclude_half.life = FALSE

  # If there is no specification there is nothing to save
  if (nrow(slope_manual_NCA_data()) == 0) {
    
    # Rerun the NCA with the modified data
    mydata(mydata)
    rv$trigger <- rv$trigger + 1

    # Stop the observeEvent
    return()
  }

  # Make previous rows values instead of input widgets
  slope_manual_NCA_data = update_slope_manual_NCA_data(slope_manual_NCA_data())


  # Eliminate all rows with conflicting or blank values 
  slope_manual_NCA_data(slope_manual_NCA_data  %>% 
  #mutate(IXmax = unlist(lapply(IXrange, function(x) max(eval(parse(text=x))))))  %>% 
  filter(PATIENT %in% names(profiles_per_patient()),
         PROFILE %in% unname(unlist(profiles_per_patient()[PATIENT])),
         all(!is.na(sapply(IXrange, function(x) eval(parse(text = x))))) & all(!is.null(sapply(IXrange, function(x) eval(parse(text = x))))),
         #paste0(PATIENT, PROFILE, IXmax) %in% paste0(resNCA()$data$conc$data$SUBJID, resNCA()$data$conc$data$DOSNO, resNCA()$data$conc$data$IX)
         #paste0(PATIENT, PROFILE, sapply(IXrange, function(ixs) max(eval(parse(ixs))))) %in% paste0(resNCA()$data$conc$data$SUBJID, resNCA()$data$conc$data$DOSNO, resNCA()$data$conc$data$IX)
         )  %>%
  # Eliminate duplicated records within the same profile
  filter(!duplicated(paste0(PATIENT, PROFILE, IXrange, fromLast=T),
         !(duplicated(paste0(PATIENT, PROFILE), fromLast=T)))
         )
  )
  


  # Update the exclusion/selection data for Lambda based on the current exc/sel table
  for (i in 1:nrow(slope_manual_NCA_data())) {
    #
    
    if(slope_manual_NCA_data()$TYPE[i] == 'Selection'){
      
      mydata$conc$data = mydata$conc$data %>% 
        mutate(is.included.hl = ifelse(USUBJID == slope_manual_NCA_data()$PATIENT[i] & DOSNO == slope_manual_NCA_data()$PROFILE[i] & IX %in% eval(parse(text = slope_manual_NCA_data()$IXrange[i])), TRUE, is.included.hl),
               REASON = ifelse(USUBJID == slope_manual_NCA_data()$PATIENT[i] & DOSNO == slope_manual_NCA_data()$PROFILE[i] & IX %in% eval(parse(text = slope_manual_NCA_data()$IXrange[i])), slope_manual_NCA_data()$REASON[i], REASON))
    } else {
      mydata$conc$data = mydata$conc$data %>% 
        mutate(is.excluded.hl = ifelse(USUBJID == slope_manual_NCA_data()$PATIENT[i] & DOSNO == slope_manual_NCA_data()$PROFILE[i] & IX %in% eval(parse(text = slope_manual_NCA_data()$IXrange[i])), TRUE, is.excluded.hl),
               REASON = ifelse(USUBJID == slope_manual_NCA_data()$PATIENT[i] & DOSNO == slope_manual_NCA_data()$PROFILE[i] & IX %in% eval(parse(text = slope_manual_NCA_data()$IXrange[i])), slope_manual_NCA_data()$REASON[i], REASON))
    }
  }
  mydata$conc$data = mydata$conc$data  %>% 
    group_by(STUDYID, USUBJID, PCSPEC, DOSNO) %>% 
    mutate(exclude_half.life = if(any(is.included.hl)) is.excluded.hl|!is.included.hl else is.excluded.hl)

  mydata(mydata)

  # Rerun the NCA with the modified data
  rv$trigger <- rv$trigger + 1
})


# Define the lambda slope plots for each patient/profile
patient_profile_plotids <- reactiveVal(NULL)
observeEvent(list(input$nca, input$search_patient), {
  req(resNCA())
  
  # Make sure the search_patient input is not NULL
  if (is.null(input$search_patient) | length(input$search_patient)==0) {
    search_patient <- unique(resNCA()$result$USUBJID)
    } else search_patient <- input$search_patient

  # Create a vector with each patient/profile lambda slope plot ID  
  patient_profile_plotids(
    mydata()$conc$data %>%
    filter(DOSNO %in% input$cyclenca, 
           USUBJID %in% search_patient)%>%
    select(USUBJID, DOSNO)%>%
    unique() %>%
    arrange(USUBJID, DOSNO) %>%
    mutate(id = paste0('slopetestplot_', USUBJID, '_', DOSNO))  %>% 
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

plot_outputs=reactiveVal(NULL)
prev_button <- reactiveVal(NULL)
next_button <- reactiveVal(NULL)
page_info <- reactiveVal(NULL)
page_selector <- reactiveVal(NULL)

# Control with the page number, the patient search and the NCA results the plots shown in the App
observeEvent(list(resNCA(), rv$page, patient_profile_plotids(), input$plots_per_page),{
    
  # Get a list with the number of plots you need to display
  num_plots_per_page <- as.numeric(req(input$plots_per_page))
  num_plots <- length(patient_profile_plotids()) 
  rv$num_pages <- ceiling(num_plots/ num_plots_per_page)

  rv$page_per_plot = rep(1:rv$num_pages, each=num_plots_per_page)[1:length(patient_profile_plotids())]

  # Define the UI pagination interface based on page number
  prev_button( if (rv$page > 1) {
    actionButton("prev_page", "Previous Page")  
  } else {
    NULL
  }
  )

  next_button ( if (rv$page < rv$num_pages) {
    actionButton("next_page", "Next Page")
  } else {
    NULL
  }
  )
  page_info (paste("Page", rv$page, "of", rv$num_pages))
  page_selector ( numericInput("page_selector", "Jump to page:", value = rv$page, min = 1, max = rv$num_pages, step = 1) )
    

    # Keep a list outputing the plots that need to be displayed
  plot_outputs(lapply(patient_profile_plotids()[rv$page_per_plot==as.numeric(rv$page)], function(id) {
    plotlyOutput(id)
  })
  )
  # names(plot_outputs) = patient_profile_plotids()[rv$page_per_plot==rv$page]
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
mydata2 = reactiveVal()
observeEvent(mydata(), mydata2(mydata()))

# Define the click events for the point exclusion and selection in the slope plots
click_counter <- reactiveVal(0)
firstclick_vals <- reactiveValues(patient = NULL, profile = NULL, idx_pnt = NULL)

observeEvent(event_data("plotly_click", priority='event'), {

  # Store the information of the last click event
  click_data <- event_data("plotly_click")
  
  if(!is.null(click_data) & !is.null(click_data$customdata)){
    # Get identifiers of the clicked plot
    patient = gsub('(.*)_.*_.*', '\\1',  click_data$customdata)
    profile = gsub('.*_(.*)_.*', '\\1',  click_data$customdata)
    idx_pnt = gsub('.*_.*_(.*)', '\\1',  click_data$customdata)
    
    # Increment the click counter
    click_counter(click_counter() + 1)
    
    
    if(click_counter()%%2==1){
      
      firstclick_vals$patient = patient
      firstclick_vals$profile = profile
      firstclick_vals$idx_pnt = idx_pnt
    }
    
    # When second click happens in the plot an event should occur
    if(click_counter()%%2==0 & click_counter()>0){
      # If the user clicks another plot after one click, reset everything and start over
      if(patient!=firstclick_vals$patient | profile!=firstclick_vals$profile){
        click_counter(1)
        firstclick_vals$patient = patient
        firstclick_vals$profile = profile
        firstclick_vals$idx_pnt = idx_pnt
      
      # If the user clicked in the same plot, perform an action over the temporary data
      } else {
        # Define a temporary data that does not affect the original until is saved by the user (save_excsel)
        mydata2 = mydata2()
        
        

        # Modify the data for the plot according to the user's clicks
        mydata2$conc$data <- mydata2$conc$data %>% 
            # If the user clicked two different points, do their selection
            mutate(
              is.included.hl = case_when(
                idx_pnt == firstclick_vals$idx_pnt ~ is.included.hl,
                USUBJID == patient & DOSNO == profile & IX %in% firstclick_vals$idx_pnt:idx_pnt ~ TRUE,
                TRUE ~ FALSE
                ),
            # If the user clicked two times the same point, do its exclusion
              is.excluded.hl = case_when(
                idx_pnt != firstclick_vals$idx_pnt ~ is.excluded.hl,
                USUBJID == patient & DOSNO == profile & IX %in% idx_pnt ~ !is.excluded.hl,
                TRUE ~ is.excluded.hl
                )
              )  %>% 
            group_by(STUDYID, USUBJID, PCSPEC, DOSNO) %>% 
            mutate(exclude_half.life = if(any(is.included.hl)) is.excluded.hl|!is.included.hl else is.excluded.hl)
        mydata2(mydata2) 
        

        # Change the plot of only the profile and patient selected 
        mydata2$conc$data = mydata2$conc$data %>% filter(USUBJID==patient, DOSNO==profile)
        mydata2$dose$data = mydata2$dose$data %>% filter(USUBJID==patient, DOSNO==profile)
        myres2 = suppressWarnings(PKNCA::pk.nca(data=mydata2, verbose=F))
        
      
        # Alter the output with the transitory changes and the new slope plot
        output[[paste0('slopetestplot_', patient, '_', profile)]] <- renderPlotly({
          lambda_slope_plot(PKNCAres_df = myres2$result,
                            PKNCAconc_df = myres2$data$conc$data,
                            dosno = profile,
                            usubjid = patient,
                            R2ADJTHRESHOL = ifelse(input$rule_adj.r.squared, input$adj.r.squared_threshold, 0.7)
          )
        })
        

        ## Make UI changes in the table displayed
        # 1) If the point selected is a exclusion that was already indicated, then remove previous records from the UI table and stop the observeEvent
          if (idx_pnt == firstclick_vals$idx_pnt && all(!mydata2$conc$data$is.excluded.hl[mydata2$conc$data$USUBJID == patient & 
                                                        mydata2$conc$data$DOSNO == profile & 
                                                        mydata2$conc$data$IX == idx_pnt])) {
          
          
          condition.vr = slope_manual_NCA_data()$PATIENT == patient & 
                         slope_manual_NCA_data()$PROFILE == profile & 
                         sapply(slope_manual_NCA_data()$IXrange, function(x) idx_pnt %in% eval(parse(text = paste0('c(',x,')'))))

          slope_manual_NCA_data <- slope_manual_NCA_data() %>%
                   mutate(IXrange = ifelse(condition.vr,
                                           yes = {
                                                  ixrange <- eval(parse(text = paste0('c(', IXrange, ')')))
                                                  ixrange <- ixrange[ixrange != idx_pnt]
                                                  paste(ixrange, collapse = ',')
                                                  },
                                           no = IXrange
                                          ))  %>% 
                                          # delete all rows where IXrange does not contain a numeric value
                    filter(grepl('\\d.*', IXrange))

          
          

          slope_manual_NCA_data(slope_manual_NCA_data) 
        } else {



        # 2) If the point selected is a selection or a exclusion that was not indicated then include it also in the UI table
        
        rowCounter(rowCounter() + 1)
        id <- paste0("Ex_", rowCounter())

        new.row.slope_manual_NCA_data <- data.frame(
            TYPE= ifelse(idx_pnt != firstclick_vals$idx_pnt, 'Selection', 'Exclusion'),
            PATIENT = patient,
            PROFILE = as.character(profile),
            IXrange = paste0(firstclick_vals$idx_pnt, ':', idx_pnt), 
            REASON = '[Graphical selection. Double click here to include a reason]', 
            id=id
        )
        slope_manual_NCA_data(rbind(slope_manual_NCA_data(), new.row.slope_manual_NCA_data))       
        
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


# observeEvent(input$go2, {     
#   
#   # Generate the filename    
#   filename <- input$filename    
#   
#   # Check if the file extension is provided and add ".csv" if it's not  
#   if (file_ext(filename) != "csv") {  
#     filename <- paste(filename, "csv", sep = ".")  
#   }  
#   
#   # Save the data frame as a CSV with the generated file name    
#   write.csv(summary_stats(), file = filename)
# 
#   #upload new file to improve
# 
#   improveRmodify::createFile(fileName = paste("test",filename),
#                              targetIdent = input$pathresults,
#                              localPath = paste("./", filename, sep = ""))
# 
# })


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
  session$sendCustomMessage(type = "downloadFile", message = list(fileName = input$fileNameInput, filePath = file_path))
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

