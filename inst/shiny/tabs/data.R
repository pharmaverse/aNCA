# SERVER LOGIC OF NAVBAR DATA TAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The data navbar tab loads a dummy data set by default.
# The user can upload a new data set by clicking the "Browse" button.
# The data set must be in CSV format and contain the following columns:
# STUDYID, USUBJID, ANALYTE, PCSPEC, DOSEFRQ, DOSNO, AFRLT, ARRLT, NRRLT, NFRLT,
# AVAL, AVALU, ROUTE, DOSEA, AGE

# "Improve" link functionality ----

# ADNCA <- reactive({
#
#   req(input$signin)
#
#   improveRget::getFile(input$path,addAsLink=F)
#   df <- improveRget::getCopy(ident = input$path)
#
#   read.csv(df$name, na.strings = c("", "NA"))
#
# })


# DATA LOADING -----------------------------------------------------------------
# Load local data (or dummy data otherwise)
# Load the dummy ADNCA example for the user
ADNCA = reactiveVal(read.csv(system.file("shiny/data/DummyRO_ADNCA.csv", package = "aNCA"), na.strings = c("", "NA"))  %>%
                        # Make sure PCSTRESC is numeric and TIME is derived from the first dose
                        mutate(TIME = ifelse(DOSNO == 1, AFRLT, ARRLT))
)


# Custom clinical file upload
observeEvent(input$local_upload, {

  ADNCA =  switch(file_ext(input$local_upload$name),
                  csv = read.csv(input$local_upload$datapath, na = c("", "NA")),
                  rds = readRDS(input$local_upload$datapath),
           validate('Invalid file type. Only accepted are .csv and .rds')
                  )

  # Make sure the dataset includes all the neded column names
  # req_cols <- c("STUDYID", "USUBJID", "ANALYTE", "PCSPEC", "DOSEFRQ", 'DOSNO', "AFRLT", "ARRLT", "NRRLT", "NFRLT", "AVAL",
  #               "AVALU", "ROUTE", "DOSEA", "AGE", "SEX", "RACE", "NDOSEDUR", "RRLTU", "DOSEU", "PCLLOQ", "DRUG", "AVAL", "AVALU", "DOSEU", "EVID")
  # missing_cols = req_cols[!req_cols %in% names(ADNCA)]
  # if (length(missing_cols)>0){
  #   showNotification(ui=paste('File is missing the next mandatory column names:\n\n',
  #                    paste(missing_cols, collapse=", ")), type = "error", duration=NULL)
  # }

    # Disconsider events that do not contain drug information (i.e, Follow-up visits)
    ADNCA <- ADNCA %>%
      filter(if ('AVISIT' %in% names(ADNCA)) tolower(gsub('[^a-zA-Z]','', AVISIT))!='followup' else T)

    # Make sure AVAL is numeric and TIME is derived from the first dose
    ADNCA <- ADNCA  %>%
        mutate(AVAL = if('AVAL' %in% names(ADNCA)) as.numeric(AVAL) 
                      else ifelse(PCSTRESC %in% c('BLQ', 'Negative', 'negative', 'NEGATIVE'), 0, as.numeric(PCSTRESC)) 
              ) %>%
              mutate(TIME = ifelse(DOSNO == 1, AFRLT, ARRLT),
                     NDOSEDUR = as.numeric(NDOSEDUR),
                     ADOSEDUR = as.numeric(ADOSEDUR))

    # browser()
    ADNCA(ADNCA)
})

# Custom preclinical file upload
observeEvent(input$preclinical_upload, {
  ADNCA(read.csv(input$preclinical_upload$datapath, na.strings = c("", "NA")))
  filters
})


# APPLY FILTERS ----------------------------------------------------------------


# Initialize a reactiveValues object to store filters
filters <- reactiveValues()

# Observe the "Add Filter" button click
observeEvent(input$add_filter, {
  # Create a unique ID for each filter
  filter_id <- paste0("filter_", input$add_filter)


  # Insert a new filter UI
  insertUI(
    selector = "#filters",
    ui = create_filter(filter_id, ADNCA())

  )

  # Observe the "Remove Filter" button click for the newly created filter
  observeEvent(input[[paste0(filter_id, "_remove")]], {

    filters[[filter_id]] <- NULL
    # Remove the filter UI
    removeUI(selector = paste0("#", filter_id))
    # Remove the filter from the reactiveValues object
    filters[[filter_id]] <- NULL
  }, ignoreInit = TRUE, once = TRUE)

  observe({
    column <- input[[paste0(filter_id, "_column")]]
    condition <- input[[paste0(filter_id, "_condition")]]
    value <- input[[paste0(filter_id, "_value")]]

    if ((input[[paste0(filter_id, "_confirm")]] == TRUE) && !is.null(column) && !is.null(condition) && !is.null(value)) {
      filters[[filter_id]] <- list(column = column, condition = condition, value = value)
    } else {
      filters[[filter_id]] <- NULL
    }
  })
})





# Include keyboard limits for the settings GUI display
## Define a function that simplifies the action
input_limit = function(input_id, max=Inf, min=-Inf, update_fun=updateNumericInput){
  observeEvent(input[[input_id]],{
    if(input[[input_id]]<min & !is.na(input[[input_id]])){update_fun(session,input_id,"", value=min)}
    if(input[[input_id]]>max & !is.na(input[[input_id]])){update_fun(session,input_id,"", value=max)}
  })
}

## Keyboard limits for the setting thresholds
input_limit('adj.r.squared_threshold', max = 1, min = 0)
input_limit('aucpext.obs_threshold', max=100, min=0)
input_limit('aucpext.pred_threshold', max=100, min=0)
input_limit('span.ratio_threshold', min=0)

## Keyboard limits for the dynamically created partial AUC ranges
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

# create reactive value with applied filters
data <- reactive({apply_filters(ADNCA(), filters)})

# update the data table object with the filtered data
output$filecontents <- renderDataTable({
  req(data())
  DT::datatable(data(),
                options = list(scrollX = TRUE,
                               scrollY = TRUE,
                               lengthMenu = list(c(10, 25, -1), c('10', '25', 'All'))
                )
  )
})

# Update analyte selection input based on the data
observeEvent(data(), updateSelectInput(session, inputId = 'analyte',
                                       label = 'Choose the analyte :',
                                       choices = unique(data()$ANALYTE)))

# When an analyte is selected and the user clicks the "Submit" button, create the PKNCA data object
mydata = reactiveVal(NULL)
observeEvent(input$submit_analyte, {

  # Segregate the data into concentration and dose records
  df_conc <-create_conc(data(), input$analyte, input$proftype)
  df_dose <- create_dose(df_conc)

# Define initially a inclusions/exclusions for lambda slope estimation (with no input)
  df_conc$is.excluded.hl = FALSE
  df_conc$is.included.hl = FALSE
  df_conc$REASON = NA  # Exclusions will have preferential reason statements than inclusions
  df_conc$exclude_half.life = FALSE

  # Make the PKNCA concentration and dose objects
  myconc <- PKNCA::PKNCAconc(df_conc,
                      formula = AVAL ~ TIME | STUDYID + PCSPEC + ANALYTE + USUBJID/DOSNO,
                      exclude_half.life='exclude_half.life',
                      time.nominal = 'NFRLT')

  mydose <- PKNCA::PKNCAdose(data = df_dose,
                      formula = DOSEA ~ TIME | STUDYID + PCSPEC + ANALYTE + USUBJID + DOSNO,
                      route = ifelse(toupper(df_dose$IQROUTE) == 'EXTRAVASCULAR', 'extravascular', 'intravascular'),
                      time.nominal = 'NFRLT',
                      duration = 'NDOSEDUR')

  # Combine the PKNCA objects into the PKNCAdata object
  # TODO think of case with different units for different analytes
  mydata <- PKNCA::PKNCAdata(data.conc = myconc,
                      data.dose = mydose,
                      units = PKNCA::pknca_units_table(concu = myconc$data$PCSTRESU[1],
                                                       doseu = myconc$data$DOSEU[1],
                                                       amountu = myconc$data$PCSTRESU[1],
                                                       timeu = myconc$data$RRLTU[1]))
  mydata(mydata)
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

# UPLOAD PRECINICAL

# Custom preclinical file upload

observeEvent(input$preclinical_upload, {
  # Switch to the 'NCA' > 'Setup' tab
  updateNavbarPage(session, "page", selected = "NCA")
  updateTabsetPanel(session, "ncapanel", selected = "Setup")
})



