# NCA TAB -----
# This module contains nested modules and all the ui and server code for the NCA tab

tab_nca_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    actionButton(ns("nca"), "Run NCA", class = "run-nca-btn"),
    downloadButton(ns("settings_save"), "Save Project Settings"),
    
    navset_tab(id = ns("ncapanel"),
               nav_panel("Setup", fluid = TRUE,
                         
                         navlistPanel(
                           tabPanel("NCA settings", nca_settings_ui(ns("nca_settings"))),
                           tabPanel("Slope Selector", slope_selector_ui(ns("slope_selector")))
                           
                         )
               ),
               nav_panel("Results", fluid = TRUE,
                         navset_pill_list(
                           nca_results_ui(ns("nca_results")),
                           nav_panel(
                             "Slopes",
                             DTOutput(ns("preslopesettings"))
                           ),
                           nav_panel(
                             "Exclusions",
                             tableOutput(ns("manual_slopes2"))
                           ),
                           tabPanel("Parameter Datasets", parameter_datasets_ui(ns("parameter_datasets")))
                         )
               ),
               tabPanel("Additional Analysis", additional_analysis_ui(ns("non_nca")))
    )
  )
  
}

tab_nca_server <- function(id, data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    mydata <- reactiveVal(NULL)
    
    observeEvent(data(), priority = 2, {
      req(data())
      
      group_columns <- intersect(colnames(data()), c("STUDYID", "PCSPEC", "ROUTE", "DRUG"))
      usubjid_column <- "USUBJID"
      time_column <- "AFRLT"
      dosno_column <- "DOSNO"
      route_column <- "ROUTE"
      analyte_column <- "ANALYTE"
      matrix_column <- "PCSPEC"
      
      df_conc <- format_pkncaconc_data(ADNCA = data(),
                                       group_columns = c(group_columns, usubjid_column, analyte_column),
                                       time_column = time_column) %>%
        arrange(across(all_of(c(usubjid_column, time_column))))
      
      df_dose <- format_pkncadose_data(pkncaconc_data = df_conc,
                                       group_columns = c(group_columns, usubjid_column),
                                       time_column = time_column,
                                       dosno_column = dosno_column,
                                       since_lastdose_time_column = "ARRLT")
      
      df_conc$is.excluded.hl <- FALSE
      df_conc$is.included.hl <- FALSE
      df_conc$REASON <- NA
      df_conc$exclude_half.life <- FALSE
      
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
      
      mydata <- PKNCA::PKNCAdata(
        data.conc = myconc,
        data.dose = mydose,
        units = PKNCA::pknca_units_table(
          concu = myconc$data$AVALU[1],
          doseu = myconc$data$DOSEU[1],
          amountu = myconc$data$AVALU[1],
          timeu = myconc$data$RRLTU[1]
        )
      )
      
      unique_analytes <- unique(mydata$conc$data[[mydata$conc$columns$groups$group_analyte]])
      analyte_column <- mydata$conc$columns$groups$group_analyte
      mydata$units <- tidyr::crossing(mydata$units,
                                      !!sym(analyte_column) := unique_analytes)  %>%
        mutate(PPSTRESU = PPORRESU, conversion_factor = 1)
      
      mydata(mydata)
    })
    
    rules <- nca_settings_server("nca_settings", data, mydata, res_nca)
    
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
    
    res_nca <- reactiveVal(NULL)
    observeEvent(input$nca, {
      req(mydata())

      withProgress(message = "Calculating NCA...", value = 0, {
        tryCatch({
          myres <- PKNCA::pk.nca(data = mydata(), verbose = FALSE)
          
          myres$result <- myres$result %>%
            inner_join(select(mydata()$dose$data, -exclude)) %>%
            mutate(start = start - !!sym(mydata()$dose$columns$time),
                   end = end - !!sym(mydata()$dose$columns$time)) %>%
            select(names(myres$result))
          
          res_nca(myres)
          updateTabsetPanel(session, "ncapanel", selected = "Results")
          
        }, error = function(e) {
          full_error <- e$parent$message
          if (grepl("pk.calc.", x = full_error)) {
            param_of_error <- gsub(".*'pk\\.calc\\.(.*)'.*", "\\1", full_error)
            full_error <- paste0("Problem in calculation of NCA parameter: ", param_of_error,
                                 "<br><br>", full_error)
          }
          modified_error <- gsub("Please report a bug.\n:", "", x = full_error, fixed = TRUE) %>%
            paste0("<br><br>If the error is unexpected, please report a bug.")
          showNotification(HTML(modified_error), type = "error", duration = NULL)
        })
      })
    })
    
    nca_results_server("nca_results", res_nca, rules(), grouping_vars)
    
    slope_rules <- slope_selector_server(
      "slope_selector",
      mydata,
      res_nca,
      profiles_per_patient,
      pk_nca_trigger,
      reactive(input$settings_upload)
    )
    
    output$preslopesettings <- DT::renderDataTable({
      pivot_wider_pknca_results(res_nca()) %>%
        select(
          any_of(c("USUBJID", "DOSNO", "ANALYTE", "PCSPEC")),
          starts_with("lambda.z"),
          starts_with("span.ratio"),
          starts_with("half.life"),
          "Exclude"
        ) %>%
        DT::datatable(
          extensions = "FixedHeader",
          options = list(scrollX = TRUE, scrollY = TRUE, lengthMenu = list(c(10, 25, -1), c("10", "25", "All")), pageLength = -1, fixedHeader = TRUE)
        ) %>%
        formatStyle("Exclude", target = "row", backgroundColor = styleEqual(NA, NA, default = "#f5b4b4"))
    })
    
    output$manual_slopes2 <- renderTable({
      slope_rules()
    })
    
    additional_analysis_server("non_nca", mydata, grouping_vars)
    parameter_datasets_server("parameter_datasets", res_nca)
    
    res_nca
  })
}

    
   