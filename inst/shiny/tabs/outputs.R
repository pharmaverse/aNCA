# SERVER LOGIC OF NAVBAR OUTPUT TAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# In the output tab of the navbar, dynamic graphics of the input
# as well as the results of the NCA analysis are displayed. The user can dynamically
# display graphics and summaries of these data.

# TABSET: General Individual Plots =============================================
#
# This tabset plots the input data generally in a dynamic lineplot and meanplot


# TAB: General Lineplot --------------------------------------------------------

#  select the analyte for the general lineplot
output$generalplot_analyte <- renderUI({
  # deselect choices that are no pp parameters
  param_choices <- data() %>%
    pull(ANALYTE) %>%
    unique()

  pickerInput("generalplot_analyte", "Select Analyte:",
              choices = param_choices,
              selected = param_choices[1],
              multiple = FALSE,
              options = list(`actions-box` = TRUE))

})

# select the usubjid for the general lineplot
output$generalplot_usubjid <- renderUI({
  # deselect choices that are no pp parameters
  param_choices <- data() %>%
    pull(USUBJID) %>%
    unique()

  pickerInput("generalplot_usubjid", "Select Subjects:",
              choices = param_choices,
              selected = param_choices[1],
              multiple = TRUE,
              options = list(`actions-box` = TRUE))
})

# select which variable to color the general lineplot by
output$generalplot_colorby <- renderUI({
  # deselect choices that are no pp parameters
  param_choices <- c("STUDYID", "PCSPEC", "ANALYTE", "USUBJID", "DOSEA", "DOSNO")
  pickerInput("generalplot_colorby", "Choose the variables to color by",
              choices = param_choices,
              selected = param_choices[1],
              multiple = FALSE,
              options = list(`actions-box` = TRUE))
})

# select the dose number/cycle for the general lineplot, if plotting by cycle is chosen
output$cycleselect <- renderUI({
  req(input$generalplot_analyte)

  y = data() %>%
    filter(ANALYTE == input$generalplot_analyte) %>%
    pull(DOSNO) %>%
    unique()
  selectInput("cycles", "Choose the cycle :", choices = sort(y))

})

# render the general lineplot output in plotly
output$individualplot <- renderPlotly({
  req(data())
  req(input$generalplot_analyte)
  req(input$generalplot_usubjid)
  req(input$generalplot_colorby)
  req(input$timescale)
  req(input$log)

  general_lineplot(data(),
                       input$generalplot_analyte,
                       input$generalplot_usubjid,
                       input$generalplot_colorby,
                       input$timescale,
                       input$log,
                       cycle = input$cycles)


})




# TAB: Mean Plot ---------------------------------------------------------------

# This tabs plots the mean concentration of the input data in a dynamic plot

# select the analyte for the mean plot
output$analytemean <- renderUI({
  y = data() %>%
    pull(ANALYTE) %>%
    unique()
  selectInput("analytemean", "Choose the Analyte:", choices = sort(y), selected = y[1])
})

# select the study id for the mean plot (needed for preclinical data, where multiple studyids per dataset)
output$studyidmean <- renderUI({
  y = data() %>%
    pull(STUDYID) %>%
    unique()
  selectInput("studyidmean", "Choose the Study ID:", choices = sort(y))
})

# select the variable to calculate the mean by
output$selectidvar <- renderUI({
  y = c("PCSPEC", "DOSEA", "TRT01A", "TRT01P")
  selectInput("selectidvar", "Choose the variable to group by:",
              choices = y, selected = "DOSEA")
})

# select the cycle to plot the mean concentrations
output$cyclemean <- renderUI({
  y = data() %>%
    filter(ANALYTE %in% input$analytemean)%>%
    pull(DOSNO) %>%
    unique()
  selectInput("cyclesmean", "Choose the cycle:", choices = sort(y))
})

# render the meanplot output in plotly
output$meanplot <- renderPlotly({

  req(input$studyidmean)
  req(input$analytemean)
  req(input$cyclesmean)

  validate(
    need(data() %>% 
                filter(STUDYID %in% input$studyidmean,
                       ANALYTE %in% input$analytemean,
                       DOSNO %in% input$cyclesmean,
                       if ('EVID' %in% names(data)) EVID == 0 else T,
                       NRRLT>0
                ) %>% 
                group_by(!!sym(input$selectidvar), NRRLT) %>% 
                summarise(N=n()) %>% 
                filter(N>=3) %>%
                nrow(.)>0,
        
         message = paste0('Data issue: No data with more than 3 points to calculate average based on nominal time (NRRLT) and selected variable: ', input$selectidvar)
         )
  )
  
  general_meanplot(data = data(),
                   selected_studyids = input$studyidmean,
                   selected_analytes = input$analytemean,
                   selected_cycles = input$cyclesmean,
                   id_variable = input$selectidvar,
                   plot_ylog = input$logmeanplot,
                   plot_sd = input$sdmeanplot) %>%
    plotly_build()

})


# TABSET: Dose Escalation Outputs ==============================================

# This tabset computes and visualizes output data from the NCA analysis for dose
# escalation meetings. The user can view summary statistics, individual and mean
# as well as boxplots of the calculated NCA parameters.


# TAB: Descriptive Statistics --------------------------------------------------


# pickerInput to filter for parameters to display in the summary table
output$summaryselect <- renderUI({
  req(resNCA())

  # available parameters
  paramselection <- unique(resNCA()$result$PPTESTCD)
  # select from available with all perselected
  pickerInput("paramselect", "Filter parameters to display:",
              choices = paramselection,
              selected = paramselection,
              multiple = TRUE,
              options = list(`actions-box` = TRUE))
})

# Update inputs based on what is avaialble in the data
observeEvent(resNCA(), {
  # Define the relevant columns for the groupby picker
  group_cols = unname(unlist(resNCA()$data$conc$columns$groups))
  classification_cols = sort(c('SEX','RACE','ACTARM','AGE','TRT01P','TRT01A', 'DOSEA'))
  classification_cols = classification_cols[classification_cols %in% names(resNCA()$data$conc$data)]

  # update the input for the groupby picker
  updateOrderInput(session, 'summarygroupbysource', items=c(group_cols, classification_cols) )
})

# Reactive expression for summary table based on selected group and parameters
summary_stats <- reactive({
  req(input$summarygroupby, input$paramselect)

  # Calculate summary stats and filter by selected parameters
  calculate_summary_stats(resNCA(), input$summarygroupby) %>%
    filter(PPTESTCD %in% input$paramselect)  %>%
    rename(PARAM=PPTESTCD)
})

# render the reactive summary table in a datatable
output$descriptivestats <- DT::renderDataTable({
  req(summary_stats())
  DT::datatable(data = summary_stats(),
                options = list(scrollX = TRUE,
                               scrollY = TRUE,
                               lengthMenu = list(c(10, 25, -1), c('10', '25', 'All'))
                )
  )
})

output$descriptivestats2 <- DT::renderDataTable({
  req(resNCA())
  DT::datatable(data = calculate_summary_stats(resNCA())  %>% rename(PARAM=PPTESTCD),
                options = list(scrollX = TRUE,
                               scrollY = TRUE,
                               lengthMenu = list(c(10, 25, -1), c('10', '25', 'All'))
                )
  )
})



# TAB: Mean Concentration over Time --------------------------------------------

# preprocess data for plotting mean concentration over time
mean_data = reactive({

  data() %>% # mydata()$conc$data %>%
    filter(ANALYTE == input$analyte,
           DOSNO %in% input$cyclenca)%>%
    mutate(DOSEA = as.factor(DOSEA),
           TRT = as.factor(NOMDOSE),
           TIME = ifelse(DOSNO == 1, AFRLT, AFRLT),
           NOMTIME = ifelse(DOSNO == 1, NFRLT, NRRLT))%>%
    group_by(DOSEA, NOMTIME, DOSNO)%>%
    summarise(Mean = geometric.mean(AVAL, na.rm = T),
              SD = sd(AVAL, na.rm = T),
              N = n())%>%
    filter(N >= 3)

})


######################################### same plots as in general plotting?
doseescalation_meanplot <- function() {

  dataset <- mydata()$conc$data %>%
    filter(ANALYTE == input$analyte,
           DOSNO %in% input$cyclenca)

  time_label = paste0('Nominal Time [', unique(dataset$RRLTU), "]")
  trtact_label = 'Dose Group'
  conc_units = paste0(unique(dataset$AVALU))
  dose_units = paste0(unique(dataset$DOSEU))
  conc_label = paste0('Mean Concentration [', conc_units, "/", dose_units, "]")

  ggplot(data = mean_data(), aes(x = NOMTIME, y = Mean, ymin = (Mean - SD), ymax = (Mean + SD), group= DOSEA, color = DOSEA))+
    geom_errorbar(width = 0.4)+
    geom_point()+
    geom_line()+
    labs(x = time_label, y=conc_label, color = trtact_label)

}

output$mean_concovertime <- renderPlotly(
  doseescalation_meanplot()
)

output$mean_concovertimelog <- renderPlotly(
  doseescalation_meanplot()+
    xgx_scale_y_log10()
)



# TAB  Dose Norm Conc over Time Plots ----

plot_data = reactive({
  req(input$analyte)

  data() %>%  # mydata()$conc$data %>%
    filter(ANALYTE == input$analyte,
           DOSNO %in% input$cyclenca)%>%
    select(AFRLT, AVAL, DOSEA, DOSNO, AFRLT, NFRLT, NRRLT, USUBJID, ANALYTE, STUDYID, AVALU, RRLTU, DOSEU, NOMDOSE)%>%
    mutate(CONC_NORM = AVAL/DOSEA,
           TRT = as.factor(NOMDOSE),
           TIME = ifelse(DOSNO == 1, AFRLT, AFRLT),
           NOMTIME = ifelse(DOSNO == 1, NFRLT, NRRLT))%>%
    na.omit()


})

normconcplot <- function() {

  dataset = plot_data()
  time_label = paste0('Nominal Time [', unique(dataset$RRLTU), "]")
  trtact_label = 'Dose Group'
  conc_units = paste0(unique(dataset$AVALU))
  dose_units = paste0(unique(dataset$DOSEU))
  concnorm_label = paste0('Normalised Concentration [', conc_units, "/", dose_units, "]")

  ggplot(data = dataset,
         aes(x = NOMTIME, y = CONC_NORM,
             group = USUBJID, color = TRT))+
    geom_point(aes(color = TRT), size = 2, alpha = 0.5)+
    geom_line(aes(group= USUBJID, color=TRT), size = 1, alpha = 0.5)+
    facet_wrap(~DOSNO)+
    labs(y=concnorm_label, x = time_label, color = trtact_label)

}

output$norm_concovertime <- renderPlotly(

  normconcplot()

  )

output$norm_concovertimesemilog <- renderPlotly(

  normconcplot()+
    xgx_scale_y_log10()
)

################################################################################

# TAB: Parameter Boxplots ----------------------------------------------------

# Create formatted Boxplot data: PKNCAconc + PP results, linking DOSEA + PPTESTCD
boxplotdata <- reactive({
  group_columns = unname(unlist(resNCA()$data$conc$columns$groups))
  
  left_join(
    resNCA()$result %>% filter(end==Inf | startsWith(PPTESTCD, 'aucint')),
    resNCA()$data$conc$data %>% distinct(across(all_of(group_columns)), 
                                         .keep_all = T), 
    by=group_columns,
    keep = F
  ) %>% 
    # Intervals should also be considered as differentiated options each
    mutate(PPTESTCD = ifelse(startsWith(PPTESTCD,'aucint'), 
                             paste0(PPTESTCD, '_', start, '-', end), 
                             PPTESTCD ) )
  
})

# select which parameter to box or violin plot
output$selectboxplot <- renderUI({
  
  param_choices <- boxplotdata()$PPTESTCD %>% unique()
  
  pickerInput("selected_param_boxplot", "Choose the parameter to display:",
              choices = param_choices,
              selected = param_choices[1],
              multiple = FALSE,
              options = list(`actions-box` = TRUE))

})




# Selector for the variables to segregate boxplots in the x axis 
output$select_xvars_boxplot <- renderUI({
  
  pickerInput(inputId = 'selected_xvars_boxplot', 
              label = 'Select X grouping variables',
              multiple = T,
              choices = intersect(names(boxplotdata()), names(data())),
              selected = 'DOSEA' 
              )
})


output$select_colorvars_boxplot <- renderUI({
  
  pickerInput(inputId = 'selected_colorvars_boxplot', 
              label = 'Select coloring variables to differentiate boxplots',
              multiple = T,
              choices = intersect(names(boxplotdata()), names(data())),
              selected = 'DOSNO' 
  )
})


observeEvent(list(input$selected_xvars_boxplot, input$selected_colorvars_boxplot),{
  
  xvar_options_list = lapply(c(input$selected_xvars_boxplot, 
                               input$selected_colorvars_boxplot), 
                             \(id_var) paste(id_var, unique(boxplotdata()[[id_var]]), sep = ': '))
  
  names(xvar_options_list) = c(input$selected_xvars_boxplot, 
                               input$selected_colorvars_boxplot)
  
  updatePickerInput(session = session, 
                    inputId = 'selected_varvalstofilter_boxplot', 
                    label = 'Select values to display for grouping',
                    choices = xvar_options_list, 
                    selected = unlist(xvar_options_list)
  )
}
)


# toggle between boxplot and violinplot
output$violin_toggle <- renderUI({
  switchInput(
    inputId = "violinplot_toggle_switch",
    label = "",
    value = TRUE,
    onLabel = "Boxplot",
    offLabel = "Violinplot"
  )
  })

# compute the boxplot
output$boxplot <- renderPlotly({
  
  req(boxplotdata())
  req(input$selected_param_boxplot)
  req(input$selected_xvars_boxplot)
  req(input$selected_colorvars_boxplot)
  req(input$selected_varvalstofilter_boxplot)
  
  flexible_violinboxplot(boxplotdata = boxplotdata(),
                         parameter = input$selected_param_boxplot,
                         xvars = input$selected_xvars_boxplot,
                         colorvars = input$selected_colorvars_boxplot,
                         varvalstofilter = input$selected_varvalstofilter_boxplot,
                         columns_to_hover = unname(unlist(resNCA()$data$conc$columns$groups)),
                         box = input$violinplot_toggle_switch) 

})

# create a download button for CDISC dataframes
# output$exportCDISC <- downloadHandler(
#   filename = function() {
#     paste("CDISC_", Sys.Date(), ".zip", sep = "")
#   },
#   content = function(file) {

#     # Export the list of dataframes to a zip file containing CSVs
#     rio::export_list(x = exportCDISC(resNCA()),
#                      file = paste("%s_", Sys.Date(), ".csv", sep = ""), # filename from above is somehow not passed here?
#                      archive = paste("CDISC_", Sys.Date(), ".zip", sep = ""))
#   }
# )


# CDISC ------------------------------------------------------------------------

# export pp and adpp as zip file
output$exportCDISC <- downloadHandler(
  filename = function() {
    paste("CDISC_", Sys.Date(), ".zip", sep = "")
  },
  content = function(file) {
    # Create a temporary directory to store the CSV files
    temp_dir <- tempdir()

    CDISC <- exportCDISC(resNCA())
    # Export the list of data frames to CSV files in the temporary directory
    file_paths <- rio::export_list(
      x = CDISC,
      file = file.path(temp_dir, paste0(names(CDISC), "_", Sys.Date(), ".csv"))
    )

    # Create a ZIP file containing the CSV files
    zip::zipr(zipfile = file, files = file_paths)
  }
)

# EXPORT Report ----------------------------------------------------------------

# ATTENTION: most of the blocks in the RMD are still eval = FALSE, as
# feature is under development and only implemented as preview for idea of
# exporting reports

# templates from: https://opensource.nibr.com/xgx/

# concat all dataframes into one object
concat_report_data <- reactive({
  data <- resNCA()
  data$formatted_res <- boxplotdata()
  data
})

# render the markdown upon clicking generate
rendered_rmd <- reactiveVal(NULL)

observeEvent(input$generate_report, {
  # specifying the temp location, params and report to be rendered
  output_file <- file.path(tempdir(), "report.html")
  params <- list(resNCA = concat_report_data())
  report_path <- system.file(paste0("shiny/www/rmd/",input$report_studytype , "_report.Rmd"), package = "aNCA")
  # render the report in new env
  rmarkdown::render(report_path,
                    output_file = output_file,
                    params = params,
                    # render in seperate environment to ensure params pass!
                    envir = new.env(parent = globalenv()))
  rendered_rmd(output_file)
})

# Display the HTML content in the Shiny app
output$rmd_content <- renderUI({
  req(rendered_rmd())
  html_content <- readLines(rendered_rmd(), warn = FALSE)
  HTML(paste(html_content, collapse = "\n"))
})

# Handle the download request
output$download_rmd <- downloadHandler(
  filename = function() {
    paste("report/SD_report", "html", sep = ".")
  },
  content = function(file) {

    file.copy(rendered_rmd(), file)
  },
  contentType = "text/html"
)

# # Download handler for the PDF report
# output$downloadReport <- downloadHandler(
#   filename = function() {
#     paste("report", Sys.Date(), ".pdf", sep = "")
#   },
#   content = function(file) {
#     # Copy the report template to a temporary directory
#     tempReport <- file.path(tempdir(), "report.Rmd")
#     file.copy("report/report.Rmd", tempReport, overwrite = TRUE)

#     # Set up parameters to pass to Rmd document
#     params <- list(
#       resNCA = concat_report_data()
#     )
#
#     # Knit the document
#     rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
#   }
# )




