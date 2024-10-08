# Define UI
shinyUI(fluidPage(

  tags$script("
    Shiny.addCustomMessageHandler('update', function(value) {
    Shiny.setInputValue('update', value);
    });
  "),

  tags$script("
    Shiny.addCustomMessageHandler('increment', function(value) {
      var newValue = value + 1;
      Shiny.setInputValue('update', newValue, {priority: 'event'});
    });
  "),

  # Add custom CSS for progress bar
  tags$style(HTML("
    .shiny-notification {
      position: fixed;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      z-index: 10000;
    }
  ")),

  navbarPage(
    id ='page',
    title = "",

    # DATA ----
    tabPanel("Data", fluid = TRUE,

             sidebarLayout(
               sidebarPanel(

                 actionButton("login", "Login and Select File Path"),
                 br(), br(), br(),

                 # Local upload option
                 fileInput('local_upload',
                           width = '60%',
                           label = NULL,
                           placeholder = 'CSV rds',
                           buttonLabel = list(icon('folder'),'Upload File...'),
                           accept=c('.csv','.rds')),

                 tags$head(
                   tags$style(HTML('.input-group-btn {
                                   width: auto;}'

                   ))),

                 br(),

                 # Add filter UI elements
                 actionButton("add_filter", "Add Filter"),
                 tags$div(id = "filters"),

                 br(),
                 br(),
     

               ),
               mainPanel(
                 DTOutput("filecontents")
               )
             )
    ),

    # NCA ----

    tabPanel("NCA", fluid = TRUE,

             fluidPage(
               tags$head(tags$style(HTML("
                          .run-nca-btn {
                            position: absolute;
                            right: 100px;
                            top: 65px;
                            z-index: 100;
                            }
                                         "))),

               actionButton("nca", "Run NCA", class = "run-nca-btn"),


             tabsetPanel(id = "ncapanel",
               tabPanel("Setup", fluid = TRUE,

                        navlistPanel(
                          tabPanel("Data Selection",

                           # Local upload option
                           fileInput('settings_upload',
                                     width = '60%',
                                     label = 'Upload Settings',
                                     buttonLabel = list(icon('folder'),'Browse'),
                                     accept=c('.csv','.xpt')),

                            # Selection of analyte
                            selectInput("analyte", "Choose the analyte :", choices=NULL),

                            br(),
                            actionButton("submit_analyte", "Submit"),
                            DTOutput("datatable"),
                          ),

                          tabPanel("Settings",


                                   selectInput('cyclenca', "Choose the Dose Number:", multiple=T,
                                               choices = c('Please specify ANALYTE in Data Selection'='')  ),

                                   selectInput("method", "Extrapolation Method:", choices = c("lin-log", "lin up/log down",
                                                                                              "linear"),
                                               selected = "lin up/log down"),

                                   br(),

                                   checkboxInput('AUCoptions', "Select Partial AUC"),
                                   conditionalPanel(
                                     condition = 'input.AUCoptions == true',
                                     fluidRow(
                                       column(
                                         width = 12,
                                         actionButton("addAUC", "+"),
                                         actionButton("removeAUC", "-")
                                       )
                                     ),
                                     tags$div(id = "AUCInputs") # Container for dynamic partial AUC inputs
                                   ),

                                   h4("Flag Rule Sets:"),

                                   fluidRow(
                                     column(
                                       width = 6,
                                       checkboxInput("rule_adj.r.squared", "RSQADJ:")
                                     ),
                                     column(
                                       width = 6,
                                       conditionalPanel(
                                         condition = 'input.rule_adj.r.squared == true',
                                         div(
                                           style = "display: flex; align-items: center;",
                                           span(">=", style = "margin-right: 5px;"),
                                           tags$style(HTML("
                                .shiny-input-container {
                                  display: inline-block;
                                }
                              ")),
                                           numericInput("adj.r.squared_threshold", "", value = 0.7, step=0.05, min = 0, max = 1)
                                         )
                                       )
                                     )

                                   ),


                                  fluidRow(
                                     column(
                                       width = 6,
                                       checkboxInput("rule_aucpext.obs", "AUCPEO (% ext.observed): ")
                                     ),
                                     column(
                                       width = 6,
                                       conditionalPanel(
                                         condition = 'input.rule_aucpext.obs == true',
                                         div(
                                           style = "display: flex; align-items: center;",
                                           span(">=", style = "margin-right: 5px;"),
                                           tags$style(HTML("
                                .shiny-input-container {
                                  display: inline-block;
                                }
                              ")),
                                           numericInput("aucpext.obs_threshold", "", value = 20, step=1, min = 0, max = 100)
                                         )
                                       )
                                     )

                                   ),


                                   fluidRow(
                                     column(
                                       width = 6,
                                       checkboxInput("rule_aucpext.pred", "AUCPEP (% ext.predicted): "),
                                     ),
                                     column(
                                       width = 6,
                                       conditionalPanel(
                                         condition = 'input.rule_aucpext.pred == true',
                                         div(
                                           style = "display: flex; align-items: center;",
                                           span(">=", style = "margin-right: 5px;"),
                                           tags$style(HTML("
                                .shiny-input-container {
                                  display: inline-block;
                                }
                              ")),
                                           numericInput("aucpext.pred_threshold", "", value = 20, step=5, min=0, max=100)
                                         )
                                       )
                                     )

                                   ),




                                   fluidRow(
                                     column(
                                       width = 6,
                                       checkboxInput("rule_span.ratio", "SPAN: "),
                                     ),
                                     column(
                                       width = 6,
                                       conditionalPanel(
                                         condition = 'input.rule_span.ratio == true',
                                         div(
                                           style = "display: flex; align-items: center;",
                                           span(">=", style = "margin-right: 5px;"),
                                           tags$style(HTML("
                                .shiny-input-container {
                                  display: inline-block;
                                }
                              ")),
                                           numericInput("span.ratio_threshold", "", value = 2, step=1, min=0)
                                         )
                                       )
                                     )

                                   )


                          ),

                          tabPanel("Slope Selector",
                                   fluidRow(
                                                                         column(
                                       width = 3,
                                       dropdown(
                                         actionButton("add_excsel", "+ Exclusion/Selection"),
                                         actionButton('remove_excsel', '- Remove selected rows'),
                                         DTOutput('slope_manual_NCA_data'),
                                         actionButton("save_excsel", "Save"),

                                         style = "unite", icon = icon("chart-line"),
                                         status = "success", width = "500px",
                                       )
                                     ),
                                     column(
                                       width = 4,
                                       selectInput("plots_per_page", "Plots per page:", choices = c(2, 4, 6, 8, 10), selected = 2, width = "50%"),
                                     ),
                                     column(
                                       width = 5,
                                       pickerInput("search_patient", label = "Search Patient", width='70%',choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
                                      #  actionButton("search_button", "Search")

                                     )
                                   ),
                                   # Commented help buton until future solution with working directory in package
                                   # tags$div(title = "Click for help",
                                   #          actionButton("slope_helpIcon", label = "", icon = icon("question-circle"))
                                   #          ),
                                   uiOutput("slopetestUI"),
                                                                     # Include details for modal message in slope_helpIcon (Instruction details)
  tags$head(
    tags$style(HTML("
      .modal-dialog {
        max-width: 800px;
      }
      .modal-header {
        border-bottom: 1px solid #e5e5e5;
        background-color: #3f8abf; /* Blue header background */
        text-align: center;
      }
      .modal-title {
        font-family: 'Arial', sans-serif;
        font-size: 32px; /* Larger title font size */
        font-weight: bold;
        color: #ffffff; /* White title color */
        margin-bottom: 10px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .modal-body {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        font-family: 'Arial', sans-serif;
        font-size: 16px;
        background-color: #f5f5f5; /* Light grey body background */
        padding: 20px;
        text-align: center;
      }
      .modal-intro {
        font-family: 'Georgia', serif; /* Elegant font type for intro text */
        font-size: 18px;
        margin-bottom: 20px;
        color: #333; /* Dark text color */
      }
      .gif-container {
        position: relative;
        margin: 20px;
        text-align: center;
        width: 220px;
        height: 220px; /* Ensure the container has a fixed height */
        padding: 15px;
        border-radius: 8px;
        box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
        transition: transform 0.3s ease-in-out;
        background-color: #ffffff; /* White container background */
      }
      .gif-container:hover {
        transform: scale(1.05);
      }
      .gif-container img {
        width: 120%;
        height: 120%;
        margin: 0px;
        object-fit: cover;
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 5px;
      }
      .gif-title {
        margin-bottom: 0px;
        margin-top: 0px;
        font-weight: bold;
        font-size: 24px; /* Larger font size for titles */
        color: #0056b3; /* More vibrant color */
        text-align: center; /* Center the title */
        font-family: 'Arial', sans-serif; /* Use a more attractive font */
        text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.1); /* Add a subtle text shadow */
      }

      .btn-primary {
        background-color: #0056b3;
        border-color: #004080;
        font-size: 20px; /* Increase font size */
        padding: 15px 30px; /* Increase padding */
        border-radius: 10px; /* Optional: Increase border radius */
      }

      .btn-help-icon {
        font-size: 20px; /* Increase font size */
        padding: 10px 20px; /* Increase padding */
      }
      "))
  ),

  bsModal(
    "modal1",
    title = tags$a(style = "color: white", icon('chart-line'), 'Slope Selector Guide'),
    "slope_helpIcon",
    size = "large",
    div(class = "modal-body",
        p(class = "modal-intro", "Upon initial NCA run, the plots will display the optimal slope selection. However, you have the flexibility to change it. Remember to save/update your results once you are done!"),
        div(class = "gif-container",
            div(class = "gif-title", "Zoom"),
            img(id = "zoom-gif", src = system.file("images/zoom_slopeselector.gif", package = "aNCA"),  alt = "Zoom")
        ),
        div(class = "gif-container",
            div(class = "gif-title", "Select"),
            img(id = "select-gif", src = system.file("images/selection_slopeselector.gif", package = "aNCA"), alt = "Select")
        ),
        div(class = "gif-container",
            div(class = "gif-title", "Exclude"),
            img(id = "exclude-gif", src = system.file("images/exclusion_slopeselector.gif", package = "aNCA"), alt = "Exclude")
        ),
        div(class = "gif-container",
            div(class = "gif-title", "Check"),
            img(id = "check-gif", src = system.file("images/status_slopeselector.gif", package = "aNCA"), alt = "Check")
        )
    )
  )
                          ),


                        )

               ),

               tabPanel("Results", fluid = TRUE,
                        navlistPanel(
                          tabPanel("NCA Results",

                                   downloadButton("settings_save", "Save Project Settings"),
                                   br(),
                                   pickerInput("params", "Select Parameters :", choices = list('Run NCA first'=''), selected = list('Run NCA first'=''), multiple = TRUE, options = list(`actions-box` = TRUE)),

                                   DTOutput("myresults"),
                                   tableOutput("summaryTable"),
                                   actionButton('download',"Download the NCA Data"),
                                   downloadButton('local_download_NCAres',"Download locally the NCA Data"),
                          ),
                          tabPanel("Slopes",
                                   DTOutput("preslopesettings")
                                   ),

                          tabPanel("Exclusions",
                                   fluidRow(tags$head(tags$style(HTML(".selectize-input {height: 100%; width: 200%; font-size: 100%;}")))),
                                   tableOutput("slope_manual_NCA_data2")
                          ),
                          # tabPanel("Summary Tables",
                          #
                          #          uiOutput("customselect"),
                          #          DTOutput("descriptivestats2"),
                          #          actionButton('downloadsum',"Download the NCA Summary Data"),
                          #          actionButton('downloadsum_csv', "Save NCA Summary Data to Directory"),
                          #          downloadButton('downloadsum_browser', "Download Summary Data"),
                          # ),
                        ),

               ),
             ),


             ),
    ),

    # OUTPUTS ----

    tabPanel("Outputs", fluid = TRUE,

             navlistPanel(

               tabPanel("General Plotting",

                        tabsetPanel(

                          tabPanel("Individual Plots",
                                   fluidRow(
                                     column(4, # This column will take 4/12 of the width of the row
                                            uiOutput("generalplot_analyte"),
                                            uiOutput("generalplot_usubjid"),
                                            uiOutput("generalplot_colorby"),
                                            radioButtons("log", "Select the Plot type:",
                                                         choices = c("Lin", "Log")),
                                            radioButtons("timescale", "Choose the Timescale",
                                                         choices = c("All Time", "By Cycle"),
                                                         selected = "All Time"),
                                            conditionalPanel(
                                              condition = "input.timescale == 'By Cycle'",
                                              uiOutput("cycleselect"                 )
                                            )
                                     ),
                                     column(8, # This column will take 8/12 of the width of the row
                                            plotlyOutput("individualplot", height = "400px")
                                            # uiOutput("independentplotslider")
                                     )
                                   )
                          ),



                          tabPanel("Mean Plots",
                                   uiOutput("studyidmean"),
                                   uiOutput("analytemean"),
                                   uiOutput("cyclemean"),
                                   uiOutput("selectidvar"),
                                   #actionButton("submitmean", "Submit"),
                                   checkboxInput("logmeanplot", label = "Scale y Log"),
                                   checkboxInput("sdmeanplot", label = "Show SD"),
                                   plotlyOutput("meanplot", height = '400px'),
                                   br(),
                                   helpText('If n<3 at the specified time point then the mean value is not displayed.')
                                   )
                          )

                        ),

              tabPanel("Dose Escalation",

                       tabsetPanel(
                         tabPanel("Descriptive Statistics",
                                  fluidRow(
                                    column(
                                      width = 9,
                                      orderInput("summarygroupbysource", "Drag and drop these variables...",
                                                 items = c("STUDYID", "USUBJID", "DOSEA", "PCSPEC", "ANALYTE"),
                                                 width = shiny::validateCssUnit('100%'),
                                                 connect = "summarygroupby"),
                                      orderInput("summarygroupby", "..to hierarchically group by (order matters!):",
                                                 items = c("DOSNO"),
                                                 width = shiny::validateCssUnit('100%'),
                                                 connect = "summarygroupbysource",
                                                 placeholder = "Drag items here to group hierarchically...")

                                    ),
                                    column(
                                      width = 3,
                                      uiOutput("summaryselect")
                                    )
                                  ),
                                  br(),
                                  DTOutput('descriptivestats'),
                                  actionButton('downloadsum',"Download the NCA Summary Data"),
                                  actionButton('downloadsum_csv', "Save NCA Summary Data to Directory"),
                                  downloadButton('downloadsum_browser', "Download Summary Data")
                         ),

                         # tabPanel("Mean Plots",
                         #
                         #          plotlyOutput('mean_concovertime'),
                         #          br(),
                         #          plotlyOutput('mean_concovertimelog')
                         #          ),
                         #
                         # tabPanel("Concentration Plots",
                         #
                         #          plotlyOutput('norm_concovertime'),
                         #          br(),
                         #          plotlyOutput('norm_concovertimesemilog')
                         #          ),

                         tabPanel("Box Plots",
                                  uiOutput("selectboxplot"),
                                  uiOutput('select_xvars_boxplot'),
                                  pickerInput(inputId = 'selected_xvals_boxplot', 
                                              label = 'Select values to display for grouping',
                                              multiple = T,
                                              choices = NULL, 
                                              selected = NULL,
                                              options = list(`actions-box` = TRUE)
                                              ),
                                  uiOutput('select_colorvars_boxplot'),
                                  
                                  
                                  uiOutput("display_dose_boxplot"),
                                  uiOutput("display_dosenumber_boxplot"),
                                  uiOutput("violin_toggle"),
                                  plotlyOutput('boxplot')
                                  # plotlyOutput("cmaxboxplot"),
                                  # br(),
                                  # plotlyOutput("aucboxplot"),


                                  )


               )
              ),

              tabPanel("CDISC",

                       # actionButton('slideR', 'Generate autoslideR data')
                       downloadButton('exportCDISC', 'Export CDISC')
              ),

              tabPanel("Report",
                       tabsetPanel(
                          tabPanel("Configuration",
                                   # add radio buttons with single or multiple dose as choice
                                   radioButtons("report_studytype", "Select the study type:",
                                                choices = c("SD", "MD")),
                                   actionButton("generate_report", "Generate Report")
                                   ),
                          tabPanel("Report",
                                   downloadButton("download_rmd", "Export Report "),
                                   uiOutput("rmd_content")

                          )

                        )
              )


             )

            )



    )

  )

)
