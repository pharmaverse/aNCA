assets <- system.file("shiny/www", package = "aNCA")
source(system.file("shiny/modules/tab_tlg.R", package = "aNCA"))

# Define UI
fluidPage(
  tags$script("
    Shiny.addCustomMessageHandler('update', function(value) {
    Shiny.setInputValue('update', value);
    }); "),

  tags$script("
    Shiny.addCustomMessageHandler('increment', function(value) {
      var newValue = value + 1;
      Shiny.setInputValue('update', newValue, {priority: 'event'});
    });
  "),

  includeCSS(file.path(assets, "style.css")),

  navbarPage(
    id = "page",
    title = "",
    # DATA ----
    tabPanel(
      "Data",
      fluid = TRUE,
      tab_data_ui("data")
    ),
    # NCA ----
    tabPanel("NCA", fluid = TRUE,
      fluidPage(
        actionButton("nca", "Run NCA", class = "run-nca-btn"),

        tabsetPanel(id = "ncapanel",
          tabPanel("Setup", fluid = TRUE,

            navlistPanel(
              tabPanel("Data Selection",
                # Local upload option
                fileInput(
                  "settings_upload",
                  width = "60%",
                  label = "Upload Settings",
                  buttonLabel = list(icon("folder"), "Browse"),
                  accept = c(".csv", ".xpt")
                ),
                br(),

                # Selection of analyte
                selectInput("select_analyte", "Choose the analyte :", choices = NULL),
                selectInput("select_dosno", "Choose the Dose Number:",
                            multiple = TRUE,
                            choices = c("Please specify ANALYTE in Data Selection" = "")),
                br(),
                actionButton("submit_analyte", "Submit"),
                DTOutput("datatable"),
              ),
              tabPanel("Settings",
                selectInput(
                  "method",
                  "Extrapolation Method:",
                  choices = c(
                    "lin-log", "lin up/log down", "linear", "Linear LinearLogInterpolation"
                  ),
                  selected = "lin up/log down"
                ),
                h4("Data imputation"),
                checkboxInput(inputId = "should_impute_c0",
                              label = "Impute C0 when missing",
                              value = TRUE),
                br(),
                checkboxInput("AUCoptions", "Select Partial AUC"),
                conditionalPanel(
                  condition = "input.AUCoptions == true",
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
                      condition = "input.rule_adj.r.squared == true",
                      div(
                        style = "display: flex; align-items: center;",
                        span(">=", style = "margin-right: 5px;"),
                        numericInput(
                          "adj.r.squared_threshold",
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
                    checkboxInput("rule_aucpext.obs", "AUCPEO (% ext.observed): ")
                  ),
                  column(
                    width = 6,
                    conditionalPanel(
                      condition = "input.rule_aucpext.obs == true",
                      div(
                        style = "display: flex; align-items: center;",
                        span(">=", style = "margin-right: 5px;"),
                        numericInput(
                          "aucpext.obs_threshold",
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
                    checkboxInput("rule_aucpext.pred", "AUCPEP (% ext.predicted): "),
                  ),
                  column(
                    width = 6,
                    conditionalPanel(
                      condition = "input.rule_aucpext.pred == true",
                      div(
                        style = "display: flex; align-items: center;",
                        span(">=", style = "margin-right: 5px;"),
                        numericInput(
                          "aucpext.pred_threshold",
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
                    checkboxInput("rule_span.ratio", "SPAN: "),
                  ),
                  column(
                    width = 6,
                    conditionalPanel(
                      condition = "input.rule_span.ratio == true",
                      div(
                        style = "display: flex; align-items: center;",
                        span(">=", style = "margin-right: 5px;"),
                        numericInput(
                          "span.ratio_threshold",
                          "",
                          value = 2,
                          step = 1,
                          min = 0
                        )
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
                      actionButton("remove_excsel", "- Remove selected rows"),
                      DTOutput("slope_manual_NCA_data"),
                      actionButton("save_excsel", "Save"),
                      style = "unite", icon = icon("chart-line"),
                      status = "success", width = "500px",
                    )
                  ),
                  column(
                    width = 4,
                    selectInput(
                      "plots_per_page",
                      "Plots per page:",
                      choices = c(2, 4, 6, 8, 10),
                      selected = 2,
                      width = "50%"
                    ),
                  ),
                  column(
                    width = 5,
                    pickerInput(
                      "search_patient",
                      label = "Search Patient",
                      width = "70%",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE)
                    ),
                  )
                ),
                uiOutput("slopetestUI"),
                # Include details for modal message in slope_helpIcon (Instruction details)
                bsModal(
                  "modal1",
                  title = tags$a(
                    style = "color: white",
                    icon("chart-line"),
                    "Slope Selector Guide"
                  ),
                  "slope_helpIcon",
                  size = "large",
                  div(class = "modal-body",
                    p(
                      class = "modal-intro",
                      "Upon initial NCA run, the plots will display the optimal slope selection.
                      However, you have the flexibility to change it. Remember to save/update
                      your results once you are done!"
                    ),
                    div(class = "gif-container",
                      div(class = "gif-title", "Zoom"),
                      img(
                        id = "zoom-gif",
                        src = system.file("images/zoom_slopeselector.gif", package = "aNCA"),
                        alt = "Zoom"
                      )
                    ),
                    div(class = "gif-container",
                      div(class = "gif-title", "Select"),
                      img(
                        id = "select-gif",
                        src = system.file("images/selection_slopeselector.gif", package = "aNCA"),
                        alt = "Select"
                      )
                    ),
                    div(class = "gif-container",
                      div(class = "gif-title", "Exclude"),
                      img(
                        id = "exclude-gif",
                        src = system.file("images/exclusion_slopeselector.gif", package = "aNCA"),
                        alt = "Exclude"
                      )
                    ),
                    div(class = "gif-container",
                      div(class = "gif-title", "Check"),
                      img(
                        id = "check-gif",
                        src = system.file("images/status_slopeselector.gif", package = "aNCA"),
                        alt = "Check"
                      )
                    )
                  )
                )
              ),
            )
          ),
          tabPanel("Results", fluid = TRUE,
            navlistPanel(
              tabPanel(
                "NCA Results",
                downloadButton("settings_save", "Save Project Settings"),
                br(),
                pickerInput(
                  "params",
                  "Select Parameters :",
                  choices = list("Run NCA first" = ""),
                  selected = list("Run NCA first" = ""),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)
                ),
                DTOutput("myresults"),
                tableOutput("summaryTable"),
                actionButton("download", "Download the NCA Data"),
                downloadButton("local_download_NCAres", "Download locally the NCA Data"),
              ),
              tabPanel(
                "Slopes",
                DTOutput("preslopesettings")
              ),
              tabPanel(
                "Exclusions",
                tableOutput("slope_manual_NCA_data2")
              )
            )
          )
        )
      )
    ),
    # OUTPUTS ----
    tabPanel("Outputs", fluid = TRUE,
      navlistPanel(
        tabPanel("General Plotting",
          tabsetPanel(
            tabPanel("Individual Plots",
              fluidRow(
                column(
                  4, # This column will take 4/12 of the width of the row
                  uiOutput("generalplot_analyte"),
                  uiOutput("generalplot_usubjid"),
                  uiOutput("generalplot_colorby"),
                  radioButtons("log", "Select the Plot type:", choices = c("Lin", "Log")),
                  radioButtons(
                    "timescale",
                    "Choose the Timescale",
                    choices = c("All Time", "By Cycle"),
                    selected = "All Time"
                  ),
                  conditionalPanel(
                    condition = "input.timescale == 'By Cycle'",
                    uiOutput("cycleselect")
                  )
                ),
                column(8, plotlyOutput("individualplot", height = "400px"))
              )
            ),
            tabPanel("Mean Plots",
              uiOutput("studyidmean"),
              uiOutput("analytemean"),
              uiOutput("cyclemean"),
              uiOutput("selectidvar"),
              checkboxInput("logmeanplot", label = "Scale y Log"),
              checkboxInput("sdmeanplot", label = "Show SD"),
              plotlyOutput("meanplot", height = "400px"),
              br(),
              helpText("If n<3 at the specified time point then the mean value is not displayed.")
            )
          )
        ),
        tabPanel("Dose Escalation",
          tabsetPanel(
            tabPanel("Descriptive Statistics",
              fluidRow(
                column(
                  width = 9,
                  orderInput(
                    "summarygroupbysource",
                    "Drag and drop these variables...",
                    items = c("STUDYID", "USUBJID", "DOSEA", "PCSPEC", "ANALYTE"),
                    width = shiny::validateCssUnit("100%"),
                    connect = "summarygroupby"
                  ),
                  orderInput(
                    "summarygroupby",
                    "..to hierarchically group by (order matters!):",
                    items = c("DOSNO"),
                    width = shiny::validateCssUnit("100%"),
                    connect = "summarygroupbysource",
                    placeholder = "Drag items here to group hierarchically..."
                  )
                ),
                column(
                  width = 3,
                  uiOutput("summaryselect")
                )
              ),
              br(),
              DTOutput("descriptivestats"),
              actionButton("downloadsum", "Download the NCA Summary Data"),
              actionButton("downloadsum_csv", "Save NCA Summary Data to Directory"),
              downloadButton("downloadsum_browser", "Download Summary Data")
            ),
            tabPanel(
              "Box Plots",
              uiOutput("selectboxplot"),
              uiOutput("select_xvars_boxplot"),
              uiOutput("select_colorvars_boxplot"),
              pickerInput(
                inputId = "selected_varvalstofilter_boxplot",
                label = "Select values to display",
                multiple = TRUE,
                choices = NULL,
                selected = NULL,
                options = list(`actions-box` = TRUE)
              ),
              uiOutput("violin_toggle"),
              plotlyOutput("boxplot")
            )
          )
        ),
        tabPanel("CDISC", downloadButton("exportCDISC", "Export CDISC")),
        tabPanel("Report",
          tabsetPanel(
            tabPanel("Configuration",
              # add radio buttons with single or multiple dose as choice
              radioButtons(
                "report_studytype",
                "Select the study type:",
                choices = c("SD", "MD")
              ),
              actionButton("generate_report", "Generate Report")
            ),
            tabPanel("Report",
              downloadButton("download_rmd", "Export Report "),
              uiOutput("rmd_content")
            )
          )
        )
      )
    ),
    # New TLG tab
    tabPanel("TLG",
      tab_tlg_ui("tlg")
    )
  )
)
