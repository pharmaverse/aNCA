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
  includeScript(file.path(assets, "index.js")),

  page_navbar(
    id = "page",
    title = "aNCA",
    # DATA ----
    nav_panel(
      "Data",
      value = "data",
      fluid = TRUE,
      tab_data_ui("data")
    ),
    # NCA ----
    nav_panel("NCA", value = "nca", fluid = TRUE,
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
                selectInput(
                  "select_analyte",
                  "Choose the analyte :",
                  choices = NULL,
                  multiple = TRUE),
                selectInput(
                  "select_dosno",
                  "Choose the Dose Number:",
                  multiple = TRUE,
                  choices = c("Please specify ANALYTE in Data Selection" = "")
                ),
                br(),
                actionButton("submit_analyte", "Submit"),
                reactableOutput("datatable"),
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
                tags$div(
                  checkboxInput(
                    inputId = "should_impute_c0",
                    label = "Impute concentration at t0 when missing",
                    value = TRUE
                  ),
                  id = "checkbox_id",
                  title = paste(
                    "Imputes a start-of-interval concentration 
                    to calculate non-observational parameters:",
                    "If DOSNO = 1 & IV bolus: C0 = 0",
                    "If DOSNO > 1 & not IV bolus: C0 = predose",
                    "If IV bolus & monoexponential data: logslope",
                    "If IV bolus & not monoexponential data: C0 = C1",
                    sep = "\n"
                  )
                ),
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
                    checkboxInput("rule_adj_r_squared", "RSQADJ:")
                  ),
                  column(
                    width = 6,
                    conditionalPanel(
                      condition = "input.rule_adj_r_squared == true",
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
                    checkboxInput("rule_aucpext_obs", "AUCPEO (% ext.observed): ")
                  ),
                  column(
                    width = 6,
                    conditionalPanel(
                      condition = "input.rule_aucpext_obs == true",
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
                    checkboxInput("rule_aucpext_pred", "AUCPEP (% ext.predicted): "),
                  ),
                  column(
                    width = 6,
                    conditionalPanel(
                      condition = "input.rule_aucpext_pred == true",
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
                    checkboxInput("rule_span_ratio", "SPAN: "),
                  ),
                  column(
                    width = 6,
                    conditionalPanel(
                      condition = "input.rule_span_ratio == true",
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

              tabPanel("Slope Selector", slope_selector_ui("slope_selector")),

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
                tableOutput("manual_slopes2")
              ),
              tabPanel("Parameter Datasets",
                       tabsetPanel(
                         tabPanel("PP",
                                  DTOutput("pp_dataset")),
                         tabPanel("ADPP",
                                  DTOutput("adpp_dataset"))
                       ))
            )
          )
        )
      )
    ),

    # VISUALISATION ----
    nav_panel("Visualisation", value = "visualisation",
      fluid = TRUE,
      tab_visuals_ui("visuals")
    ),
    # New TLG tab
    nav_panel("TLG", value = "tlg",
      tab_tlg_ui("tlg")
    )
  ),
  shinyjs::useShinyjs(),
  reactable.extras::reactable_extras_dependency()
)
