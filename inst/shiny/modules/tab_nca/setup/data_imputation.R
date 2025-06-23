data_imputation_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    input_switch(
      id = ns("should_impute_c0"),
      label = "Impute Concentration",
      value = TRUE
    ),
    br(),
    helpText(HTML(paste(
      "Imputes a start-of-interval concentration to calculate non-observational parameters:",
      "- If DOSNO = 1 & IV bolus: C0 = 0",
      "- If DOSNO > 1 & not IV bolus: C0 = predose",
      "- If IV bolus & monoexponential data: logslope",
      "- If IV bolus & not monoexponential data: C0 = C1",
      sep = "<br>"
    ))),
    br(),
    selectInput(
      ns("select_blq_strategy"),
      "Select BLQ Imputation Strategy",
      choices = c(
        "tmax based imputation",
        "Positional BLQ imputation",
        "Set value for all BLQ"
      ),
      selected = "tmax based imputation"
    ),
    # Always render all inputs, but use conditionalPanel to show/hide
    div(
      style = "margin-top: 1em;",
      conditionalPanel(
        condition = sprintf("input['%s'] == 'tmax based imputation'", ns("select_blq_strategy")),
        selectizeInput(ns("before_tmax"), "Before tmax",  choices = c("drop", "keep"), selected = "keep", options = list(create = TRUE), width = "25%"),
        selectizeInput(ns("after_tmax"), "After tmax", choices = c("drop", "keep"), selected = "drop", options = list(create = TRUE), width = "25%")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Positional BLQ imputation'", ns("select_blq_strategy")),
        selectizeInput(ns("before_first_non_blq"), "Before first non-BLQ", choices = c("drop", "keep"), selected = "drop", options = list(create = TRUE), width = "25%"),
        selectizeInput(ns("in_between_non_blqs"), "In between non-BLQs", choices = c("drop", "keep"), selected = "keep", options = list(create = TRUE), width = "25%"),
        selectizeInput(ns("after_last_non_blq"), "After last non-BLQ", choices = c("drop", "keep"), selected = "drop", options = list(create = TRUE), width = "25%")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Set value for all BLQ'", ns("select_blq_strategy")),
        selectizeInput(ns("blq_value"), "BLQ value", choices = c("drop", "keep"), selected = "0.05", options = list(create = TRUE), width = "25%")
      )
    )
  )
}

data_imputation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    should_impute_c0 <- reactive({
      input$should_impute_c0
    })
    
    blq_rule <- reactive({
      req(input$select_blq_strategy)
      rule_list <- switch(input$select_blq_strategy,
                          "tmax based imputation" = list(
                            before.tmax = input$before_tmax,
                            after.tmax = input$after_tmax
                          ),
                          "Positional BLQ imputation" = list(
                            first = input$before_first_non_blq,
                            middle = input$in_between_non_blqs,
                            last = input$after_last_non_blq
                          ),
                          "Set value for all BLQ" = input$blq_value,
                          NULL
      )
      are_valid_inputs <- sapply(rule_list, function(x) grepl('[0-9]*', x) || x %in% c('drop', 'keep'))
      if (any(!are_valid_inputs) && !is.null(rule_list[[1]])) {
        browser()
        showNotification("Please, BLQ imputation values should be numeric or NA.", type = "error")
        return(NULL)
      }
      rule_list
    })
    
    return(list(
      should_impute_c0 = should_impute_c0,
      blq_imputation_rule = blq_rule
    ))
  })
}
