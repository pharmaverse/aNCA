blq_imputation_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(
      ns("select_blq_strategy"),
      "Select BLQ Imputation Strategy",
      choices = c(
        "tmax based imputation",
        "Positional BLQ imputation",
        "Set value for all BLQ",
        "1/3 LLOQ rule",
        "1/2 LLOQ rule"
      ),
      selected = "tmax based imputation"
    ),
    uiOutput(ns("dynamic_inputs")),
    actionButton(ns("apply_blq_rule"), "Apply BLQ Rule")
  )
}

blq_imputation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$dynamic_inputs <- renderUI({
      switch(input$select_blq_strategy,
        "tmax based imputation" = tagList(
          numericInput(ns("before_tmax"), "Before tmax", value = 0),
          numericInput(ns("after_tmax"), "After tmax", value = NA)
        ),
        "Positional BLQ imputation" = tagList(
          numericInput(ns("before_first_non_blq"), "Before first non-BLQ", value = NULL),
          numericInput(ns("in_between_non_blqs"), "In between non-BLQs", value = NULL),
          numericInput(ns("after_last_non_blq"), "After last non-BLQ", value = NULL)
        ),
        "Set value for all BLQ" = numericInput(ns("blq_value"), "Value", value = NULL),
        NULL
      )
    })

    blq_rule <- reactiveVal(NULL)
    
    observeEvent(input$apply_blq_rule, {
      rule <- switch(input$select_blq_strategy,
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
        "1/3 LLOQ rule" = 1/3,
        "1/2 LLOQ rule" = 1/2
      )
      
      blq_rule(rule)
    })
    
    return(blq_rule)
  })
}
