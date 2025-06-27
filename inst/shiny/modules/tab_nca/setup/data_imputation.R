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
        blq_selectize(ns("before_tmax"), "Before tmax", selected = "0"),
        blq_selectize(ns("after_tmax"), "After tmax", selected = "drop")
      ),
      conditionalPanel(
        condition = sprintf(
          "input['%s'] == 'Positional BLQ imputation'", ns("select_blq_strategy")
        ),
        blq_selectize(ns("before_first_non_blq"), "Before first non-BLQ", selected = "drop"),
        blq_selectize(ns("in_between_non_blqs"), "In between non-BLQs", selected = "keep"),
        blq_selectize(ns("after_last_non_blq"), "After last non-BLQ", selected = "drop")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Set value for all BLQ'", ns("select_blq_strategy")),
        blq_selectize(ns("blq_value_first"), "BLQ value (first)", selected = "0.05"),
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
      rule_list <- switch(
        input$select_blq_strategy,
        "tmax based imputation" = list(
          before.tmax = input$before_tmax,
          after.tmax = input$after_tmax
        ),
        "Positional BLQ imputation" = list(
          first = input$before_first_non_blq,
          middle = input$in_between_non_blqs,
          last = input$after_last_non_blq
        ),
        "Set value for all BLQ" = list(
          first = input$blq_value,
          middle = input$blq_value,
          last = input$blq_value
        ),
        NULL
      )
      # Transform in the list text number to numeric
      rule_list <- lapply(rule_list, function(x) {
        if (x %in% c("drop", "keep")) {
          return(x)
        } else if (grepl("^[0-9]+(\\.[0-9]+)?$", x)) {
          return(as.numeric(x))
        } else {
          return(NA)
        }
      })
      are_invalid_inputs <- is.na(unlist(rule_list))
      if (any(are_invalid_inputs)) {
        showNotification(
          "BLQ imputation values must be numeric, 'drop' or 'keep'. 
          Otherwise, default PKNCA imputation (BLQ position) will be used",
          type = "warning",
          duration = 8
        )
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

# Helper function for BLQ selectize inputs
blq_selectize <- function(id, label, selected = NULL) {
  selectizeInput(
    id, label,
    choices = c("drop", "keep", selected),
    selected = selected,
    options = list(create = TRUE),
    width = "25%"
  )
}