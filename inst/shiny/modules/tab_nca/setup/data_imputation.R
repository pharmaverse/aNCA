data_imputation_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # BLQ imputation widgets
    fluidRow(
      column(
        width = 10,
        selectInput(
          ns("select_blq_strategy"),
          "Select BLQ Imputation Strategy",
          choices = c(
            "Tmax based imputation",
            "Positional BLQ imputation",
            "Set value for all BLQ",
            "No BLQ handling"
          ),
          selected = "No BLQ handling"
        )
      ),
      column(
        width = 2,
        dropdown(
          div(
            tags$h4("BLQ Imputation Help"),
            p(
              "Imputes concentration values for BLQ (Below Limit of Quantification) samples.",
              "Only applies to non-observational parameters:"
            ),
            tags$table(
              style = "width:100%; border-collapse:collapse; margin-bottom: 8px;",
              tags$thead(
                tags$tr(
                  tags$th(
                    "Strategy",
                    style = "text-align:left; border-bottom:1px solid #ddd; padding:4px;"
                  ),
                  tags$th(
                    "Description",
                    style = "text-align:left; border-bottom:1px solid #ddd; padding:4px;"
                  )
                )
              ),
              tags$tbody(
                tr("Tmax based", "Set rules for BLQ values before/after Tmax"),
                tr("Positional",
                   "Set rules for BLQ before, between or after the first/last non-BLQ values"),
                tr("Set value", "Assign a single value to all BLQ"),
                tr("No handling", "Keep all values as is")
              )
            ),
            tags$b("Custom values:"),
            p(HTML(
              "In dropdowns, select <code>drop</code>, <code>keep</code>, ",
              "or type a number (e.g., <code>0.05</code>) ",
              "and press <kbd>Enter</kbd>.<br>Only numeric values, <code>drop</code>, ",
              "or <code>keep</code> are accepted."
            ))
          ),
          style = "unite",
          right = TRUE,
          icon = icon("question"),
          status = "primary",
          width = "400px"
        )
      )
    ),
    div(
      style = "margin-top: 1em;",
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Tmax based imputation'", ns("select_blq_strategy")),
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
        blq_selectize(ns("blq_value"), "Value for BLQ", selected = "0.05")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'No BLQ handling'", ns("select_blq_strategy")),
        helpText("No BLQ imputation will be applied. All values are kept as is")
      )
    ),
    hr(),
    # Start impute (C0) widget and help button
    fluidRow(
      column(
        width = 10,
        input_switch(
          id = ns("should_impute_c0"),
          label = "Impute Start Concentration",
          value = TRUE
        )
      ),
      column(
        width = 2,
        dropdown(
          div(
            tags$h4("Start Concentration Imputation Help"),
            p(
              "Imputes a start-of-interval concentration (dose time).",
              "It only applies to non-observational parameters.",
              "Depending on the interval, the imputation changes:"
            ),
            tags$table(
              style = "width:100%; border-collapse:collapse;",
              tags$thead(
                tags$tr(
                  tags$th(
                    "Interval scenario",
                    style = "text-align:left; border-bottom:1px solid #ddd; padding:4px;"
                  ),
                  tags$th(
                    "Imputation Rule",
                    style = "text-align:left; border-bottom:1px solid #ddd; padding:4px;"
                  )
                )
              ),
              tags$tbody(
                tr("Start concentration already present", "C0 = C0"),
                tr("Not IV bolus (or metabolite) & first dose", "C0 = 0"),
                tr("Not IV bolus (or metabolite) & not first dose with predose", "C0 = predose"),
                tr("IV bolus & monoexponential data", "log-back-extrapolation"),
                tr("IV bolus & not monoexponential data", "C0 = C1")
              )
            )
          ),
          style = "unite",
          right = TRUE,
          icon = icon("question"),
          status = "primary",
          width = "500px"
        )
      )
    )
  )
}

data_imputation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    blq_imputation_rule <- reactive({
      req(input$select_blq_strategy)
      rule_list <- switch(input$select_blq_strategy,
        "Tmax based imputation" = list(
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
        "No BLQ handling" = list(
          first = "keep",
          middle = "keep",
          last = "keep"
        ),
        NULL
      )

      # Transform text numbers in the list to numeric
      rule_list <- lapply(rule_list, function(x) {
        if (x %in% c("drop", "keep")) {
          x
        } else if (grepl("^[0-9]+(\\.[0-9]+)?$", x)) {
          as.numeric(x)
        } else {
          NA
        }
      })
      are_invalid_inputs <- is.na(unlist(rule_list))
      if (any(are_invalid_inputs)) {
        showNotification(
          paste0(
            "BLQ imputation values must be numeric, 'drop' or 'keep'. ",
            "Otherwise, no BLQ imputation will be applied."
          ),
          type = "warning",
          duration = 8
        )
        return(NULL)
      }

      rule_list
    })

    list(
      should_impute_c0 = reactive(input$should_impute_c0),
      blq_imputation_rule = blq_imputation_rule
    )
  })
}

# Helper function for BLQ selectize inputs
blq_selectize <- function(id, label, selected = NULL) {
  selectizeInput(
    id, label,
    choices = unique(c("drop", "keep", selected)),
    selected = selected,
    options = list(
      create = TRUE,
      placeholder = "Type a numeric value to impute or select option"
    ),
    width = "25%"
  )
}

# Helper for table rows in help popups (variable columns)
tr <- function(...) {
  tags$tr(
    lapply(list(...), function(cell) tags$td(cell, style = "padding:4px;"))
  )
}
