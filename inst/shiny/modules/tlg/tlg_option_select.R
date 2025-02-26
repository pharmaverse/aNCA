tlg_option_select_ui <- function(id, opt_def, data) {
  ns <- NS(id)

  label <- if (is.null(opt_def$label)) id else opt_def$label

  choices <- {
    if (isTRUE(opt_def$choices == ".colnames")) {
      names(data())
    } else if (length(opt_def$choices) == 1 && grepl("^\\$", opt_def$choices)) {
      unique(data()[, sub("^\\$", "", opt_def$choices)])
    } else {
      opt_def$choices
    }
  }

  selected <- {
    if (!is.null(opt_def$default)) {
      if (isTRUE(opt_def$default == ".all")) {
        choices
      } else {
        opt_def$default
      }
    } else {
      ""
    }
  }

  selectInput(
    ns("select"),
    label = label,
    selected = selected,
    choices = c("", choices),
    multiple = isTRUE(opt_def$multiple)
  )
}

tlg_option_select_server <- function(id, opt_def, data) {
  moduleServer(id, function(input, output, session) {
    reactive({
      input$select
    })
  })
}