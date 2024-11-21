tlg_plot_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 9,
      plotOutput(ns("plot"))
    ),
    column(
      width = 3,
      uiOutput(ns("options"))
    )
  )
}

tlg_plot_server <- function(id, render_plot, options = NULL, data = NULL) {
  moduleServer(id, function(input, output, session) {
    plot_list <- reactive({
      plot_options <- purrr::list_modify(list(data = data()), !!!reactiveValuesToList(opts))

      purrr::iwalk(plot_options, \(value, name) {
        if (isTRUE(value == ""))
          plot_options[[name]] <<- NULL
      })

      do.call(render_plot, plot_options)
    })

    output$plot <- renderPlot({
      plot_list()[[1]]
    })

    opts <- reactiveValues()

    option_widgets <- lapply(options, function(opt_id) {
      observeEvent(input[[opt_id]], {
        opts[[opt_id]] <- input[[opt_id]]
      })

      textInput(
        session$ns(opt_id),
        label = opt_id
      )
    })

    output$options <- renderUI(option_widgets)
  })
}