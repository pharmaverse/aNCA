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
    output$plot <- renderPlot({
      do.call(render_plot, purrr::list_modify(list(data = data()), !!!reactiveValuesToList(opts)))
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