tlg_plot_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 9,
      fluidRow(
        class = "plot-widgets-container",
        div(align = "left", shinyjs::disabled(actionButton(ns("previous_page"), "Previous Page"))),
        div(
          align = "center",
          tags$span(
            class = "inline-select-input",
            tags$span("Page "),
            uiOutput(ns("select_page_ui")),
            tags$span(" out of "),
            uiOutput(ns("page_number"), inline = TRUE)
          )
        ),
        div(align = "right", actionButton(ns("next_page"), "Next Page"))
      ),
      # Plots display #
      plotlyOutput(ns("plot"))
    ),
    column(
      width = 3,
      uiOutput(ns("options"))
    )
  )
}

tlg_plot_server <- function(id, render_plot, options = NULL, data = NULL) {
  moduleServer(id, function(input, output, session) {
    current_page <- reactiveVal(1)

    #' updating current page based on user input
    observeEvent(input$next_page, current_page(current_page() + 1))
    observeEvent(input$previous_page, current_page(current_page() - 1))
    observeEvent(input$select_page, current_page(as.numeric(input$select_page)))

    observeEvent(data(), {
      current_page(1)
      output$page_number <- renderUI(length(plot_list()))
      output$select_page_ui <- renderUI({
        selectInput(
          inputId = session$ns("select_page"),
          label = "",
          choices = seq_len(length(plot_list())),
          selected = 1
        )
      })
    })

    observeEvent(current_page(), {
      shinyjs::toggleState(id = "previous_page", condition = current_page() > 1)
      shinyjs::toggleState(id = "next_page", condition = current_page() < length(plot_list()))
      updateSelectInput(session = session, inputId = "select_page", selected = current_page())
    })


    plot_list <- reactive({
      plot_options <- purrr::list_modify(list(data = data()), !!!reactiveValuesToList(options_))

      purrr::iwalk(plot_options, \(value, name) {
        if (isTRUE(value %in% c(NULL, "", 0)))
          plot_options[[name]] <<- NULL
      })

      do.call(render_plot, plot_options)
    })

    output$plot <- renderPlotly({
      plot_list()[[current_page()]]
    })

    options_ <- reactiveValues()
    option_widgets <- purrr::imap(options, function(opt_def, opt_id) {
      observeEvent(input[[opt_id]], {
        options_[[opt_id]] <- input[[opt_id]]
      })

      label <- if (is.null(opt_def$label)) opt_id else opt_def$label

      switch(
        opt_def$type,
        text = {
          textInput(
            session$ns(opt_id),
            label = label,
            value = ""
          )
        },
        numeric = {
          numericInput(
            session$ns(opt_id),
            label = label,
            value = 0
          )
        },
        select = {
          choices <- {
            if (isTRUE(opt_def$choices == ".colnames")) {
              names(data())
            } else if (length(opt_def$choices) == 1 && grepl("^\\.", opt_def$choices)) {
              unique(data()[, sub("^\\.", "", opt_def$choices)])
            } else {
              opt_def$choices
            }
          }
          selectInput(
            session$ns(opt_id),
            label = label,
            selected = "",
            choices = c("", choices),
            multiple = isTRUE(opt_def$multiple)
          )
        }
      )
    })

    output$options <- renderUI(option_widgets)
  })
}