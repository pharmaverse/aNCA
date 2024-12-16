tlg_plot_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    class = "tlg-plot-module",
    column(
      width = 9,
      fluidRow(
        class = "plot-widgets-container",
        div(
          align = "left",
          tags$span(
            class = "inline-select-input",
            style = "margin-right: 5em;",
            tags$span("Plots per page:"),
            selectInput(
              ns("plots_per_page"),
              "",
              choices = c("All", 1, 2, 4, 6, 8, 10)
            )
          ),
          shinyjs::disabled(actionButton(ns("previous_page"), "Previous Page"))
        ),
        div(
          align = "center",
          tags$span(
            class = "inline-select-input",
            tags$span("Page "),
            selectInput(
              inputId = ns("select_page"),
              label = "",
              choices = ""
            ),
            tags$span(" out of "),
            uiOutput(ns("page_number"), inline = TRUE),
          )
        ),
        div(align = "right", actionButton(ns("next_page"), "Next Page"))
      ),
      # Plots display #
      uiOutput(ns("plots"))
    ),
    column(
      width = 3,
      uiOutput(ns("options"), class = "plot-options-container")
    )
  )
}

tlg_plot_server <- function(id, render_plot, options = NULL) {
  moduleServer(id, function(input, output, session) {
    data <- session$userData$data

    current_page <- reactiveVal(1)

    #' updating current page based on user input
    observeEvent(input$next_page, current_page(current_page() + 1))
    observeEvent(input$previous_page, current_page(current_page() - 1))
    observeEvent(input$select_page, current_page(as.numeric(input$select_page)))

    #' hold reactive information about the page layout
    num_pages <- reactive({
      req(plot_list(), plots_per_page())
      ceiling(length(plot_list()) / plots_per_page())
    })

    plots_per_page <- reactive({
      if (is.null(input$plots_per_page)) return(NULL)
      if (input$plots_per_page == "All") {
        isolate(length(plot_list()))
      } else {
        as.numeric(input$plots_per_page)
      }
    })

    #' updates UI responsible for page change
    observeEvent(list(current_page(), num_pages()), {
      req(num_pages(), current_page())
      shinyjs::toggleState(id = "previous_page", condition = current_page() > 1)
      shinyjs::toggleState(
        id = "next_page",
        condition = num_pages() != 1 && current_page() < num_pages()
      )
      updateSelectInput(session = session, inputId = "select_page", selected = current_page())
    })
    observeEvent(plots_per_page(), {
      req(num_pages(), plots_per_page())
      current_page(1)

      output$page_number <- renderUI(paste0(num_pages(), "."))
      updateSelectInput(inputId = "select_page", choices = seq_len(num_pages()))
    })

    #' keeps list of plots to render, with options gathered from the UI and applied
    plot_list <- reactive({
      plot_options <- purrr::list_modify(list(data = data()), !!!reactiveValuesToList(options_))

      purrr::iwalk(plot_options, \(value, name) {
        if (isTRUE(value %in% c(NULL, "", 0)))
          plot_options[[name]] <<- NULL
      })

      do.call(render_plot, plot_options)
    })

    output$plots <- renderUI({
      req(plot_list(), plots_per_page(), current_page())
      num_plots <- length(plot_list())
      page_end <- current_page() * plots_per_page()
      page_start <- page_end - plots_per_page() + 1
      if (page_end > num_plots) page_end <- num_plots

      plot_list()[page_start:page_end]
    })

    #' resets the options to defaults
    observeEvent(input$reset_widgets, {
      purrr::walk(names(options), shinyjs::reset)
    })

    #' holds options gathered from UI widgets
    options_ <- reactiveValues()

    #' creates widgets responsible for custimizing the plots
    option_widgets <- purrr::imap(options, function(opt_def, opt_id) {
      if (grepl(".group_label", opt_id)) {
        return(tags$h1(opt_def, class = "tlg-group-label"))
      }

      observeEvent(input[[opt_id]], {
        options_[[opt_id]] <- input[[opt_id]]
      })

      create_edit_widget(opt_def, opt_id)
    })

    option_widgets <- tagList(
      actionButton(
        inputId = session$ns("reset_widgets"),
        label = "Reset to defaults"
      ),
      option_widgets
    )

    output$options <- renderUI(option_widgets)
  })
}

#' Creates editing widget of appropriate type.
#' @param opt_def Definition of the option
#' @param opt_id  Id of the option
#' @param session Session object for namespacing the widgets
#' @returns Shiny widget with appropriate type, label and options
create_edit_widget <- function(opt_def, opt_id, session = shiny::getDefaultReactiveDomain()) {
  label <- if (is.null(opt_def$label)) opt_id else opt_def$label

  switch(
    opt_def$type,
    text = {
      textInput(
        session$ns(opt_id),
        label = label,
        value = opt_def$default
      )
    },
    numeric = {
      numericInput(
        session$ns(opt_id),
        label = label,
        value = opt_def$default
      )
    },
    select = {
      choices <- {
        if (isTRUE(opt_def$choices == ".colnames")) {
          names(session$userData$data())
        } else if (length(opt_def$choices) == 1 && grepl("^\\.", opt_def$choices)) {
          unique(session$userData$data()[, sub("^\\.", "", opt_def$choices)])
        } else {
          opt_def$choices
        }
      }

      selected <- {
        if (!is.null(opt_def$default)) {
          if (opt_def$default == ".all") {
            choices
          } else {
            opt_def$default
          }
        } else {
          ""
        }
      }

      selectInput(
        session$ns(opt_id),
        label = label,
        selected = selected,
        choices = c("", choices),
        multiple = isTRUE(opt_def$multiple)
      )
    }
  )
}