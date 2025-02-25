tlg_list_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      position = "right",
      div(
        class = "tlg-options-container",
        dropdown(
          div(
            tags$h2("List options"),
            tags$p("
              You can specify any plot customization options that are supported by the specific
              plot module.
            "),
            tags$p("Leaving a widget empty will allow default behaviour of the listing function."),
            tags$p(
              "In text fields, you can reference values / columns in the dataset by using
              the dollar sign (", tags$b("$"), ") and providing column name, e.g. ",
              tags$b("$DOSEU"), "."
            ),
            tags$p("
              You can also reference ", tags$i("label"), " attribute of any column by prefacing the
              column name by exclamation mark (", tags$b("!"), "), e.g. ", tags$b("!DOSEU"), ".
            ")
          ),
          style = "unite",
          right = TRUE,
          icon = icon("question"),
          status = "primary"
        ),
        actionButton(
          inputId = ns("reset_widgets"),
          label = "Reset to defaults"
        ),
        uiOutput(ns("options"), class = "tlg-options-container")
      )
    ),
    div(
      class = "tlg-widgets-container",
      div(
        align = "left",
        tags$span(
          class = "inline-select-input",
          style = "margin-right: 5em;",
          tags$span("Plots per page:"),
          selectInput(
            ns("lists_per_page"),
            "",
            choices = c("All", 1, 2, 4, 6, 8, 10)
          )
        ),
        shinyjs::disabled(actionButton(ns("previous_page"), "Previous Page", class = "btn-page"))
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
      div(align = "right", actionButton(ns("next_page"), "Next Page", class = "btn-page"))
    ),
    shinycssloaders::withSpinner(
      verbatimTextOutput(ns("lists"))
    )
  )
}

tlg_list_server <- function(id, render_list, options = NULL) {
  moduleServer(id, function(input, output, session) {
    data <- session$userData$data
    current_page <- reactiveVal(1)

    #' updating current page based on user input
    observeEvent(input$next_page, current_page(current_page() + 1))
    observeEvent(input$previous_page, current_page(current_page() - 1))
    observeEvent(input$select_page, {
      if (input$select_page == "") return(NULL)
      current_page(as.numeric(input$select_page))
    })

    #' hold reactive information about the page layout
    num_pages <- reactive({
      req(list_list(), lists_per_page())
      ceiling(length(list_list()) / lists_per_page())
    })

    lists_per_page <- reactive({
      if (is.null(input$lists_per_page)) return(NULL)
      if (is.null(list_list())) return(NULL)
      if (input$lists_per_page == "All") {
        length(list_list())
      } else {
        as.numeric(input$lists_per_page)
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
    observeEvent(lists_per_page(), {
      req(num_pages(), lists_per_page())
      current_page(1)

      output$page_number <- renderUI(paste0(num_pages(), "."))
      updateSelectInput(inputId = "select_page", choices = seq_len(num_pages()))
    })

    #' keeps list of plots to render, with options gathered from the UI and applied
    list_list <- reactive({
      if (length(options_()) == 0) return(NULL)

      list_options <- purrr::list_modify(list(data = data()), !!!options_())

      purrr::iwalk(list_options, \(value, name) {
        if (isTRUE(value %in% c(NULL, "", 0)))
          list_options[[name]] <<- NULL
      })
      tryCatch({
        do.call(render_list, list_options)
      },
      error = function(e) {
        log_error("Error in list rendering:")
        print(e)
        "Error: list rendering failed with current options.
        Check the R console for more information."
      })
    })

    output$lists <- renderPrint({
      req(list_list(), lists_per_page(), current_page())
      num_plots <- length(list_list())
      page_end <- current_page() * lists_per_page()
      page_start <- page_end - lists_per_page() + 1
      if (page_end > num_plots) page_end <- num_plots

      list_list()[page_start:page_end]
    })

    #' resets the options to defaults
    observeEvent(input$reset_widgets, {
      purrr::walk(names(options), shinyjs::reset)
    })

    #' holds options gathered from UI widgets
    options_ <- reactive({
      lapply(names(options), \(opt_id) input[[opt_id]]) |>
        setNames(names(options)) |>
        purrr::keep(\(x) !is.null(x))
    }) |>
      shiny::debounce(750)

    #' creates widgets responsible for custimizing the plots
    output$options <- renderUI({
      purrr::imap(options, create_edit_widget)
    })
  })
}
