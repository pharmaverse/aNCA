#' Module handling specific Tables, Lists and Graphs.
#'
#' @details
#' Takes in the definition of a TLG, as defined in `tlg.yaml` file. Generates a paginated interface
#' that allows viewing of the TLG with default values. In addition, generates editing widgets in a
#' sidebar that allow the user to specify parameters passed into the rendering function.
#'
#' To read more check out documentation for each function of the module and the contributing
#' guidelines.

#' Function generating UI for a TLG module.
#'
#' @param id      id of the module, preferably with randomly generated part to avoid conflicts
#' @param type    type of the module, either "graph" or "listing", decides the rendering funciton
#' @param options list of options to customize input parameters
tlg_module_ui <- function(id, type, options) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      position = "right",
      div(
        class = "tlg-options-container",
        dropdown(
          div(
            tags$h2(str_glue("{type} options")),
            tags$p(str_glue("
              You can specify any {type} customization options that are supported by the specific
              {type} implementation function.
            ")),
            tags$p(str_glue(
              "Leaving a widget empty will allow default behaviour of the {type} function."
            )),
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
          tags$span("Entries per page:"),
          selectInput(
            ns("entries_per_page"),
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
      switch(
        type,
        graph = uiOutput(ns("tlg_output")),
        listing = verbatimTextOutput(ns("tlg_output"))
      )
    )
  )
}

#' Function generating a server function for a TLG module.
#' @param id          id of the module, preferably with randomly generated part to avoid conflicts
#' @param data        adnca data object used for processing the TLG
#' @param type        type of the module, either "graph" or "listing",
#'                    decides the rendering funciton
#' @param render_list function that renders the list of entries, actual implementation of the TLG
#' @param options     list of options to customize input parameters
#'
tlg_module_server <- function(id, data, type, render_list, options = NULL) {
  moduleServer(id, function(input, output, session) {
    render_fn <- switch(
      type,
      "graph" = renderUI,
      "listing" = renderPrint
    )

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
      req(tlg_list(), entries_per_page())
      ceiling(length(tlg_list()) / entries_per_page())
    })

    entries_per_page <- reactive({
      if (is.null(input$entries_per_page)) return(NULL)
      if (is.null(tlg_list())) return(NULL)
      if (input$entries_per_page == "All") {
        length(tlg_list())
      } else {
        as.numeric(input$entries_per_page)
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
    observeEvent(entries_per_page(), {
      req(num_pages(), entries_per_page())
      current_page(1)

      output$page_number <- renderUI(paste0(num_pages(), "."))
      updateSelectInput(inputId = "select_page", choices = seq_len(num_pages()))
    })

    #' keeps list of plots to render, with options gathered from the UI and applied
    tlg_list <- reactive({
      list_options <- purrr::imap(reactiveValuesToList(options_values), \(value, name) value())

      if (any(sapply(list_options, is.null))) return(NULL)

      list_options <- purrr::keep(list_options, \(value) all(!value %in% c(NULL, "", 0, NA)))

      tryCatch({
        do.call(render_list, purrr::list_modify(list(data = data()), !!!list_options))
      },
      error = function(e) {
        log_error("Error in list rendering:")
        print(e)
        "Error: list rendering failed with current options.
        Check the R console for more information."
      })
    }) %>%
      debounce(750)

    output$tlg_output <- render_fn({
      req(tlg_list(), entries_per_page(), current_page())

      num_plots <- length(tlg_list())
      page_end <- current_page() * entries_per_page()
      page_start <- page_end - entries_per_page() + 1
      if (page_end > num_plots) page_end <- num_plots

      unname(tlg_list()[page_start:page_end])
    })

    options_values <- lapply(names(options), \(option) {
      if (is.character(options[[option]])) return(NULL)
      fn <- get(str_glue("tlg_option_{options[[option]]$type}_server"))
      fn(option, options[[option]], data, reactive(input$reset_widgets))
    }) %>%
      setNames(names(options)) %>%
      purrr::keep(\(x) !is.null(x)) %>%
      do.call(reactiveValues, .)

    #' creates widgets responsible for custimizing the plots
    output$options <- renderUI({
      purrr::imap(options, \(def, id) .tlg_module_edit_widget(session$ns(id), def, data))
    })
  })
}

#' Creates editing widget of appropriate type.
#' @param opt_def Definition of the option
#' @param opt_id  Id of the option
#' @param session Session object for namespacing the widgets
#' @returns Shiny widget with appropriate type, label and options
.tlg_module_edit_widget <- function(opt_id, opt_def, data) {
  if (grepl(".group_label", opt_id)) {
    return(tags$h1(opt_def, class = "tlg-group-label"))
  }
  ui_fn <- get(str_glue("tlg_option_{opt_def$type}_ui"))
  ui_fn(opt_id, opt_def, data)
}
