#' Page and Searcher UI (pagination controls only)
#'
#' This UI module provides the page navigation controls (previous, next, selector, number).
#' The search_subject input remains outside for now in the parent (slope_selector.R)
page_and_searcher_page_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    class = "plot-widgets-container-2",
    div(
      class = "plot-widget-group",
      actionButton(
        ns("previous_page"),
        "Previous Page",
        class = "btn-page"
      )
    ),
    div(
      class = "plot-widget-group",
      tags$span("Page "),
      pickerInput(ns("select_page"), "", choices = c(), width = "100px"),
      tags$span("of "),
      uiOutput(ns("page_number"), inline = TRUE)
    ),
    div(
      class = "plot-widget-group",
      actionButton(
        ns("next_page"),
        "Next Page",
        class = "btn-page"
      )
    )
  )
}

#' Page and Searcher Server
#'
#' Handles pagination and subject search logic for displaying plots.
#' Outputs: list of reactives: page_start, page_end, is_plot_searched, num_pages
page_and_searcher_server <- function(id, search_subject, plot_outputs, plots_per_page) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Internalize current page state
    current_page <- reactiveVal(1)

    # Calculate is_plot_searched
    is_plot_searched <- reactive({
      req(plot_outputs())
      search_val <- search_subject()
      if (is.null(search_val) || length(search_val) == 0) {
        rep(TRUE, length(names(plot_outputs())))
      } else {
        grepl(
          paste0("USUBJID=(", paste0(search_val, collapse = ")|("), ")"),
          names(plot_outputs())
        )
      }
    })

    num_plots <- reactive(sum(is_plot_searched()))
    plots_per_page_num <- reactive(as.numeric(plots_per_page()))
    num_pages <- reactive(ceiling(num_plots() / plots_per_page_num()))

    # Navigation events
    observeEvent(input$next_page, {
      if (current_page() < num_pages()) {
        current_page(current_page() + 1)
      }
      shinyjs::disable(selector = ".btn-page")
    })
    observeEvent(input$previous_page, {
      if (current_page() > 1) {
        current_page(current_page() - 1)
      }
      shinyjs::disable(selector = ".btn-page")
    })
    observeEvent(input$select_page, {
      val <- as.numeric(input$select_page)
      if (!is.na(val)) current_page(val)
    })
    observeEvent(list(plots_per_page(), search_subject()), {
      current_page(1)
    })

    page_end <- reactive({
      min(current_page() * plots_per_page_num(), num_plots())
    })
    page_start <- reactive({
      max(page_end() - plots_per_page_num() + 1, 1)
    })

    # UI outputs for page number and page selector
    output$page_number <- renderUI(num_pages())
    observe({
      updatePickerInput(
        session = session,
        inputId = "select_page",
        choices = 1:num_pages(),
        selected = current_page()
      )
    })
    observe({
      shinyjs::toggleState(id = "previous_page", condition = current_page() == 1)
      shinyjs::toggleState(id = "next_page", condition = current_page() == num_pages())
    })
    observe({
      shinyjs::toggleClass(
        selector = ".slope-plots-container",
        class = "multiple",
        condition = plots_per_page_num() != 1
      )
    })

    list(
      page_start = page_start,
      page_end = page_end,
      is_plot_searched = is_plot_searched
    )
  })
}
