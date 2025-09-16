
#' Slope Selector Module (Server/UI)
#'
#' This module manages the half life point selection or exclusion via plots and a table UI.
#' It coordinates the display and update of plots and the manual slopes table.
#'
#' --- Reactivity Flow Schema ---
#'
#' processed_pknca_data (input, from parent)
#'   └─> slope_selector_server
#'        ├──> plot_outputs (reactive, updated by processed_pknca_data changes)
#'        └──> ├> handle_plotly_click (updates manual_slopes on plot click)
#'             └> handle_table_edits (updates manual_slopes on user edits & setting overrides)
#'               └─> manual_slopes (output, used by parent to update processed_pknca_data)
#'
#'
#' @param id Character. Shiny module id.
#' @param processed_pknca_data Reactive. PKNCAdata object for plotting and table context.
#' @param manual_slopes_override Reactive. Optional custom settings override for the slopes table.
#' @return manual_slopes (data.frame of user slope inclusions/exclusions)
#'
#' @details
#' - The module's main output is the manual_slopes table, which is updated by user
#'   edits in the table UI (handle_table_edits) or by plot clicking (handle_plotly_click).
#' - The parent module (setup.R) uses manual_slopes to update processed_pknca_data,
#'   which is then fed back in this module to update plots.

slope_selector_ui <- function(id) {
  ns <- NS(id)
  assets <- system.file("shiny/www", package = "aNCA")

  div(
    class = "slope-selector-module",
    handle_table_edits_ui(ns("manual_slopes")),
    # Help widget #
    dropdown(
      div(
        tags$h1("Slope selector guide"),
        p("
            Upon initial NCA run, the plots will display the optimal slope selection.
            However, you have the flexibility to change it. Remember to apply your
            changes once you are done!
          "),
        div(class = "gif-grid",
          div(
            class = "gif-container",
            tags$h1("Check"),
            img(src = "images/slope_plot_check.gif", alt = "Check")
          ),
          div(
            class = "gif-container",
            tags$h1("Zoom"),
            img(src = "images/slope_plot_zoom.gif", alt = "Zoom")
          ),
          div(
            class = "gif-container",
            tags$h1("Select"),
            img(src = "images/slope_plot_select.gif", alt = "Select")
          ),
          div(
            class = "gif-container",
            tags$h1("Exclude"),
            img(src = "images/slope_plot_exclude.gif", alt = "Exclude")
          )
        )
      ),
      style = "unite",
      right = TRUE,
      icon = icon("question"),
      status = "primary"
    ),
    # Widgets for manipulating plots display #
    fluidRow(
      class = "plot-widgets-container",
      div(
        class = "plot-widget-group",
        selectInput(
          ns("plots_per_page"),
          "Plots per page:",
          choices = c(1, 2, 3, 4, 6, 8, 9, 12, 16),
          selected = 1
        )
      ),
      div(
        class = "plot-widget-group",
        selectInput(
          ns("search_subject"),
          label = "Search Subject",
          choices = NULL,
          multiple = TRUE
        ),
      )
    ),
    fluidRow(
      orderInput(
        ns("order_groups"),
        label = "Order plots by:",
        items = NULL
      )
    ),
    br(),
    # Plots display #
    uiOutput(ns("slope_plots_ui"), class = "slope-plots-container"),
    br(),
    # Use the new pagination UI module
    page_and_searcher_page_ui(ns("page_and_searcher")),
    br()
  )
}

slope_selector_server <- function( # nolint
  id, processed_pknca_data, manual_slopes_override
) {
  moduleServer(id, function(input, output, session) {
    log_trace("{id}: Attaching server")

    ns <- session$ns

    pknca_data <- reactiveVal(NULL)
    plot_outputs <- reactiveVal(NULL)

    observeEvent(processed_pknca_data(), {
      req(processed_pknca_data())

      new_pknca_data <- processed_pknca_data()
      new_pknca_data$intervals <- new_pknca_data$intervals %>%
        filter(type_interval == "main", half.life) %>%
        unique()
      changes <- detect_pknca_data_changes(
        old = pknca_data(),
        new = new_pknca_data,
        excl_hl_col = new_pknca_data$conc$columns$exclude_half.life,
        incl_hl_col = new_pknca_data$conc$columns$include_half.life
      )

      if (changes$in_data) {
        # New data or major changes: regenerate all plots
        plot_outputs(get_halflife_plots(new_pknca_data)[["plots"]])
      } else if (changes$in_hl_adj) {
        # Modify plots with new half-life adjustments (inclusions/exclusions)
        plot_outputs(handle_hl_adj_change(new_pknca_data, pknca_data(), plot_outputs()))
      } else if (changes$in_selected_intervals) {
        # Add/remove plots based on intervals (analyte, profile, specimen selection from setup.R)
        plot_outputs(handle_interval_change(new_pknca_data, pknca_data(), plot_outputs()))
      }

      # Update the searching widget choices based on the new data
      if (changes$in_data | changes$in_selected_intervals) {
        updateSelectInput(
          session = session,
          inputId = "search_subject",
          label = "Search Subject",
          choices = unique(new_pknca_data$intervals$USUBJID)
        )
      }
      if (changes$in_data) {
        updateOrderInput(
          session = session,
          inputId = "order_groups",
          items = group_vars(new_pknca_data)
        )
      }
      # Save the plots for the zip download (nca_results.R)
      session$userData$results$slope_selector <- plot_outputs()

      # Update the object for future comparisons
      pknca_data(new_pknca_data)
    })

    # HACK: workaround to avoid plotly_click not being registered warning
    session$userData$plotlyShinyEventIDs <- "plotly_click-A"

    # --- Pagination and search logic ---
    search_subject_r <- reactive(input$search_subject)
    plots_per_page_r <- reactive(input$plots_per_page)

    # Call the pagination/searcher module to:
    # - Providing indices of plots for the selected subject(s)
    # - Providing indices for which plots to display based on pagination
    page_search <- page_and_searcher_server(
      id = "page_and_searcher",
      search_subject = search_subject_r,
      plot_outputs = plot_outputs,
      plots_per_page = plots_per_page_r
    )

    # Render only the plots requested by the user
    observe({
      req(plot_outputs())
      output$slope_plots_ui <- renderUI({
        shinyjs::enable(selector = ".btn-page")
        plot_outputs() %>%
          # Filter plots based on user search
          .[page_search$is_plot_searched()] %>%
          # Arrange plots by the specified group order
          arrange_plots_by_groups(input$order_groups) %>%
          # Display only the plots for the current page
          .[page_search$page_start():page_search$page_end()]
      })
    })

    # Creates an initial version of the manual slope adjustments table with pknca_data
    # and handles the addition and deletion of rows through the UI
    slopes_table <- handle_table_edits_server("manual_slopes", pknca_data, manual_slopes_override)
    manual_slopes <- slopes_table$manual_slopes
    refresh_reactable <- slopes_table$refresh_reactable

    # Define the click events for the point exclusion and selection in the slope plots
    last_click_data <- reactiveVal(NULL)
    observeEvent(event_data("plotly_click", priority = "event"), {
      log_trace("slope_selector: plotly click detected")

      click_result <- handle_plotly_click(
        last_click_data,
        manual_slopes,
        event_data("plotly_click"),
        pknca_data()
      )
      # Update reactive values: last click & manual slopes table
      last_click_data(click_result$last_click_data)
      manual_slopes(click_result$manual_slopes)

      # render rectable anew #
      shinyjs::runjs("memory = {};") # needed to properly reset reactable.extras widgets
      refresh_reactable(refresh_reactable() + 1)
    })

    #' Separate event handling updating displayed reactable upon every change (adding and removing
    #' rows, plots selection, edits). This needs to be separate call, since simply re-rendering
    #' the table would mean losing focus on text inputs when entering values.
    observeEvent(manual_slopes(), {
      req(manual_slopes())

      # Update reactable with rules
      reactable::updateReactable(
        outputId = "manual_slopes",
        data = manual_slopes()
      )
    })
    #' returns half life adjustments rules to update processed_pknca_data in setup.R
    list(
      manual_slopes = manual_slopes
    )
  })
}
