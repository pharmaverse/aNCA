
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
#'             └> slopes_table (updates manual_slopes on user edits & setting overrides)
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
#'   edits in the table UI (slopes_table) or by plot clicking (handle_plotly_click).
#' - The parent module (nca_setup.R) uses manual_slopes to update processed_pknca_data,
#'   which is then fed back in this module to update plots.

slope_selector_ui <- function(id) {
  ns <- NS(id)
  assets <- system.file("shiny/www", package = "aNCA")

  div(
    class = "slope-selector-module",
    manual_slopes_table_ui(ns("manual_slopes")),
    # Help widget #
    fluidRow(
      column(
        width = 12,
        dropdown(
          div(
            tags$h2("Slope Selector Help"),
            p("
                Upon the initial NCA run, the plots will showcase the optimal slope selection. 
                However, you have the option to modify it according to your preferences. 
                Please remember to apply your changes once you are done by clicking Run NCA again!
              "),
            div(class = "gif-grid",
              div(
                class = "gif-container",
                tags$h1("Check"),
                tags$h6("Hover the mouse over points to inspect individual samples."),
                img(src = "images/slope_plot_check.gif", alt = "Check")
              ),
              div(
                class = "gif-container",
                tags$h1("Zoom"),
                tags$h6("Click and drag to select and zoom in a specific area.",
                  " Double click to zoom out."
                ),
                img(src = "images/slope_plot_zoom.gif", alt = "Zoom")
              ),
              div(
                class = "gif-container",
                tags$h1("Select"),
                tags$h6("Click the first and then the last point",
                        " you want to include in the slope."),
                img(src = "images/slope_plot_select.gif", alt = "Select")
              ),
              div(
                class = "gif-container",
                tags$h1("Exclude"),
                tags$h6(
                  tags$div("Double click a point to exclude it."),
                  tags$div("Double click it again to include it back.")
                ),
                img(src = "images/slope_plot_exclude.gif", alt = "Exclude")
              )
            )
          ),
          style = "unite",
          right = TRUE,
          icon = icon("question"),
          status = "primary",
          width = "600px"
        ),
      )
    ),
    # Widgets for manipulating plots display #
    fluidRow(
      class = "plot-widgets-container",
      div(
        class = "plot-widget-group",
        selectInput(
          ns("plots_per_page"),
          "Plots per page:",
          choices = c(1, 3, 6, 9, 18, 100),
          selected = 9
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
        items = NULL,
        width = "100%"
      )
    ),
    br(),
    # Plots display #
    uiOutput(ns("slope_plots_ui"), class = "slope-plots-container"),
    br(),
    # Use the new pagination UI module
    page_and_searcher_ui(ns("page_and_searcher")),
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
      # Keep main intervals where half.life or any dependent param is selected.
      # get_halflife_plots() handles the rest (forcing half.life, clearing impute).
      hl_dep_params <- intersect(
        PKNCA::get.parameter.deps("half.life"),
        names(new_pknca_data$intervals)
      )
      new_pknca_data$intervals <- new_pknca_data$intervals %>%
        filter(type_interval == "main") %>%
        filter(half.life | if_any(all_of(hl_dep_params))) %>%
        distinct(across(all_of(
          c(group_vars(new_pknca_data), "start", "end")
        )), .keep_all = TRUE)
      changes <- detect_pknca_data_changes(
        old = pknca_data(),
        new = new_pknca_data,
        reason_col = "REASON"
      )

      if (changes$in_data || changes$in_options) {
        # New data or options changes (e.g. min.hl.points): regenerate all plots
        plot_outputs(get_halflife_plots(
          new_pknca_data, title_vars = "ATPTREF"
        )[["plots"]])
      } else if (changes$in_hl_adj) {
        # Modify plots with new half-life adjustments (inclusions/exclusions)
        plot_outputs(handle_hl_adj_change(new_pknca_data, pknca_data(), plot_outputs()))
      } else if (changes$in_selected_intervals) {
        # Add/remove plots based on intervals (selection from nca_setup.R)
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
      # Update the order input widget options based on the new data
      if (changes$in_data) {
        req(processed_pknca_data())
        # Only use columns that have more than 1 value in the whole dataset
        pknca_data <- processed_pknca_data()
        group_conc_cols <- group_vars(pknca_data)
        group_conc_n_levels <- sapply(pknca_data$conc$data[group_conc_cols], \(x) length(unique(x)))
        group_cols_to_order <- group_conc_cols[group_conc_n_levels > 1]

        updateOrderInput(
          session = session,
          inputId = "order_groups",
          items = group_cols_to_order
        )
      }

      # Save the plots for the zip download (nca_results.R)
      session$userData$results$slope_selector <- plot_outputs()

      # Update the object for future comparisons
      pknca_data(new_pknca_data)
    })

    # Call the pagination/searcher module to:
    # - Providing indices of plots for the selected subject(s)
    # - Providing indices for which plots to display based on pagination
    page_search <- page_and_searcher_server(
      id = "page_and_searcher",
      search_subject = reactive(input$search_subject),
      plot_outputs = plot_outputs,
      plots_per_page = reactive(input$plots_per_page)
    )

    observe({
      req(!is.null(plot_outputs()))
      output$slope_plots_ui <- renderUI({
        if (length(plot_outputs()) == 0) {
          div(
            class = "slope-selector-empty-state",
            icon("info-circle"),
            tags$p(
              "No slope plots to display.",
              "Half-life plots require at least one half-life",
              "related parameter to be selected",
              "(e.g., LAMZHL, LAMZ, R2ADJ, LAMZNPT)."
            )
          )
        } else {
          shinyjs::enable(selector = ".btn-page")
          plot_outputs() %>%
            # Filter plots based on user search
            .[page_search$is_plot_searched()] %>%
            # Arrange plots by the specified group order
            arrange_plots_by_groups(input$order_groups) %>%
            # Display only the plots for the current page
            .[page_search$page_start():page_search$page_end()]
        }
      })
    })

    # Creates an initial version of the manual slope adjustments table with pknca_data
    # and handles the addition and deletion of rows through the UI
    slopes_table <- manual_slopes_table_server("manual_slopes", pknca_data, manual_slopes_override)
    manual_slopes <- slopes_table$manual_slopes
    refresh_reactable <- slopes_table$refresh_reactable

    # Track user-initiated slope changes (buttons, plotly clicks)
    user_changed_slopes <- reactiveVal(FALSE)

    # Define the click events for the point exclusion and selection in the slope plots
    last_click_data <- reactiveVal(NULL)
    observeEvent(event_data("plotly_click", priority = "event"), {
      log_trace("slope_selector: plotly click detected")
      req(plot_outputs())
      click_result <- handle_plotly_click(
        last_click_data,
        manual_slopes,
        event_data("plotly_click"),
        pknca_data()
      )
      # Update reactive values: last click & manual slopes table
      last_click_data(click_result$last_click_data)
      user_changed_slopes(TRUE)
      manual_slopes(click_result$manual_slopes)

      # render rectable anew #
      shinyjs::runjs("memory = {};") # needed to properly reset reactable.extras widgets
      refresh_reactable(refresh_reactable() + 1)
    })

    # Also flag user-initiated changes from add/remove buttons
    observeEvent(slopes_table$refresh_reactable(), {
      user_changed_slopes(TRUE)
    }, ignoreInit = TRUE)

    #' Separate event handling updating displayed reactable upon every change (adding and removing
    #' rows, plots selection, edits). This needs to be separate call, since simply re-rendering
    #' the table would mean losing focus on text inputs when entering values.
    observeEvent(manual_slopes(), {
      req(manual_slopes())

      if (user_changed_slopes()) {
        n_rules <- nrow(manual_slopes())
        n_excl <- sum(manual_slopes()$TYPE == "Exclusion", na.rm = TRUE)
        n_incl <- n_rules - n_excl
        log_info("Slope rules updated: ", n_rules, " total (", n_incl, " selections, ", n_excl, " exclusions)")
        user_changed_slopes(FALSE)
      }

      # Update reactable with rules
      reactable::updateReactable(
        outputId = "manual_slopes",
        data = manual_slopes()
      )

    })
    #' returns half life adjustments rules to update processed_pknca_data in nca_setup.R
    manual_slopes
  })
}
