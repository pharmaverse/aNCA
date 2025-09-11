#' NCA Slope selector module handling slope selection via interactive plots and tables.
#'
#' Generates appropriate interface and gathers user input from the table, as well as interactive
#' plotly plots, with with user can define inclusions and exclusions for the data.
#'
#' @param id ID of the module.
#' @param pknca_data `PKNCAdata` object with data to base the plots on.
#' @param manual_slopes_override Reactive experssion with override for the manual slopes selection.
#'                               If changes are detected, current settings will be overwritten with
#'                               values from that reactive.
#'
#' @returns List with reactive expressions:
#'   * manual_slopes - Data frame containing inclusions / exclusions.
#'   * profiles_per_subject - Grouping for each subject.
#'   * slopes_pknca_groups - Grouping for the slopes, in accordance to the settings.

slope_selector_ui <- function(id) {
  ns <- NS(id)
  assets <- system.file("shiny/www", package = "aNCA")

  div(
    class = "slope-selector-module",
    manual_slopes_table_ui(ns("manual_slopes")),
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
        )
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
    observeEvent(list(processed_pknca_data()), {
      req(processed_pknca_data())

      # Prepare a standard PKNCA object with only the basic adjustments for the half-life plots
      # and only over the user selected analytes, profiles and specimens
      new_pknca_data <- processed_pknca_data()
      new_pknca_data$intervals <- new_pknca_data$intervals %>%
        filter(type_interval == "main", half.life) %>%
        unique()
      excl_hl_col <- new_pknca_data$conc$columns$exclude_half.life
      incl_hl_col <- new_pknca_data$conc$columns$include_half.life
      is_new_data <- !is.null(new_pknca_data$conc$data) && is.null(pknca_data()$conc$data)
browser()
      if (is_new_data) {
        # Prepare a default list with the half life plots
        browser()
        plot_outputs(get_halflife_plot(new_pknca_data))
      } else {
        is_change_in_conc_data <- !isTRUE(
          all.equal(
            dplyr::select(new_pknca_data$conc$data, -any_of(c(excl_hl_col, incl_hl_col))),
            dplyr::select(pknca_data()$conc$data, -any_of(c(excl_hl_col, incl_hl_col)))
          )
        )
        is_change_in_hl_adj <- !isTRUE(
          all.equal(
            dplyr::select(new_pknca_data$conc$data, any_of(c(excl_hl_col, incl_hl_col))),
            dplyr::select(pknca_data()$conc$data, any_of(c(excl_hl_col, incl_hl_col)))
          )
        )
        is_change_in_selected_intervals <- !isTRUE(
          all.equal(new_pknca_data$intervals, pknca_data()$intervals)
        )

        if (is_change_in_conc_data) {
          plot_outputs(get_halflife_plot(new_pknca_data))
        } else if (is_change_in_hl_adj) {
          browser()
          affected_groups <- anti_join(
            dplyr::select(new_pknca_data$conc$data, any_of(c(group_vars(new_pknca_data), "NCA_PROFILE", excl_hl_col, incl_hl_col))),
            dplyr::select(pknca_data()$conc$data, any_of(c(group_vars(pknca_data()), "NCA_PROFILE", excl_hl_col, incl_hl_col))),
            by = c(group_vars(new_pknca_data), "NCA_PROFILE", excl_hl_col, incl_hl_col)
          ) %>%
            select(any_of(c(group_vars(new_pknca_data), "NCA_PROFILE"))) %>%
            distinct()
          new_plots <- .update_plots_with_pknca(new_pknca_data, plot_outputs(), affected_groups)
          plot_outputs(new_plots)
        } else if (is_change_in_selected_intervals) {
          browser()
          new_intervals <- anti_join(new_pknca_data$intervals, pknca_data()$intervals)
          rm_intervals <- anti_join(pknca_data()$intervals, new_pknca_data$intervals)
          if (nrow(new_intervals) > 0) {
            affected_groups <- new_intervals %>%
              select(any_of(c(group_vars(new_pknca_data), "NCA_PROFILE"))) %>%
              distinct()
            new_plots <- .update_plots_with_pknca(new_pknca_data, plot_outputs(), affected_groups)
            plot_outputs(new_plots)
          }
          if (nrow(rm_intervals) > 0) {
            rm_plot_names <- rm_intervals %>%
              select(any_of(c(group_vars(new_pknca_data), "start", "end"))) %>%
              # Create a column that is just a pasted character with the name and value of each column in the row
              distinct() %>%
              mutate(across(everything(), as.character)) %>%
              # Create a column that is just a pasted character with the name and value of each column in the row
              mutate(id = purrr::pmap_chr(
                .,
                function(...) {
                  vals <- list(...)
                  paste0(names(vals), ": ", vals, collapse = ", ")
                }
              )) %>%
              pull(id)

            remaining_plots <- plot_outputs()[!names(plot_outputs()) %in% rm_plot_names]
            plot_outputs(remaining_plots)
          }
        }
      }

      # Update the search options if the options actually changed
      if (is_new_data || is_change_in_conc_data || is_change_in_selected_intervals) {
        updateSelectInput(
          session = session,
          inputId = "search_subject",
          label = "Search Subject",
          choices = unique(new_pknca_data$intervals$USUBJID)
        )
      }

      # Update the object
      pknca_data(new_pknca_data)
    })

    # Get grouping columns for plots and tables
    slopes_pknca_groups <- reactive({
      req(pknca_data())

      pknca_data()$intervals %>%
        select(any_of(c(group_vars(pknca_data()), "NCA_PROFILE")))

      # relevant_group_cols <- pknca_data()$conc$data %>%
      #   select(
      #     any_of(
      #       c(group_vars(pknca_data()), "NCA_PROFILE")
      #     )
      #   ) %>%
      #   # TODO (Gerardo): Include a better new version of select_minimal_grouping_cols
      #   # Select columns that have more than 1 unique value
      #   select(where(~ n_distinct(.) > 1)) %>%
      #   colnames()

      # pknca_data()$intervals %>%
      #   select(any_of(relevant_group_cols))
    })

    # HACK: workaround to avoid plotly_click not being registered warning
    session$userData$plotlyShinyEventIDs <- "plotly_click-A"


    # --- Pagination and search logic using the new module ---
    search_subject_r <- reactive({ input$search_subject })
    plots_per_page_r <- reactive({ input$plots_per_page })

    page_search <- page_and_searcher_server(
      id = "page_and_searcher",
      search_subject = search_subject_r,
      plot_outputs = plot_outputs,
      plots_per_page = plots_per_page_r,
      ns_parent = ns
    )

    # Render only the plots requested by the user (using the module's outputs)
    observe({
      req(plot_outputs())
      output$slope_plots_ui <- renderUI({
        shinyjs::enable(selector = ".btn-page")
        plot_outputs()[page_search$has_plot_subject()][page_search$page_start():page_search$page_end()]
      })
    })

    # Creates an initial version of the manual slope adjustments table with pknca_data
    # and handles the addition and deletion of rows through the UI
    slopes_table <- manual_slopes_table_server("manual_slopes", pknca_data)
    manual_slopes <- slopes_table$manual_slopes
    refresh_reactable <- slopes_table$refresh_reactable

    # Define the click events for the point exclusion and selection in the slope plots
    last_click_data <- reactiveVal(NULL)

    observeEvent(event_data("plotly_click", priority = "event"), {
      log_trace("slope_selector: plotly click detected")

      result <- handle_plotly_click(
        last_click_data,
        manual_slopes,
        event_data("plotly_click"),
        pknca_data(),
        plot_outputs()
      )
      # Update reactive values in the observer
      last_click_data(result$last_click_data)
      plot_outputs(result$plot_outputs)
      manual_slopes(result$manual_slopes)
      # render rectable anew #
      shinyjs::runjs("memory = {};") # needed to properly reset reactable.extras widgets
      refresh_reactable(refresh_reactable() + 1)
    })

    #' Separate event handling updating displayed reactable upon every change (adding and removing
    #' rows, plots selection, edits). This needs to be separate call, since simply re-rendering
    #' the table would mean losing focus on text inputs when entering values.
    manual_slopes_version <- reactiveValues(lst = NULL, current = NULL)
    observeEvent(manual_slopes(), {
      req(manual_slopes())
      print("observeEvent manual_slopes()")

      # Update reactable with rules
      reactable::updateReactable(
        outputId = "manual_slopes",
        data = manual_slopes()
      )
# 
#       # Update slopes version
#       manual_slopes_version$lst <- manual_slopes_version$current
#       manual_slopes_version$current <- manual_slopes()
# 
#       # Update only the plots affected by the changed rules
#       req(plot_outputs())
#       if (!is.null(manual_slopes_version$lst)) {
#         browser()
#         print(manual_slopes_version$current)
#         print(manual_slopes_version$lst)
#         rules_added <- anti_join(
#           manual_slopes_version$current,
#           manual_slopes_version$lst,
#           by = names(manual_slopes())
#         )
# 
#         rules_removed <- anti_join(
#           manual_slopes_version$lst,
#           manual_slopes_version$current,
#           by = names(manual_slopes())
#         )
# 
#         slopes_to_update <- bind_rows(rules_added, rules_removed) %>%
#           select(any_of(c(group_vars(pknca_data()), "NCA_PROFILE"))) %>%
#           distinct()
#       } else {
#         # This will do the default udpate (all slopes in the manual slopes table)
#         slopes_to_update <- NULL
#       }
# 
#       plot_outputs(
#           .update_plots_with_rules(pknca_data(), manual_slopes(), plot_outputs(), slopes_to_update)
#         )
#       print("updated plots")
#       print(names(plot_outputs()))
    })

    #' If any settings are uploaded by the user, overwrite current rules
    observeEvent(manual_slopes_override(), {
      req(manual_slopes_override())

      if (nrow(manual_slopes_override()) == 0) return(NULL)

      log_debug_list("Manual slopes override:", manual_slopes_override())

      override_valid <- apply(manual_slopes_override(), 1, function(r) {
        dplyr::filter(
          plot_data()$conc$data,
          PCSPEC == r["PCSPEC"],
          USUBJID == r["USUBJID"],
          PARAM == r["PARAM"],
          NCA_PROFILE == r["NCA_PROFILE"],
          DOSNOA == r["DOSNOA"]
        ) |>
          NROW() != 0
      }) |>
        all()

      if (!override_valid) {

        msg <- "Manual slopes not compatible with current data, leaving as default."
        log_warn(msg)
        showNotification(msg, type = "warning", duration = 5)
        return(NULL)
      }

      manual_slopes(manual_slopes_override())
    })

    #' return reactive with slope exclusions data to be displayed in Results -> Exclusions tab
    list(
      manual_slopes = manual_slopes
    )
  })
}
