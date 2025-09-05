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
        pickerInput(ns("select_page"), "", choices = c(),
                    width = "100px"),
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
    ),
    br()
  )
}

slope_selector_server <- function( # nolint
  id, adnca_data, selected_analytes, selected_profile, selected_pcspec, manual_slopes_override
) {
  moduleServer(id, function(input, output, session) {
    log_trace("{id}: Attaching server")

    ns <- session$ns
    
    pknca_data <- reactiveVal(NULL)
    plot_outputs <- reactiveVal(NULL)
    observeEvent(list(adnca_data(), selected_analytes, selected_profile, selected_pcspec), {
      req(adnca_data(), selected_analytes, selected_pcspec, selected_profile)

      browser()

      # Prepare a standard PKNCA object with only the basic adjustments for the half-life plots
      # and only over the user selected analytes, profiles and specimens
      pknca_data <- PKNCA_update_data_object(
        adnca_data = adnca_data(),
        selected_analytes = selected_analytes,
        selected_profile = selected_profile,
        selected_pcspec = selected_pcspec,
        params = "half.life",
        # The next parameters should not matter for the calculations
        # So reactivity should not be involved
        auc_data = data.frame(
          start_auc = NA_real_,
          end_auc = NA_real_
        ),
        method = "lin up/log down",
        # TODO (Gerardo): This would better be FALSE, but start changes...
        should_impute_c0 = TRUE,
        hl_adj_rules = manual_slopes()
      )
      pknca_data(pknca_data)

      # Prepare a default list with the half life plots
      plot_outputs(get_halflife_plot(pknca_data()))
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
    current_page <- reactiveVal(1)

    #' updating current page based on user input
    observeEvent(input$next_page, {
      current_page(current_page() + 1)
      shinyjs::disable(selector = ".btn-page")
    })
    observeEvent(input$previous_page, {
      if (current_page() == 1) return(NULL)
      current_page(current_page() - 1)
      shinyjs::disable(selector = ".btn-page")
    })
    observeEvent(input$select_page, current_page(as.numeric(input$select_page)))
    observeEvent(list(input$plots_per_page, input$search_subject), current_page(1))

    #' Updating plot outputUI, dictating which plots get displayed to the user.
    #' Scans for any related reactives (page number, subject filter etc) and updates the plot output
    #' UI to have only plotlyOutput elements for desired plots.
    observeEvent(list(
      plot_outputs(), input$plots_per_page, input$search_subject, current_page()
    ), {
      req(plot_outputs())
      log_trace("{id}: Updating displayed plots")

      # Decide
      # Make sure the search_subject input is not NULL
      search_subject <- {
        if (is.null(input$search_subject) || length(input$search_subject) == 0) {
          subject_col <- pknca_data()$conc$columns$subject
          unique(
            pknca_data()$intervals %>%
              filter(half.life) %>%
              .[[subject_col]]
          )
        } else {
          input$search_subject
        }
      }

      has_plot_subject <- grepl(
        paste0("USUBJID: ", "(", paste0(search_subject, collapse = ")|("), ")"),
        names(plot_outputs())
      )

      # find which plots should be displayed based on page #
      num_plots <- sum(has_plot_subject)
      plots_per_page <- as.numeric(input$plots_per_page)
      num_pages <- ceiling(num_plots / plots_per_page)

      if (current_page() > num_pages) {
        current_page(current_page() - 1)
        return(NULL)
      }

      page_end <- current_page() * plots_per_page
      page_start <- page_end - plots_per_page + 1
      if (page_end > num_plots) page_end <- num_plots

      # update page number display #
      output$page_number <- renderUI(num_pages)

      # Render only the plots requested by the user
      output$slope_plots_ui <- renderUI({
        shinyjs::enable(selector = ".btn-page")
        plot_outputs()[has_plot_subject][page_start:page_end]
      })

      # update jump to page selector #
      updatePickerInput(
        session = session,
        inputId = "select_page",
        choices = 1:num_pages,
        selected = current_page()
      )

      # update plot display #
      shinyjs::toggleClass(
        selector = ".slope-plots-container",
        class = "multiple",
        condition = plots_per_page != 1
      )

      # disable buttons if necessary #
      shinyjs::toggleState(id = ns("previous_page"), condition = current_page() == 1)
      shinyjs::toggleState(id = ns("next_page"), condition = current_page() == num_pages)
    })

    #' Rendering slope plots based on nca data.
    observeEvent(pknca_data(), {
      req(pknca_data())

      log_trace("{id}: Rendering plots")
      # Update the subject search input to make available choices for the user
      updateSelectInput(
        session = session,
        inputId = "search_subject",
        label = "Search Subject",
        choices = unique(pknca_data()$intervals$USUBJID)
      )
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

      # Update slopes version
      manual_slopes_version$lst <- manual_slopes_version$current
      manual_slopes_version$current <- manual_slopes()

      # Update only the plots affected by the changed rules
      req(plot_outputs())
      if (!is.null(manual_slopes_version$lst)) {
        browser()
        print(manual_slopes_version$current)
        print(manual_slopes_version$lst)
        rules_added <- anti_join(
          manual_slopes_version$current,
          manual_slopes_version$lst,
          by = names(manual_slopes())
        )

        rules_removed <- anti_join(
          manual_slopes_version$lst,
          manual_slopes_version$current,
          by = names(manual_slopes())
        )

        slopes_to_update <- bind_rows(rules_added, rules_removed) %>%
          select(any_of(c(group_vars(pknca_data()), "NCA_PROFILE"))) %>%
          distinct()
      } else {
        # This will do the default udpate (all slopes in the manual slopes table)
        slopes_to_update <- NULL
      }

      plot_outputs(
          .update_plots_with_rules(pknca_data(), manual_slopes(), plot_outputs(), slopes_to_update)
        )
      print("updated plots")
      print(names(plot_outputs()))
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
