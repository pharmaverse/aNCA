slope_selector_ui <- function(id) {
  ns <- NS(id)
  assets <- system.file("shiny/www", package = "aNCA")

  div(
    class = "slope-selector-module",
    includeCSS(file.path(assets, "slope_selector.css")),
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
          choices = c(1, 2, 4, 8, 16),
          selected = 1
        )
      ),

      div(
        class = "plot-widget-group",
        selectInput(
          ns("search_patient"),
          label = "Search Patient",
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


slope_selector_server <- function(
  id, pknca_data, res_nca,
  pk_nca_trigger, settings_upload
) {
  moduleServer(id, function(input, output, session) {
    log_trace("{id}: Attaching server")

    ns <- session$ns

    #Get grouping columns for plots and tables
    slopes_groups <- reactive({
      req(pknca_data())

      pknca_data()$conc$columns$groups %>%
        purrr::list_c() %>%
        append("DOSNO") %>%
        purrr::keep(\(col) {
          !is.null(col) && col != "DRUG" && length(unique(pknca_data()$conc$data[[col]])) > 1
        })
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
      current_page(current_page() - 1)
      shinyjs::disable(selector = ".btn-page")
    })
    observeEvent(input$select_page, current_page(as.numeric(input$select_page)))
    observeEvent(list(input$plots_per_page, input$search_patient), current_page(1))

    # Generate dynamically the minimum results you need for the lambda plots
    lambdas_res <- reactive({
      req(pknca_data())
      pknca_data <- pknca_data()

      if (!"type_interval" %in% names(pknca_data$intervals)) {
        NULL
      } else {
        all_params <- names(PKNCA::get.interval.cols())
        result_obj <- PKNCA::pk.nca(data = pknca_data, verbose = FALSE)
        result_obj$result <- result_obj$result %>%
          mutate(start_dose = start, end_dose = end)

        result_obj
      }
    }) |>
      bindEvent(pknca_data())

    # Profiles per Patient ----
    # Define the profiles per patient
    profiles_per_patient <- reactive({
      req(lambdas_res())

      lambdas_res()$result %>%
        mutate(USUBJID = as.character(USUBJID),
               DOSNO = as.character(DOSNO)) %>%
        group_by(!!!syms(unname(unlist(lambdas_res()$data$conc$columns$groups)))) %>%
        summarise(DOSNO = unique(DOSNO), .groups = "drop") %>%
        unnest(DOSNO)  # Convert lists into individual rows

    })

    #' Updating plot outputUI, dictating which plots get displayed to the user.
    #' Scans for any related reactives (page number, patient filter etc) and updates the plot output
    #' UI to have only plotlyOutput elements for desired plots.
    observeEvent(list(
      plot_data(), lambdas_res(), input$plots_per_page, input$search_patient, current_page()
    ), {
      req(lambdas_res())
      log_trace("{id}: Updating displayed plots")

      # Make sure the search_patient input is not NULL
      search_patient <- {
        if (is.null(input$search_patient) || length(input$search_patient) == 0) {
          unique(lambdas_res()$result$USUBJID)
        } else {
          input$search_patient
        }
      }

      # create plot ids based on available data #
      patient_profile_plot_ids <- pknca_data()$intervals %>%
        select(any_of(c(unname(unlist(pknca_data()$dose$columns$groups)),
                        unname(unlist(pknca_data()$conc$columns$groups)),
                        "DOSNO"))) %>%
        filter(USUBJID %in% search_patient) %>%
        select(slopes_groups(), USUBJID) %>%
        unique() %>%
        arrange(USUBJID)

      num_plots <- nrow(patient_profile_plot_ids)

      # find which plots should be displayed based on page #
      plots_per_page <- as.numeric(input$plots_per_page)
      page_end <- current_page() * plots_per_page
      page_start <- page_end - plots_per_page + 1
      if (page_end > num_plots) page_end <- num_plots

      plots_to_render <- slice(ungroup(patient_profile_plot_ids), page_start:page_end)

      plot_outputs <- apply(plots_to_render, 1, function(row) {

        lambda_slope_plot(
          conc_pknca_df = plot_data()$conc$data,
          row_values = as.list(row),
          myres = lambdas_res(),
          r2adj_threshold = 0.7
        ) |>
          htmlwidgets::onRender(
            # nolint start
            "function(el, x) {
              const plotlyElements = $('.slope-selector-module .plotly.html-widget.html-fill-item.html-widget-static-bound.js-plotly-plot');
              plotlyElements.css('height', '100%');
              plotlyElements.css('aspect-ratio', '1');
              window.dispatchEvent(new Event('resize'));
            }"
            # nolint end
          )
      })


      output$slope_plots_ui <- renderUI({
        shinyjs::enable(selector = ".btn-page")
        plot_outputs
      })

      # update page number display #
      num_pages <- ceiling(num_plots / plots_per_page)
      output$page_number <- renderUI(num_pages)

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
    observeEvent(res_nca(), {
      req(
        res_nca(),
        profiles_per_patient()
      )
      log_trace("{id}: Rendering plots")
      # Update the patient search input to make available choices for the user
      updateSelectInput(
        session = session,
        inputId = "search_patient",
        label = "Search Patient",
        choices = unique(res_nca()$result$USUBJID)
      )
    })

    slopes_table <- manual_slopes_table_server("manual_slopes", pknca_data,
                                               profiles_per_patient, slopes_groups, pk_nca_trigger)

    manual_slopes <- slopes_table$manual_slopes
    refresh_reactable <- slopes_table$refresh_reactable

    #' Plot data is a local reactive copy of full data. The purpose is to display data that
    #' is already adjusted with the applied rules, so that the user can verify added selections
    #' and exclusions before applying them to the actual dataset.
    plot_data <- reactive({
      req(pknca_data(), manual_slopes(), profiles_per_patient())
      filter_slopes(pknca_data(), manual_slopes(), profiles_per_patient(), slopes_groups())
    }) %>%
      shiny::debounce(750)

    # Define the click events for the point exclusion and selection in the slope plots
    last_click_data <- reactiveValues()

    observeEvent(slopes_groups(), {
      # Reinitialize dynamic columns when slopes_groups changes
      for (col in tolower(slopes_groups())) {
        last_click_data[[col]] <- ""
      }
      last_click_data$idx_pnt <- ""
    })

    observeEvent(event_data("plotly_click", priority = "event"), {
      log_trace("slope_selector: plotly click detected")

      result <- handle_plotly_click(last_click_data,
                                    manual_slopes,
                                    slopes_groups(),
                                    event_data("plotly_click"))
      # Update reactive values in the observer
      last_click_data <- result$last_click_data
      manual_slopes(result$manual_slopes)
      # render rectable anew #
      shinyjs::runjs("memory = {};") # needed to properly reset reactable.extras widgets
      refresh_reactable(refresh_reactable() + 1)
    })

    #' If any settings are uploaded by the user, overwrite current rules
    observeEvent(settings_upload(), {
      req(settings_upload()$datapath)

      #' TODO(mateusz): This is suboptimal, as currently the .csv file is read twice (once in the
      #' nca.R file, and second time here). Ideally, file should be loaded once and then relevant
      #' info passed to appropriate modules. This should be reworked once the application is
      #' modularized and improved further.
      setts <- read.csv(settings_upload()$datapath)
      imported_slopes <- setts %>%
        select(TYPE, USUBJID, ANALYTE, PCSPEC, DOSNO, IX, REASON) %>%
        mutate(PATIENT = as.character(USUBJID), PROFILE = as.character(DOSNO)) %>%
        group_by(TYPE, PATIENT, ANALYTE, PCSPEC, PROFILE, REASON) %>%
        summarise(RANGE = .compress_range(IX), .groups = "keep") %>%
        select(TYPE, PATIENT, ANALYTE, PCSPEC, PROFILE, RANGE, REASON) %>%
        na.omit()

      manual_slopes(imported_slopes)
    })

    #' return reactive with slope exclusions data to be displayed in Results -> Exclusions tab
    list(
      manual_slopes = manual_slopes,
      profiles_per_patient = profiles_per_patient,
      slopes_groups = slopes_groups
    )
  })
}
