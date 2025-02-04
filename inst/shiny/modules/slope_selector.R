slope_selector_ui <- function(id) {
  ns <- NS(id)
  assets <- system.file("shiny/www", package = "aNCA")

  div(
    class = "slope-selector-module",
    includeCSS(file.path(assets, "slope_selector.css")),
    fluidRow(
      class = "slopes-widgets-container",
      # Selection and exclusion controls #
      div(
        class = "plot-widget-group",
        actionButton(ns("add_rule"), "+ Exclusion/Selection", class = "btn-success")
      ),
      div(
        class = "plot-widget-group",
        actionButton(ns("remove_rule"), "- Remove selected rows", class = "btn-warning")
      ),
      div(
        class = "plot-widget-group",
        actionButton(ns("save_ruleset"), tags$b("Apply"), class = "btn-primary")
      )
    ),
    # Table with selections and exclusions #
    fluidRow(
      reactableOutput(ns("manual_slopes")),
    ),
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
          choices = c(1, 2, 4, 6, 8, 10),
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
    fluidRow(
      class = "plot-widgets-container",
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
        selectInput(ns("select_page"), "", choices = c(),
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
  id, mydata, res_nca, profiles_per_patient,
  cycle_nca, analyte_nca, pcspec_nca,
  pk_nca_trigger, settings_upload
) {
  moduleServer(id, function(input, output, session) {
    log_trace("{id}: Attaching server")

    #Get grouping columns for plots and tables
    slopes_groups <- reactive({
      req(mydata())

      mydata()$conc$columns$groups %>%
        purrr::list_c() %>%
        append("DOSNO") %>%
        purrr::keep(\(col) {
          !is.null(col) && col != "DRUG" && length(unique(mydata()$conc$data[[col]])) > 1
        })
    })

    # Reactive for SLOPE_SELECTOR_COLUMNS
    SLOPE_SELECTOR_COLUMNS <- reactive({
      req(slopes_groups())
      c("TYPE", slopes_groups(), "RANGE", "REASON")
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

    #' Updating plot outputUI, dictating which plots get displayed to the user.
    #' Scans for any related reactives (page number, patient filter etc) and updates the plot output
    #' UI to have only plotlyOutput elements for desired plots.
    observeEvent(list(
      plot_data(), res_nca(), input$plots_per_page, input$search_patient, current_page()
    ), {

      req(res_nca())
      log_trace("{id}: Updating displayed plots")

      # Make sure the search_patient input is not NULL
      search_patient <- {
        if (is.null(input$search_patient) || length(input$search_patient) == 0) {
          unique(res_nca()$result$USUBJID)
        } else {
          input$search_patient
        }
      }

      # create plot ids based on available data #
      patient_profile_plot_ids <- mydata()$conc$data %>%
        filter(
          DOSNO %in% cycle_nca,
          ANALYTE %in% analyte_nca,
          PCSPEC %in% pcspec_nca,
          USUBJID %in% search_patient
        ) %>%
        select(slopes_groups()) %>%
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
          res_nca()$result,
          plot_data()$conc$data,
          row_values = as.list(row),
          R2ADJTHRESHOL = 0.7,
          res_nca()
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
      updateSelectInput(
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
      shinyjs::toggleState(id = "previous_page", condition = current_page() > 1)
      shinyjs::toggleState(id = "next_page", condition = current_page() < num_pages)
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

    #' Object for storing exclusion and selection data for lambda slope calculation
    manual_slopes <- reactiveVal({
      data.frame(
        TYPE = character(),
        RANGE = character(),
        REASON = character(),
        stringsAsFactors = FALSE
      )
    })

    observeEvent(mydata(), {

      current_slopes <- manual_slopes()
      # Add missing dynamic columns with default values (e.g., NA_character_)
      for (col in slopes_groups()) {
        if (!col %in% colnames(current_slopes)) {
          current_slopes[[col]] <- character()
        }
      }

      # Update the reactive Val
      manual_slopes(current_slopes)
    })

    row_counter <- reactiveVal(0)

    #' Adds new row to the selection/exclusion datatable
    observeEvent(input$add_rule, {
      log_trace("{id}: adding manual slopes row")

      # Create a named list for dynamic columns based on `profiles_per_patient`
      dynamic_values <- lapply(slopes_groups(), function(col) {
        value <- as.character(unique(profiles_per_patient()[[col]]))
        if (length(value) > 0) value[1] else NA_character_  # Handle empty or NULL cases
      })

      names(dynamic_values) <- slopes_groups()

      # Create the new row with both fixed and dynamic columns
      new_row <- as.data.frame(c(
        dynamic_values,
        TYPE = "Selection",
        RANGE = "1:3",
        REASON = ""
      ), stringsAsFactors = FALSE)

      updated_data <- as.data.frame(rbind(manual_slopes(), new_row), stringsAsFactors = FALSE)
      manual_slopes(updated_data)
    })

    #' Removes selected row
    observeEvent(input$remove_rule, {
      log_trace("{id}: removing manual slopes row")

      selected <- getReactableState("manual_slopes", "selected")
      req(selected)
      edited_slopes <- manual_slopes()[-selected, ]
      manual_slopes(edited_slopes)
    })


    #' Render manual slopes table
    refresh_reactable <- reactiveVal(1)
    output$manual_slopes <- renderReactable({

      log_trace("{id}: rendering slope edit data table")
      # Isolate to prevent unnecessary re-renders on every edit
      isolate({
        data <- manual_slopes()
      })

      # Fixed columns (TYPE, RANGE, REASON)
      fixed_columns <- list(
        TYPE = colDef(
          cell = dropdown_extra(
            id = session$ns("edit_TYPE"),
            choices = c("Selection", "Exclusion"),
            class = "dropdown-extra"
          ),
          width = 200
        ),
        RANGE = colDef(
          cell = text_extra(
            id = session$ns("edit_RANGE")
          )
        ),
        REASON = colDef(
          cell = text_extra(
            id = session$ns("edit_REASON")
          ),
          width = 300
        )
      )

      # Dynamic group column definitions
      dynamic_columns <- lapply(slopes_groups(), function(col) {
        colDef(
          cell = dropdown_extra(
            id = session$ns(paste0("edit_", col)),
            choices = unique(profiles_per_patient()[[col]]), # Dynamically set choices
            class = "dropdown-extra"
          )
        )
      })
      names(dynamic_columns) <- slopes_groups()

      # Combine columns in the desired order
      all_columns <- c(
        dynamic_columns,
        list(
          TYPE = fixed_columns$TYPE,
          RANGE = fixed_columns$RANGE,
          REASON = fixed_columns$REASON
        )
      )

      # Render reactable
      reactable(
        data = data,
        defaultColDef = colDef(
          align = "center"
        ),
        columns = all_columns,
        selection = "multiple",
        defaultExpanded = TRUE,
        borderless = TRUE,
        theme = reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        )
      )
    }) %>%
      shiny::bindEvent(refresh_reactable())

    #' Separate event handling updating displayed reactable upon every change (adding and removing
    #' rows, plots selection, edits). This needs to be separate call, since simply re-rendering
    #' the table would mean losing focus on text inputs when entering values.
    observeEvent(manual_slopes(), {
      reactable::updateReactable(
        outputId = "manual_slopes",
        data = manual_slopes()
      )
    })

    #' For each of the columns in slope selector data frame, attach an event that will read
    #' edits for that column made in the reactable.
    observe({
      req(SLOPE_SELECTOR_COLUMNS())
      # Dynamically attach observers for each column
      purrr::walk(SLOPE_SELECTOR_COLUMNS(), \(colname) {
        observeEvent(input[[paste0("edit_", colname)]], {
          edit <- input[[paste0("edit_", colname)]]
          edited_slopes <- manual_slopes()
          edited_slopes[edit$row, edit$column] <- edit$value
          manual_slopes(edited_slopes)
        })
      })
    })

    # Observe input$nca
    observeEvent(profiles_per_patient(), {
      mydata(.filter_slopes(mydata(), manual_slopes(), profiles_per_patient(), slopes_groups()))
    })

    #' saves and implements provided ruleset
    observeEvent(input$save_ruleset, {
      mydata(.filter_slopes(mydata(), manual_slopes(), profiles_per_patient(), slopes_groups()))
      pk_nca_trigger(pk_nca_trigger() + 1)
    })

    #' Plot data is a local reactive copy of full data. The purpose is to display data that
    #' is already adjusted with the applied rules, so that the user can verify added selections
    #' and exclusions before applying them to the actual dataset.
    plot_data <- reactive({
      req(mydata(), manual_slopes(), profiles_per_patient())
      .filter_slopes(mydata(), manual_slopes(), profiles_per_patient(), slopes_groups())
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
      handle_plotly_click(id, last_click_data, manual_slopes, slopes_groups)

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
    return(reactive({
      manual_slopes()
    }))
  })
}
