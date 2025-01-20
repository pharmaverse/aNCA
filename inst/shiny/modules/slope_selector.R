slope_selector_ui <- function(id) {
  ns <- NS(id)
  assets <- system.file("shiny/www", package = "aNCA")

  div(
    class = "slope-selector-module",
    includeCSS(file.path(assets, "slope_selector.css")),
    fluidRow(
      # Selection and exclusion controls #
      actionButton(ns("add_rule"), "+ Exclusion/Selection", class = "btn-success"),
      actionButton(ns("remove_rule"), "- Remove selected rows", class = "btn-warning"),
      actionButton(ns("save_ruleset"), tags$b("Apply"), class = "btn-primary"),
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
            div(class = "gif-container",
              tags$h1("Check"),
              img(src = "images/slope_plot_check.gif", alt = "Check")
            ),
            div(class = "gif-container",
              tags$h1("Zoom"),
              img(src = "images/slope_plot_zoom.gif", alt = "Zoom")
            ),
            div(class = "gif-container",
              tags$h1("Select"),
              img(src = "images/slope_plot_select.gif", alt = "Select")
            ),
            div(class = "gif-container",
              tags$h1("Exclude"),
              img(src = "images/slope_plot_exclude.gif", alt = "Exclude")
            )
          )
        ),
        style = "unite",
        right = TRUE,
        icon = icon("question"),
        status = "primary"
      )
    ),
    # Table with selections and exclusions #
    fluidRow(
      reactableOutput(ns("manual_slopes")),
    ),
    # Widgets for manipulating plots display #
    fluidRow(
      class = "plot-widgets-container",
      div(
        tags$span(
          class = "inline-select-input",
          tags$label(
            "Plots per page: "
          ),
          selectInput(
            ns("plots_per_page"),
            "",
            choices = c(1, 2, 4, 6, 8, 10),
            selected = 1
          )
        )
      ),
      div(
        pickerInput(
          ns("search_patient"),
          label = "Search Patient",
          choices = NULL,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
      ),
      div(align = "left", actionButton(ns("previous_page"), "Previous Page", class = "btn-page")),
      div(
        align = "center",
        tags$span(
          class = "inline-select-input",
          tags$span("Page "),
          selectInput(ns("select_page"), "", choices = c()),
          tags$span(" out of "),
          uiOutput(ns("page_number"), inline = TRUE)
        )
      ),
      div(align = "right", actionButton(ns("next_page"), "Next Page", class = "btn-page"))
    ),
    # Plots display #
    uiOutput(ns("slope_plots_ui"), class = "slope-plots-container", style = "height:70%;"),
  )
}

.SLOPE_SELECTOR_COLUMNS <- c("TYPE", "PATIENT", "ANALYTE", "PCSPEC", "PROFILE", "IXrange", "REASON")

slope_selector_server <- function(
  id, mydata, res_nca, profiles_per_patient, cycle_nca, analyte_nca, pcspec_nca, pk_nca_trigger, settings_upload
) {
  moduleServer(id, function(input, output, session) {
    log_trace("{id}: Attaching server")

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
        dplyr::select(USUBJID, ANALYTE, PCSPEC, DOSNO) %>%
        unique() %>%
        dplyr::arrange(USUBJID, ANALYTE, PCSPEC, DOSNO)

      num_plots <- nrow(patient_profile_plot_ids)

      # find which plots should be displayed based on page #
      plots_per_page <- as.numeric(input$plots_per_page)
      page_end <- current_page() * plots_per_page
      page_start <- page_end - plots_per_page + 1
      if (page_end > num_plots) page_end <- num_plots

      plots_to_render <- dplyr::slice(ungroup(patient_profile_plot_ids), page_start:page_end)

      plot_outputs <- apply(plots_to_render, 1, function(row) {
        lambda_slope_plot(
          res_nca()$result,
          plot_data()$conc$data,
          row["DOSNO"],
          row["USUBJID"],
          row["ANALYTE"],
          row["PCSPEC"],
          0.7
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

      # TODO(mateusz): do we need to update the `profiles_per_patient()` reactiveVal?
      # or can we just have it locally?

      # Define the profiles selected (dosno) that each patient (usubjid) has
      profiles_per_patient(
        res_nca()$result %>%
          mutate(USUBJID = as.character(USUBJID),
                 DOSNO = as.character(DOSNO)) %>%
          group_by(USUBJID, ANALYTE, PCSPEC) %>%
          summarise(DOSNO = unique(DOSNO), .groups = "drop") %>%
          unnest(DOSNO)  # Convert lists into individual rows
      )

      # Update the patient search input to make available choices for the user
      updatePickerInput(
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
        PATIENT = character(),
        ANALYTE = character(),
        PCSPEC = character(),
        PROFILE = character(),
        IXrange = character(),
        REASON = character()
      )
    })

    row_counter <- reactiveVal(0)

    #' Adds new row to the selection/exclusion datatable
    observeEvent(input$add_rule, {
      log_trace("{id}: adding manual slopes row")

      new_row <- data.frame(
        TYPE = "Selection",
        PATIENT = as.character(unique(profiles_per_patient()$USUBJID)[1]),
        ANALYTE = unique(profiles_per_patient()$ANALYTE)[1],
        PCSPEC = unique(profiles_per_patient()$PCSPEC)[1],
        PROFILE = as.character(unique(profiles_per_patient()$DOSNO)[1]),
        IXrange = "1:3",
        REASON = "",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      updated_data <- rbind(manual_slopes(), new_row)
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

      # enclosing this in isolate() so the table is not re-rendered on every edit #
      isolate({
        data <- manual_slopes()
      })

      reactable(
        data = data,
        defaultColDef = colDef(
          align = "center"
        ),
        columns = list(
          TYPE = colDef(
            cell = dropdown_extra(
              id = session$ns("edit_TYPE"),
              choices = c("Selection", "Exclusion"),
              class = "dropdown-extra"
            ),
            minWidth = 75
          ),
          PATIENT = colDef(
            cell = dropdown_extra(
              id = session$ns("edit_PATIENT"),
              choices = unique(profiles_per_patient()$USUBJID),
              class = "dropdown-extra"
            ),
            minWidth = 75
          ),
          ANALYTE = colDef(
            cell = dropdown_extra(
              id = session$ns("edit_ANALYTE"),
              choices = unique(profiles_per_patient()$ANALYTE),
              class = "dropdown-extra"
            ),
            minWidth = 75
          ),
          PCSPEC = colDef(
            cell = dropdown_extra(
              id = session$ns("edit_PCSPEC"),
              choices = unique(profiles_per_patient()$PCSPEC),
              class = "dropdown-extra"
            ),
            minWidth = 75
          ),
          PROFILE = colDef(
            cell = dropdown_extra(
              id = session$ns("edit_PROFILE"),
              choices = unique(profiles_per_patient()$DOSNO),
              class = "dropdown-extra"
            ),
            minWidth = 75
          ),
          IXrange = colDef(
            cell = text_extra(
              id = session$ns("edit_IXrange")
            ),
            minWidth = 75
          ),
          REASON = colDef(
            cell = text_extra(
              id = session$ns("edit_REASON")
            ),
            minWidth = 300
          )
        ),
        selection = "multiple",
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
    purrr::walk(.SLOPE_SELECTOR_COLUMNS, \(colname) {
      observeEvent(input[[paste0("edit_", colname)]], {
        edit <- input[[paste0("edit_", colname)]]
        edited_slopes <- manual_slopes()
        edited_slopes[edit$row, edit$column] <- edit$value
        manual_slopes(edited_slopes)
      })
    })

    # Observe input$nca
    observeEvent(profiles_per_patient(), {
      mydata(.filter_slopes(mydata(), manual_slopes(), profiles_per_patient()))
    })

    #' saves and implements provided ruleset
    observeEvent(input$save_ruleset, {
      mydata(.filter_slopes(mydata(), manual_slopes(), profiles_per_patient()))
      pk_nca_trigger(pk_nca_trigger() + 1)
    })

    #' Plot data is a local reactive copy of full data. The purpose is to display data that
    #' is already adjusted with the applied rules, so that the user can verify added selctions
    #' and exclusions before applying them to the actual dataset.
    plot_data <- reactive({
      req(mydata(), manual_slopes(), profiles_per_patient())
      .filter_slopes(mydata(), manual_slopes(), profiles_per_patient())
    }) %>%
      shiny::debounce(750)

    # Define the click events for the point exclusion and selection in the slope plots
    last_click_data <- reactiveValues(patient = "", profile = "", analyte = "", pcspec =  "", idx_pnt = "")
    observeEvent(event_data("plotly_click", priority = "event"), {
      # Store the information of the last click event #
      click_data <- event_data("plotly_click")

      # If no information is present, exit #
      if (is.null(click_data) || is.null(click_data$customdata))
        return(NULL)

      log_trace("{id}: plotly click with data detected")

      # Get identifiers of the clicked plot #
      patient <- gsub("(.*)_.*_.*", "\\1",  click_data$customdata)
      profile <- gsub(".*_(.*)_.*", "\\1",  click_data$customdata)
      analyte <- gsub(".*_.*_(.*)_.*", "\\1",  click_data$customdata)
      pcspec <- gsub(".*_.*_.*_(.*)", "\\1",  click_data$customdata)
      idx_pnt <- gsub(".*_.*_(.*)", "\\1",  click_data$customdata)

      #' If not data was previously provided, or user clicked on different plot,
      #' update last data and exit
      if (patient != last_click_data$patient || profile != last_click_data$profile) {
        last_click_data$patient <- patient
        last_click_data$profile <- profile
        last_click_data$analyte <- analyte
        last_click_data$pcspec <- pcspec
        last_click_data$idx_pnt <- idx_pnt
        return(NULL)
      }

      # If valid selection is provided, construct new row
      new_slope_rule <- data.frame(
        TYPE = if (idx_pnt != last_click_data$idx_pnt) "Selection" else "Exclusion",
        PATIENT = as.character(patient),
        ANALYTE = analyte,
        PCSPEC = pcspec,
        PROFILE = as.character(profile),
        IXrange = paste0(last_click_data$idx_pnt, ":", idx_pnt),
        REASON = "[Graphical selection. Click here to include a reason]"
      )

      # Check if there is any overlap with existing rules, adda new or edit accordingly
      new_manual_slopes <- .check_slope_rule_overlap(manual_slopes(), new_slope_rule)

      manual_slopes(new_manual_slopes)

      # after adding new rule, reset last click data #
      last_click_data$patient <- ""
      last_click_data$profile <- ""
      last_click_data$analyte <- ""
      last_click_data$pcspec <- ""
      last_click_data$idx_pnt <- ""

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
        summarise(IXrange = .compress_range(IX), .groups = "keep") %>%
        select(TYPE, PATIENT, ANALYTE, PCSPEC, PROFILE, IXrange, REASON) %>%
        na.omit()

      manual_slopes(imported_slopes)
    })

    #' return reactive with slope exclusions data to be displayed in Results -> Exclusions tab
    return(reactive({
      manual_slopes()
    }))
  })
}
