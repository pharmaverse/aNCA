# NCA TAB -----
# This module contains nested modules and all the ui and server code for the NCA tab
# NCA UI function ----
tab_nca_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    actionButton(ns("nca"), "Run NCA", class = "run-nca-btn"),
    download_settings_ui(ns("download_settings")),
    navset_tab(
      id = ns("ncapanel"),
      #' Pre-nca setup
      nav_panel(
        "Setup",
        fluid = TRUE,
        navset_pill_list(
          nav_panel("NCA settings", nca_setup_ui(ns("nca_settings"))),
          nav_panel("Slope Selector", slope_selector_ui(ns("slope_selector")))
        )
      ),
      #' Results
      nav_panel(
        "Results", fluid = TRUE,
        navset_pill_list(
          nca_results_ui(ns("nca_results")),
          nav_panel(
            "Slopes Information",
            navset_pill(
              nav_panel("Slopes Results", DTOutput(ns("preslopesettings"))),
              nav_panel("Manual Adjustments", tableOutput(ns("manual_slopes2"))),
            )
          ),
          nav_panel("Descriptive Statistics", descriptive_statistics_ui(ns("descriptive_stats"))),
          nav_panel("Parameter Datasets", parameter_datasets_ui(ns("parameter_datasets")))
        )
      ),
      #' Additional analysis
      nav_panel("Additional Analysis", additional_analysis_ui(ns("non_nca")))
    )
  )
}

# NCA Server Function ----
# Requires processed data and grouping vars from tab_data module
tab_nca_server <- function(id, adnca_data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #' Initializes PKNCA::PKNCAdata object from pre-processed adnca data
    pknca_data <- reactive({
      req(adnca_data())
      log_trace("Creating PKNCA::data object.")

      PKNCA_create_data_object(adnca_data())
    }) |>
      bindEvent(adnca_data())

    #' NCA Setup module
    nca_setup <- nca_setup_server("nca_settings", adnca_data, pknca_data)
    processed_pknca_data <- nca_setup$processed_pknca_data
    units_table <- nca_setup$units_table
    rules <- nca_setup$rules

    #' Slope rules setup module
    slope_rules <- slope_selector_server(
      "slope_selector",
      processed_pknca_data,
      res_nca,
      pk_nca_trigger,
      reactive(input$settings_upload)
    )

    output$preslopesettings <- DT::renderDataTable({
      req(res_nca())
      pivot_wider_pknca_results(res_nca()) %>%
        select(
          any_of(c("USUBJID", "DOSNO", "ANALYTE", "PCSPEC")),
          starts_with("lambda.z"),
          starts_with("span.ratio"),
          starts_with("half.life"),
          "Exclude"
        ) %>%
        DT::datatable(
          extensions = "FixedHeader",
          options = list(scrollX = TRUE, scrollY = "80vh",
                         lengthMenu = list(c(10, 25, -1), c("10", "25", "All")),
                         pageLength = -1, fixedHeader = TRUE)
        ) %>%
        formatStyle("Exclude", target = "row",
                    backgroundColor = styleEqual(NA, NA, default = "#f5b4b4"))
    })

    output$manual_slopes2 <- renderTable({
      slope_rules()
    })

    # SLOPE SELECTOR ----
    # Keep the UI table constantly actively updated
    observeEvent(input, {
      req(processed_pknca_data())
      dynamic_columns <- c(
        setdiff(unname(unlist(processed_pknca_data()$conc$columns$groups)), "DRUG"),
        "DOSNO"
      )
      for (input_name in grep(
        paste0("(", paste(c(dynamic_columns, "TYPE", "RANGE", "REASON"),
                          collapse = "|"), ")_Ex\\d+$"),
        names(input), value = TRUE
      )) {
        observeEvent(input[[input_name]], {
          # Get the ID of the exclusion
          id <- gsub("_(Ex\\d+)$", "", input_name)

          # Update the reactive list of exclusion IDs
          manual_slopes <- manual_slopes()
          set_selected_value(
            manual_slopes[manual_slopes$id == id, ], paste0(input[[input_name]])
          ) <- manual_slopes[manual_slopes$id == id, ]
          manual_slopes(manual_slopes)

        })
      }
    })

    #' Triggers NCA analysis, creating res_nca reactive val
    pk_nca_trigger <- reactiveVal(0)
    observeEvent(input$nca, {
      pk_nca_trigger(pk_nca_trigger() + 1)
    })
    res_nca <- reactive({
      req(processed_pknca_data())

      withProgress(message = "Calculating NCA...", value = 0, {
        tryCatch({
          #' Calculate results
          res <- PKNCA_calculate_nca(processed_pknca_data())

          #' Apply units
          if (!is.null(units_table())) {
            res$data$units <- units_table()
            res$result <- res$result %>%
              select(-PPSTRESU, -PPSTRES) %>%
              left_join(
                units_table(),
                by = intersect(names(.), names(units_table()))
              ) %>%
              mutate(PPSTRES = PPORRES * conversion_factor) %>%
              select(-conversion_factor)
          }

          updateTabsetPanel(session, "ncapanel", selected = "Results")

          res
        }, error = function(e) {
          # TODO: this does not catch errors properly
          full_error <- e$parent$message
          if (grepl("pk.calc.", x = full_error)) {
            param_of_error <- gsub(".*'pk\\.calc\\.(.*)'.*", "\\1", full_error)
            full_error <- paste0("Problem in calculation of NCA parameter: ", param_of_error,
                                 "<br><br>", full_error)
          }
          modified_error <- gsub("Please report a bug.\n:", "", x = full_error, fixed = TRUE) %>%
            paste0("<br><br>If the error is unexpected, please report a bug.")
          showNotification(HTML(modified_error), type = "error", duration = NULL)
        })
      })
    }) |>
      bindEvent(pk_nca_trigger())

    nca_results_server("nca_results", res_nca, rules(), grouping_vars)

    # TODO: This will be removed in #241
    # https://github.com/pharmaverse/aNCA/pull/241
    profiles_per_patient <- reactive({
      req(processed_pknca_data())
      # Check if res_nca() is available and valid
      if (!is.null(res_nca())) {
        res_nca()$result %>%
          mutate(USUBJID = as.character(USUBJID),
                 DOSNO = as.character(DOSNO)) %>%
          group_by(!!!syms(unname(unlist(processed_pknca_data()$conc$columns$groups)))) %>%
          summarise(DOSNO = unique(DOSNO), .groups = "drop") %>%
          unnest(DOSNO)  # Convert lists into individual rows
      } else {
        processed_pknca_data()$conc$data %>%
          mutate(USUBJID = as.character(USUBJID)) %>%
          group_by(!!!syms(unname(unlist(processed_pknca_data()$conc$columns$groups)))) %>%
          summarise(DOSNO = list(unique(DOSNO)), .groups = "drop")
      }
    })

    #' Descriptive statistics module
    descriptive_statistics_server("descriptive_stats", res_nca, grouping_vars)

    #' Settings download module
    download_settings_server("download_settings", processed_pknca_data, res_nca)

    #' Additional analysis module
    additional_analysis_server("non_nca", processed_pknca_data, grouping_vars)

    #' Parameter datasets module
    parameter_datasets_server("parameter_datasets", res_nca)

    # return results for use in other modules
    res_nca
  })
}
