#' Module handling NCA analysis pipeline.
#'
#' @details
#' Handles raw data processing into `PKNCA` objects, runs the NCA analysis and displays
#' results in various formats.
#' 1. `pknca_data` object is created with the help of `PKNCA` package.
#' 2. With `nca_setup.R` and `slope_selector.R` modules, starting settings are gathered and applied
#'    to the `pknca_data`, creating `processed_data` object.
#' 3. `processed_data` is then used to calculate `res_nca` (the actual results of the analysis),
#'    when the user is happy with the settings.
#' 4. Results and other data object are passed to various sub-modules for further post-processing
#'    and display, including modules like `nca_results.R`, `parameter_datasets.R`,
#'    `descriptive_statistics.R` and `additional_analysis.R`
#'
#'
#' @param id           ID of the module.
#' @param adnca_data   Raw ADNCA data uploaded by the user, with any mapping and filters applied.
#' @param grouping_vars A character vector with grouping variables for the analysis.
#'
#' @returns `res_nca` reactive with results data object.
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
              nav_panel("Slopes Results", DTOutput(ns("slope_results"))),
              nav_panel("Manual Adjustments", tableOutput(ns("manual_slopes"))),
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

tab_nca_server <- function(id, adnca_data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #' Setup session-wide object for storing data units. Units can be edited by the user on
    #' various steps of the workflow (pre- and post-NCA calculation) and the whole application
    #' should respect the units, regardless of location.
    session$userData$units_table <- reactiveVal(NULL)

    #' Initializes PKNCA::PKNCAdata object from pre-processed adnca data
    pknca_data <- reactive({
      req(adnca_data())
      log_trace("Creating PKNCA::data object.")

      tryCatch({
        #' Create data object
        pknca_object <- PKNCA_create_data_object(adnca_data())
        log_success("PKNCA data object created.")

        #' Enable related tabs and update the curent view if data is created succesfully.
        purrr::walk(c("nca", "visualisation", "tlg"), \(tab) {
          shinyjs::enable(selector = paste0("#page li a[data-value=", tab, "]"))
        })

        pknca_object
      }, error = function(e) {
        log_error(e$message)
        showNotification(e$message, type = "error", duration = NULL)
        return(NULL)
      })
    }) |>
      bindEvent(adnca_data())

    #' NCA Setup module
    nca_setup <- nca_setup_server("nca_settings", adnca_data, pknca_data)
    processed_pknca_data <- nca_setup$processed_pknca_data
    rules <- nca_setup$rules
    auc_options <- nca_setup$bioavailability

    #' Slope rules setup module
    slope_rules <- slope_selector_server(
      "slope_selector",
      processed_pknca_data,
      res_nca,
      pk_nca_trigger,
      reactive(input$settings_upload)
    )

    output$manual_slopes <- renderTable(slope_rules$manual_slopes())

    #' Triggers NCA analysis, creating res_nca reactive
    pk_nca_trigger <- reactiveVal(0)
    observeEvent(input$nca, pk_nca_trigger(pk_nca_trigger() + 1))
    res_nca <- reactive({
      req(processed_pknca_data())

      withProgress(message = "Calculating NCA...", value = 0, {
        log_info("Calculating NCA results...")
        tryCatch({
          #' Calculate results
          res <- processed_pknca_data() %>%
            filter_slopes(
              slope_rules$manual_slopes(),
              slope_rules$profiles_per_patient(),
              slope_rules$slopes_groups()
            ) %>%
            PKNCA_calculate_nca()

          #' Apply units
          if (!is.null(session$userData$units_table())) {
            res$data$units <- session$userData$units_table()
            res$result <- res$result %>%
              select(-PPSTRESU, -PPSTRES) %>%
              left_join(
                session$userData$units_table(),
                by = intersect(names(.), names(session$userData$units_table()))
              ) %>%
              mutate(PPSTRES = PPORRES * conversion_factor) %>%
              select(-conversion_factor)
          }

          updateTabsetPanel(session, "ncapanel", selected = "Results")

          log_success("NCA results calculated.")

          res
        }, error = function(e) {
          # TODO: this does not catch errors properly sometimes
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

    #' Show slopes results
    output$slope_results <- DT::renderDataTable({
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

    nca_results_server("nca_results",
                       processed_pknca_data,
                       res_nca, rules(),
                       grouping_vars,
                       auc_options)

    #' Descriptive statistics module
    descriptive_statistics_server("descriptive_stats", res_nca, grouping_vars, auc_options)

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
