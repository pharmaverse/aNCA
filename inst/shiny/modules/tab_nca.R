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
#' @param extra_group_vars Column name(s) of the additional variable options for input widgets
#'
#' @returns `res_nca` reactive with results data object.
tab_nca_ui <- function(id) {
  ns <- NS(id)

  navset_card_pill(
    id = ns("nca_navset"),
    nav_panel(
      "Setup",
      fluid = TRUE,
      actionButton(
        inputId = ns("run_nca"),
        label = "Run NCA",
        icon = icon("play"),
        class = "btn btn-primary",
        width = "100%"
      ),
      setup_ui(ns("nca_setup")),
    ),
    #' Results
    nav_panel(
      "Results", fluid = TRUE,
      navset_pill_list(
        nca_results_ui(ns("nca_results")),
        nav_panel(
          "Slopes Information",
          navset_pill(
            nav_panel("Slopes Results", reactable_ui(ns("slope_results"))),
            nav_panel("Manual Adjustments", reactable_ui(ns("manual_slopes"))),
          )
        ),
        nav_panel(
          "Descriptive Statistics", descriptive_statistics_ui(ns("descriptive_stats"))
        ),
        nav_panel("Parameter Datasets", parameter_datasets_ui(ns("parameter_datasets"))),
        nav_panel("Parameter Plots", parameter_plots_ui(ns("parameter_plots")))
      )
    ),
    #' Additional analysis
    nav_panel(
      "Additional Analysis",
      additional_analysis_ui(ns("non_nca"))
    )
  )
}

tab_nca_server <- function(id, pknca_data, extra_group_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #' Setup session-wide object for storing data units. Units can be edited by the user on
    #' various steps of the workflow (pre- and post-NCA calculation) and the whole application
    #' should respect the units, regardless of location.
    session$userData$units_table <- reactiveVal(NULL)

    adnca_data <- reactive(pknca_data()$conc$data)

    # #' NCA Setup module
    nca_setup <- setup_server("nca_setup", adnca_data, pknca_data)

    processed_pknca_data <- nca_setup$processed_pknca_data
    settings <- nca_setup$settings
    session$userData$settings <- settings # This will be saved in the results zip folder
    ratio_table <- nca_setup$ratio_table
    slope_rules <- nca_setup$slope_rules

    reactable_server("manual_slopes", slope_rules$manual_slopes)

    # List all irrelevant warnings to suppres in the NCA calculation
    irrelevant_regex_warnings <- c(
      "No intervals for data$",
      "^Too few points for half-life"
    )

    #' Triggers NCA analysis, creating res_nca reactive
    res_nca <- reactive({
      req(processed_pknca_data())

      if (all(!unlist(processed_pknca_data()$intervals[sapply(processed_pknca_data()$intervals,
                                                              is.logical)]))) {
        log_error("Invalid parameters")
        showNotification("No suitable parameters selected for NCA calculation.
         Please go back and select parameters suitable for the data.",
                         type = "error", duration = NULL)
        return(NULL)
      }

      loading_popup("Calculating NCA results...")

      log_info("Calculating NCA results...")

      tryCatch({
        # Create env for storing PKNCA run warnings, so that warning messages can be appended
        # from within warning handler without bleeding to global env.
        pknca_warn_env <- new.env()
        pknca_warn_env$warnings <- c()

        # Update units table
        processed_pknca_data <- processed_pknca_data()
        if (!is.null(session$userData$units_table())) {
          processed_pknca_data$units <- session$userData$units_table()
        }

        #' Calculate results
        res <- withCallingHandlers({
          processed_pknca_data %>%
            filter_slopes(
              slope_rules$manual_slopes(),
              slope_rules$profiles_per_subject(),
              slope_rules$slopes_groups(),
              check_reasons = TRUE
            ) %>%
            # Perform PKNCA parameter calculations
            PKNCA_calculate_nca() %>%
            # Add bioavailability results if requested
            add_f_to_pknca_results(settings()$bioavailability) %>%
            # Apply standard CDISC names
            mutate(
              PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD")
            ) %>%
            # Apply flag rules to mark results in the `exclude` column
            PKNCA_hl_rules_exclusion(
              rules = isolate(settings()$flags) %>%
                purrr::keep(\(x) x$is.checked) %>%
                purrr::map(\(x) x$threshold)
            ) %>%
            # Add parameter ratio calculations
            calculate_table_ratios_app(ratio_table = ratio_table())
        },
        warning = function(w) {
          if (!grepl(paste(irrelevant_regex_warnings, collapse = "|"),
                     conditionMessage(w))) {
            pknca_warn_env$warnings <- append(pknca_warn_env$warnings, conditionMessage(w))
          }
          invokeRestart("muffleWarning")
        })

        # Display unique warnings thrown by PKNCA run.
        purrr::walk(unique(pknca_warn_env$warnings), function(w) {
          w_message <- paste0("PKNCA run produced a warning: ", w)
          log_warn(w_message)
          showNotification(w_message, type = "warning", duration = 5)
        })

        updateTabsetPanel(session, "nca_navset", selected = "Results")

        log_success("NCA results calculated.")

        # Reshape intervals, filter
        params_not_requested <- res$data$intervals %>%
          select(any_of(setdiff(names(PKNCA::get.interval.cols()), c("start", "end")))) %>%
          # For all logical columns, mutate FALSE to NA
          mutate(across(where(is.logical), ~ ifelse(.x, TRUE, NA))) %>%
          # Only select column that are only NA
          select(where(~ all(is.na(.x)))) %>%
          names()

        # Filter for requested params based on intervals
        res$result <- res$result %>%
          filter(!PPTESTCD %in% translate_terms(params_not_requested, "PKNCA", "PPTESTCD"))

        res
      }, error = function(e) {
        log_error("Error calculating NCA results:\n{conditionMessage(e)}")
        showNotification(.parse_pknca_error(e), type = "error", duration = NULL)
        NULL
      }, finally = {
        # Delay the removal of loading modal to give it enough time to render
        later::later(~shiny::removeModal(session = session), delay = 0.5)
      })
    }) %>%
      bindEvent(input$run_nca)

    #' Show slopes results
    pivoted_slopes <- reactive({
      req(res_nca())
      pivot_wider_pknca_results(res_nca()) %>%
        select(
          any_of(c("USUBJID", "ATPTREF", "PARAM", "PCSPEC")),
          starts_with("LAMZ"),
          starts_with("lambda.z"),
          starts_with("R2ADJ"),
          "Exclude"
        )
    })

    reactable_server(
      "slope_results",
      pivoted_slopes,
      download_buttons = c("csv", "xlsx"),
      file_name = function() paste0("NCA_Slope_Results_", Sys.Date()),
      defaultPageSize = 10,
      showPageSizeOptions = TRUE,
      pageSizeOptions = reactive(c(10, 50, nrow(pivoted_slopes()))),
      style = list(fontSize = "0.75em")
    )

    #' Prepares and displays the pivoted NCA results
    nca_results_server(
      "nca_results", processed_pknca_data, res_nca, settings, ratio_table, extra_group_vars
    )

    #' Descriptive statistics module
    descriptive_statistics_server("descriptive_stats", res_nca, extra_group_vars)

    #' Additional analysis module
    additional_analysis_server("non_nca", processed_pknca_data, extra_group_vars)

    #' Parameter datasets module
    parameter_datasets_server("parameter_datasets", res_nca)

    #' Parameter plots module
    parameter_plots_server("parameter_plots", res_nca)

    # return results for use in other modules
    list(res_nca = res_nca, processed_pknca_data = processed_pknca_data)
  })
}

#'
#' Parses error from results calculation pipeline into a html-formatted message ready
#' to be displayed in the interface.
#'
#' @param e Error object.
#' @returns String with html-formatted error message.
.parse_pknca_error <- function(e) {
  msg <- conditionMessage(e)

  if (grepl("pk.calc.", msg)) {
    # Handle errors from PKNCA package.
    param_of_error <- gsub(".*'pk\\.calc\\.(.*)'.*", "\\1", msg)
    msg <- paste0(
      "Problem in calculation of NCA parameter: ",
      param_of_error,
      "<br><br>", msg,
      "<br><br>If the error is unexpected, please report a bug."
    )

  } else if (grepl("^No reason provided", msg)) {
    # Handle no reason provided erros from the calculation function.
    msg <- paste(msg, "<br><br>Please provide the reason in Setup > Slope Selector tab.")

  } else {
    # Handle unknown error
    msg <- paste0(
      "Unknown error detected when calculating NCA results,",
      " please inspect the logs and report a bug."
    )
  }

  HTML(gsub("\\\n", "<br>", msg))
}
