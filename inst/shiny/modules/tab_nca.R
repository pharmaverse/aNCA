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
    div(
      class = "d-flex justify-content-between",
      actionButton(ns("nca"), "Run NCA", class = "run-nca-btn")
    ),
    navset_tab(
      id = ns("ncapanel"),
      nav_panel(
        "Setup",
        fluid = TRUE,
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
          nav_panel("Descriptive Statistics", descriptive_statistics_ui(ns("descriptive_stats"))),
          nav_panel("Parameter Datasets", parameter_datasets_ui(ns("parameter_datasets"))),
          nav_panel("Parameter Plots", parameter_plots_ui(ns("parameter_plots")))
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
        ############################################################################################
        # TODO: Until PKNCA manages to simplify by default in PPORRESU its volume units,
        # this is implemented here via hardcoding in PPSTRESU
        pknca_object$units <- pknca_object$units %>%
          mutate(
            PPSTRESU = ifelse(
              PPTESTCD %in% metadata_nca_parameters$PKNCA[
                metadata_nca_parameters$unit_type == "volume"
              ],
              sapply(PPSTRESU, \(x) simplify_unit(x, as.character = TRUE)),
              PPSTRESU
            ),
            conversion_factor = ifelse(
              PPTESTCD %in% metadata_nca_parameters$PKNCA[
                metadata_nca_parameters$unit_type == "volume"
              ],
              get_conversion_factor(PPORRESU, PPSTRESU),
              conversion_factor
            )
          )
        ############################################################################################
        log_success("PKNCA data object created.")

        #' Enable related tabs and update the curent view if data is created succesfully.
        purrr::walk(c("nca", "exploration", "tlg"), \(tab) {
          shinyjs::enable(selector = paste0("#page li a[data-value=", tab, "]"))
        })

        pknca_object
      }, error = function(e) {
        log_error(e$message)
        showNotification(e$message, type = "error", duration = NULL)
        NULL
      })
    }) |>
      bindEvent(adnca_data())

    # #' NCA Setup module
    nca_setup <- setup_server("nca_setup", adnca_data, pknca_data)

    processed_pknca_data <- nca_setup$processed_pknca_data
    settings <- nca_setup$settings
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

      withProgress(message = "Calculating NCA...", value = 0, {
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
              PKNCA_calculate_nca() %>%
              # Add bioavailability results if requested
              add_f_to_pknca_results(settings()$bioavailability) %>%
              # Apply standard CDISC names
              mutate(
                PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD")
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

          # Apply flag rules to mark results in the `exclude` column
          current_rules <- isolate(settings()$flags)
          flag_rules_to_apply <- current_rules |>
            purrr::keep(~ .x$is.checked) |>
            purrr::map(~ .x$threshold)
          res <- PKNCA_hl_rules_exclusion(res, flag_rules_to_apply)

          # Display unique warnings thrown by PKNCA run.
          purrr::walk(unique(pknca_warn_env$warnings), \(w) {
            w_message <- paste0("PKNCA run produced a warning: ", w)
            log_warn(w_message)
            showNotification(w_message, type = "warning", duration = 5)
          })

          updateTabsetPanel(session, "ncapanel", selected = "Results")

          log_success("NCA results calculated.")

          # Apply standard CDISC names and return the object
          res %>%
            mutate(PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD"))

        }, error = function(e) {
          log_error("Error calculating NCA results:\n{conditionMessage(e)}")
          showNotification(.parse_pknca_error(e), type = "error", duration = NULL)
          NULL
        })
      })
    }) |>
      bindEvent(input$nca)

    #' Show slopes results
    pivoted_slopes <- reactive({
      req(res_nca())
      pivot_wider_pknca_results(res_nca()) %>%
        select(
          any_of(c("USUBJID", "NCA_PROFILE", "PARAM", "PCSPEC")),
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
      file_name = \() paste0("NCA_Slope_Results_", Sys.Date()),
      defaultPageSize = 10,
      showPageSizeOptions = TRUE,
      pageSizeOptions = reactive(c(10, 50, nrow(pivoted_slopes()))),
      style = list(fontSize = "0.75em")
    )

    #' Prepares and displays the pivoted NCA results
    nca_results_server(
      "nca_results", processed_pknca_data, res_nca, settings, ratio_table, grouping_vars
    )

    #' Descriptive statistics module
    descriptive_statistics_server("descriptive_stats", res_nca, grouping_vars)

    #' Additional analysis module
    additional_analysis_server("non_nca", processed_pknca_data, grouping_vars)

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
