#' Module handling pre-processing of data.
#'
#' @details
#' Handles user upload or dummy data, filtering, mapping and reviewing. The general pipeline for
#' the module is:
#' 1. Upload raw adnca data (or return a dummy dataset)
#' 2. Filter the raw data based on user input
#' 3. Process the data based on column mapping specifications
#' The module also allows the user to review the data after performing filtering and mapping -
#' the processed data will go further into the analysis pipeline.
#'
#' @returns list containing:
#'   - pknca_data: reactive PKNCAdata object for analysis
#'   - adnca_raw: reactive raw uploaded ADNCA data (or dummy data)
#'   - extra_group_vars: reactive grouping variables from column mapping
#'   - settings_override: reactive uploaded settings (or NULL)
#'   - auto_replay_ready: reactive logical, TRUE when auto-replay data
#'     processing is complete and the app can navigate to the saved tab

#' Abort auto-replay, dismiss loading popup, and show a warning.
#' @param auto_replay ReactiveVal to reset.
#' @param message Warning message to display.
#' @param log_msg Message for the log.
#' @keywords internal
#' @noRd
.abort_auto_replay <- function(auto_replay, message, log_msg) {
  auto_replay(FALSE)
  shiny::removeModal()
  log_warn(log_msg)
  showNotification(message, type = "warning", duration = 10)
}

tab_data_ui <- function(id) {
  ns <- NS(id)

  tabs <- c("Upload", "Mapping", "Filtering", "Preview")

  tagList(
    shinyjs::useShinyjs(),
    div(
      class = "data-tab-container",
      div(
        class = "data-tab-content-container",
        div(
          class = "data-tab-content",
          navset_pill(
            id = ns("data_navset"),
            nav_panel(
              "Upload",
              stepper_ui("Upload", tabs),
              data_upload_ui(ns("raw_data"))
            ),
            nav_panel(
              "Mapping",
              stepper_ui("Mapping", tabs),
              data_mapping_ui(ns("column_mapping"))
            ),
            nav_panel(
              "Filtering",
              stepper_ui("Filtering", tabs),
              data_filtering_ui(ns("data_filtering"))
            ),
            nav_panel(
              "Preview",
              id = ns("data_navset-review"),
              div(
                stepper_ui("Preview", tabs),
                div(
                  uiOutput(ns("processed_data_message")),
                  card(reactable_ui(ns("data_processed")), class = "border-0 shadow-none")
                )
              )
            )
          )
        )
      ),
      div(
        class = "data-tab-btns-container",
        # Left side: Restart button
        actionButton(ns("restart"), "Restart"),
        # Right side: Previous and Next buttons
        div(
          class = "nav-btns",
          actionButton(ns("prev_step"), "Previous", disabled = TRUE),
          actionButton(ns("next_step"), "Next", , class = "btn-primary")
        )
      )
    )
  )
}

tab_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    trigger_mapping_submit <- reactiveVal(0)
    steps <- c("upload", "mapping", "filtering", "preview")
    step_labels <- c("Upload", "Mapping", "Filtering", "Preview")
    data_step <- reactiveVal("upload")

    # Auto-replay state: tracks whether we are auto-advancing through
    # the data pipeline after a settings upload.
    auto_replay <- reactiveVal(FALSE)
    observe({
      current <- data_step()
      if (current == steps[1]) {
        shinyjs::disable("prev_step")
      } else {
        shinyjs::enable("prev_step")
      }
    })
    observeEvent(input$restart, {
      log_info("Application restarting...")
      session$reload()
    })
    observeEvent(input$next_step, {
      shinyjs::disable("next_step") # Disable button on click
      current_step <- isolate(data_step())
      if (current_step %in% c("upload", "filtering")) {
        idx <- match(current_step, steps)
        data_step(steps[idx + 1])
        updateTabsetPanel(session, "data_navset", selected = step_labels[idx + 1])
      } else if (current_step == "mapping") {
        trigger_mapping_submit(trigger_mapping_submit() + 1)
      } else if (current_step == "preview") {
        shinyjs::runjs("document.querySelector(`a[data-value='exploration']`).click();")
      }
    })

    # enable next step after progression
    observe({
      data_step()
      shinyjs::enable("next_step")
    })

    observeEvent(input$prev_step, {
      current <- data_step()
      idx <- match(current, steps)
      if (!is.na(idx) && idx > 1) {
        data_step(steps[idx - 1])
      }
      updateTabsetPanel(session, "data_navset", selected = step_labels[idx - 1])
    })
    #' Load raw ADNCA data
    uploaded_data <- data_upload_server("raw_data")

    # Call the column mapping module
    imported_mapping <- reactive({
      override <- uploaded_data$settings_override()
      list(
        mapping = override$mapping,
        time_duplicate_keys = override$time_duplicate_keys
      )
    })
    column_mapping <- data_mapping_server(
      id = "column_mapping",
      adnca_data = uploaded_data$adnca_raw,
      imported_mapping = imported_mapping,
      trigger = trigger_mapping_submit
    )
    #' Reactive value for the processed dataset
    adnca_mapped <- column_mapping$processed_data

    # Auto-replay step 1: When settings with mapping are uploaded,
    # activate auto-replay and trigger mapping submission after a
    # short delay to let Shiny flush the selectize input updates.
    observeEvent(uploaded_data$settings_override(), {
      override <- uploaded_data$settings_override()
      if (!is.null(override) && !is.null(override$mapping)) {
        auto_replay(TRUE)
        session$userData$auto_replay_target_tab <- override$tab %||% ""
        session$userData$auto_replay_nca_ran <- isTRUE(override$nca_ran)
        log_info("Auto-replay: settings detected, will auto-advance.")
        loading_popup("Restoring session...")
        # Delay to allow mapping selectize inputs to update.
        # Check for skipped mappings before triggering submit —
        # .process_imported_mapping sets mapping_skipped during the
        # same flush cycle that populates the selectize inputs.
        shinyjs::delay(500, {
          skipped <- session$userData$mapping_skipped %||% character(0)
          if (length(skipped) > 0) {
            .abort_auto_replay(
              auto_replay,
              paste(
                "Session restore stopped: some column mappings could",
                "not be applied. Please review and continue manually."
              ),
              "Auto-replay aborted: partial mapping failure."
            )
            data_step("mapping")
            updateTabsetPanel(
              session, "data_navset", selected = "Mapping"
            )
          } else {
            trigger_mapping_submit(trigger_mapping_submit() + 1)
          }
        })
      }
    })

    # Auto-replay safety: abort if the pipeline doesn't complete within
    # 15 seconds. Covers mapping failures, unresolved duplicates, PKNCA
    # data creation errors, and any other unexpected stalls.
    observeEvent(trigger_mapping_submit(), {
      if (!auto_replay()) return()
      shinyjs::delay(15000, {
        if (auto_replay()) {
          .abort_auto_replay(
            auto_replay,
            paste(
              "Session restore stopped: the data pipeline did not",
              "complete. Please review and continue manually."
            ),
            "Auto-replay aborted: pipeline did not complete in time."
          )
        }
      })
    }, ignoreInit = TRUE)

    observeEvent(adnca_mapped(), {
      req(adnca_mapped())

      # Auto-replay step 2 (safety net): Check for partial mapping failure.
      # Primary check is in step 1 delay callback; this catches late races.
      if (auto_replay()) {
        skipped <- session$userData$mapping_skipped %||% character(0)
        if (length(skipped) > 0) {
          .abort_auto_replay(
            auto_replay,
            paste(
              "Session restore stopped: some column mappings could",
              "not be applied. Please review and continue manually."
            ),
            "Auto-replay aborted: partial mapping failure."
          )
          data_step("mapping")
          updateTabsetPanel(session, "data_navset", selected = "Mapping")
          return()
        }
      }

      data_step("filtering")
      updateTabsetPanel(session, "data_navset", selected = "Filtering")
    })

    #' Filter data
    imported_filters <- reactive(uploaded_data$settings_override()$filters)
    filtering_result <- data_filtering_server(
      "data_filtering", adnca_mapped, imported_filters
    )
    processed_data <- filtering_result$filtered_data

    # Auto-replay step 3: After filtering completes, advance to preview.
    # When filters are imported, processed_data fires twice: once with
    # unfiltered data (from raw_adnca_data change), then again after
    # auto-submit applies filters. We use a flag to skip the first fire
    # when imported filters are present.
    auto_replay_filter_pending <- reactiveVal(FALSE)

    observeEvent(uploaded_data$settings_override(), {
      override <- uploaded_data$settings_override()
      has_filters <- !is.null(override$filters) && length(override$filters) > 0
      auto_replay_filter_pending(has_filters)
    })

    observeEvent(processed_data(), {
      if (!auto_replay()) return()

      if (auto_replay_filter_pending()) {
        # First fire was from raw data change; wait for filter auto-submit
        auto_replay_filter_pending(FALSE)
        return()
      }

      data_step("preview")
      updateTabsetPanel(session, "data_navset", selected = "Preview")
    })

    #' Global variable to store grouping variables
    extra_group_vars <- column_mapping$grouping_variables
    output$processed_data_message <- renderUI({
      tryCatch(
        {
          req(processed_data())
          p(
            "This is the data set that will be used for the analysis.
          If you would like to make any changes please return to the previous tabs."
          )
        },
        error = function(e) {
          p("Please map your data in the 'Column Mapping' section before reviewing it.")
        }
      )
    })

    # Update the data table object with the filtered data
    reactable_server(
      "data_processed",
      processed_data,
      height = "50vh",
      pageSizeOptions = reactive(c(10, 25, 50, 100, nrow(processed_data())))
    )

    # Use raw data + mapping to create a PKNCA object
    #' Initializes PKNCA::PKNCAdata object from raw adnca data
    pknca_data <- reactive({
      req(processed_data())
      log_trace("Creating PKNCA::data object.")

      tryCatch({
        #' Create data object
        pknca_object <- PKNCA_create_data_object(
          adnca_data = uploaded_data$adnca_raw(),
          mapping = column_mapping$mapping(),
          applied_filters = filtering_result$applied_filters(),
          time_duplicate_rows = column_mapping$time_duplicate_rows()
        )
        ############################################################################################
        # TODO: Until PKNCA manages to simplify by default in PPORRESU its volume units,
        # this is implemented here via hardcoding in PPSTRESU
        pknca_object$units <- pknca_object$units %>%
          mutate(
            PPSTRESU = {
              new_ppstresu <- ifelse(
                PPTESTCD %in% metadata_nca_parameters$PKNCA[
                  metadata_nca_parameters$unit_type == "volume"
                ],
                sapply(PPSTRESU, function(x) simplify_unit(x, as_character = TRUE)),
                PPSTRESU
              )
              # Only accept changes producing simple units
              ifelse(nchar(new_ppstresu) < 3, new_ppstresu, .[["PPSTRESU"]])
            },
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
        purrr::walk(c("nca", "exploration", "tlg"), function(tab) {
          shinyjs::enable(selector = paste0("#page li a[data-value=", tab, "]"))
        })

        pknca_object
      }, error = function(e) {
        log_error(e$message)
        showNotification(e$message, type = "error", duration = NULL)
        NULL
      })
    }) %>%
      bindEvent(processed_data())

    # Auto-replay step 4: Signal that data processing is complete and
    # the app can navigate to the saved tab.
    auto_replay_ready <- reactiveVal(FALSE)
    observeEvent(pknca_data(), {
      if (!auto_replay()) return()
      if (is.null(pknca_data())) {
        # PKNCA data creation failed — abort auto-replay.
        # The error notification is already shown by the tryCatch above.
        auto_replay(FALSE)
        shiny::removeModal()
        return()
      }
      auto_replay(FALSE)
      auto_replay_ready(TRUE)
    })

    list(
      pknca_data = pknca_data,
      adnca_raw = uploaded_data$adnca_raw,
      extra_group_vars = extra_group_vars,
      settings_override = uploaded_data$settings_override,
      auto_replay_ready = auto_replay_ready
    )
  })
}
