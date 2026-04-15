#' Abort auto-replay, dismiss loading popup, and show a warning.
#' @param auto_replay ReactiveVal to reset.
#' @param message Warning message to display.
#' @param log_msg Message for the log.
#' @keywords internal
#' @noRd
.abort_auto_replay <- function(auto_replay, message, log_msg, session) {
  auto_replay(FALSE)
  session$userData$auto_replay_active <- FALSE
  shiny::removeModal()
  log_warn(log_msg)
  showNotification(message, type = "warning", duration = 10)
}

#' Set up step navigation observers (next/prev/restart buttons).
#' @param input Shiny input.
#' @param session Shiny session.
#' @param data_step ReactiveVal for current step.
#' @param trigger_mapping_submit ReactiveVal to trigger mapping.
#' @param steps Character vector of step IDs.
#' @param step_labels Character vector of step display labels.
#' @keywords internal
#' @noRd
.setup_step_navigation <- function(input, session, data_step,
                                   trigger_mapping_submit,
                                   steps, step_labels) {
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
    shinyjs::disable("next_step")
    current_step <- isolate(data_step())
    if (current_step %in% c("upload", "filtering")) {
      idx <- match(current_step, steps)
      data_step(steps[idx + 1])
      updateTabsetPanel(
        session, "data_navset", selected = step_labels[idx + 1]
      )
    } else if (current_step == "mapping") {
      trigger_mapping_submit(trigger_mapping_submit() + 1)
    } else if (current_step == "preview") {
      shinyjs::runjs(
        "document.querySelector(`a[data-value='exploration']`).click();"
      )
    }
  })

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
    updateTabsetPanel(
      session, "data_navset", selected = step_labels[idx - 1]
    )
  })
}


#' Start auto-replay: store target tab, show loading popup, and
#' trigger mapping submission after a delay. Aborts if any column
#' mappings were skipped.
#' @param override List from settings_override (must contain `tab`,
#'   `nca_ran`, `filters`).
#' @param auto_replay ReactiveVal tracking auto-replay state.
#' @param data_step ReactiveVal for current step.
#' @param trigger_mapping_submit ReactiveVal to trigger mapping.
#' @param session Shiny session.
#' @keywords internal
#' @noRd
.start_auto_replay <- function(override, auto_replay, data_step,
                               trigger_mapping_submit, session) {
  auto_replay(TRUE)
  session$userData$auto_replay_active <- TRUE
  session$userData$auto_replay_target_tab <- override$tab %||% ""
  session$userData$auto_replay_nca_ran <- isTRUE(override$nca_ran)
  has_filters <- !is.null(override$filters) && length(override$filters) > 0
  session$userData$auto_replay_filter_pending <- has_filters
  log_info("Auto-replay: settings detected, will auto-advance.")
  loading_popup("Restoring session...")

  shinyjs::delay(500, {
    skipped <- session$userData$mapping_skipped %||% character(0)
    if (length(skipped) > 0) {
      .abort_auto_replay(
        auto_replay,
        paste(
          "Session restore stopped: some column mappings could",
          "not be applied. Please review and continue manually."
        ),
        "Auto-replay aborted: partial mapping failure.",
        session
      )
      data_step("mapping")
      updateTabsetPanel(session, "data_navset", selected = "Mapping")
    } else {
      trigger_mapping_submit(trigger_mapping_submit() + 1)
    }
  })
}

#' Set up auto-replay observers for restoring a session from settings.
#' Wires up the pipeline: settings upload -> mapping -> filtering ->
#' preview -> signal ready. Includes a 15s safety timeout.
#' @param uploaded_data List of reactives from data_upload_server.
#' @param auto_replay ReactiveVal tracking auto-replay state.
#' @param data_step ReactiveVal for current step.
#' @param trigger_mapping_submit ReactiveVal to trigger mapping.
#' @param processed_data Reactive for filtered data.
#' @param pknca_data Reactive for PKNCA data object.
#' @param session Shiny session.
#' @returns ReactiveVal that signals when auto-replay is complete.
#' @keywords internal
#' @noRd
.setup_auto_replay <- function(uploaded_data, auto_replay, data_step,
                               trigger_mapping_submit,
                               processed_data, pknca_data, session) {
  # Step 1: Detect settings upload and trigger mapping submission.
  observeEvent(uploaded_data$settings_override(), {
    override <- uploaded_data$settings_override()
    if (is.null(override) || is.null(override$mapping)) return()
    .start_auto_replay(
      override, auto_replay, data_step, trigger_mapping_submit, session
    )
  })

  # Safety timeout: abort if pipeline doesn't complete within 15s.
  # The delay callback checks auto_replay() itself, so no early return
  # needed — the timer is harmless if auto-replay finishes first.
  observeEvent(trigger_mapping_submit(), {
    shinyjs::delay(15000, {
      if (auto_replay()) {
        .abort_auto_replay(
          auto_replay,
          paste(
            "Session restore stopped: the data pipeline did not",
            "complete. Please review and continue manually."
          ),
          "Auto-replay aborted: pipeline did not complete in time.",
          session
        )
      }
    })
  }, ignoreInit = TRUE)

  # Step 2 (safety net) is handled by the adnca_mapped observer in
  # tab_data_server, which also handles the normal advance to filtering.

  # Step 3: After filtering completes, advance to preview.
  # When filters are imported, processed_data fires twice: once with
  # unfiltered data, then again after auto-submit. Skip the first fire.
  # The pending flag is set in .start_auto_replay via session$userData.
  observeEvent(processed_data(), {
    if (!auto_replay()) return()
    if (isTRUE(session$userData$auto_replay_filter_pending)) {
      session$userData$auto_replay_filter_pending <- FALSE
      return()
    }
    data_step("preview")
    updateTabsetPanel(session, "data_navset", selected = "Preview")
  })

  # Step 4: Signal that data processing is complete.
  auto_replay_ready <- reactiveVal(FALSE)
  observeEvent(pknca_data(), {
    if (!auto_replay()) return()
    if (is.null(pknca_data())) {
      auto_replay(FALSE)
      session$userData$auto_replay_active <- FALSE
      shiny::removeModal()
      return()
    }
    auto_replay(FALSE)
    # Keep auto_replay_active TRUE until NCA auto-run completes (cleared
    # in tab_nca_server) so intermediate notifications stay suppressed.
    auto_replay_ready(TRUE)
  })

  auto_replay_ready
}

# TODO: Remove once PKNCA simplifies volume units in PPORRESU by default.
#' Simplify volume-type units in a PKNCA units table.
#' @param units A data.frame with PPTESTCD, PPORRESU, PPSTRESU,
#'   conversion_factor columns.
#' @returns The units data.frame with simplified volume units.
#' @keywords internal
#' @noRd
.simplify_volume_units <- function(units) {
  vol_params <- metadata_nca_parameters$PKNCA[
    metadata_nca_parameters$unit_type == "volume"
  ]
  is_vol <- units$PPTESTCD %in% vol_params

  new_ppstresu <- ifelse(
    is_vol,
    sapply(units$PPSTRESU, function(x) {
      simplify_unit(x, as_character = TRUE)
    }),
    units$PPSTRESU
  )
  # Only accept changes producing simple units
  new_ppstresu <- ifelse(
    nchar(new_ppstresu) < 3, new_ppstresu, units$PPSTRESU
  )

  units$PPSTRESU <- new_ppstresu
  units$conversion_factor <- ifelse(
    is_vol,
    get_conversion_factor(units$PPORRESU, units$PPSTRESU),
    units$conversion_factor
  )
  units
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

    auto_replay <- reactiveVal(FALSE)

    .setup_step_navigation(
      input, session, data_step, trigger_mapping_submit, steps, step_labels
    )
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

    # Advance to filtering after mapping completes.
    # Also acts as auto-replay step 2 safety net: if mappings were
    # skipped, abort auto-replay and stay on mapping.
    observeEvent(adnca_mapped(), {
      req(adnca_mapped())
      if (auto_replay()) {
        skipped <- session$userData$mapping_skipped %||% character(0)
        if (length(skipped) > 0) {
          .abort_auto_replay(
            auto_replay,
            paste(
              "Session restore stopped: some column mappings could",
              "not be applied. Please review and continue manually."
            ),
            "Auto-replay aborted: partial mapping failure.",
            session
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
    pknca_data <- reactive({
      req(processed_data())
      log_trace("Creating PKNCA::data object.")

      tryCatch({
        pknca_object <- PKNCA_create_data_object(
          adnca_data = uploaded_data$adnca_raw(),
          mapping = column_mapping$mapping(),
          applied_filters = filtering_result$applied_filters(),
          time_duplicate_rows = column_mapping$time_duplicate_rows()
        )
        pknca_object$units <- .simplify_volume_units(pknca_object$units)
        log_success("PKNCA data object created.")

        purrr::walk(c("nca", "exploration", "tlg"), function(tab) {
          shinyjs::enable(
            selector = paste0("#page li a[data-value=", tab, "]")
          )
        })

        pknca_object
      }, error = function(e) {
        log_error(e$message)
        showNotification(e$message, type = "error", duration = NULL)
        NULL
      })
    }) %>%
      bindEvent(processed_data())

    auto_replay_ready <- .setup_auto_replay(
      uploaded_data, auto_replay, data_step, trigger_mapping_submit,
      processed_data, pknca_data, session
    )

    list(
      pknca_data = pknca_data,
      adnca_raw = uploaded_data$adnca_raw,
      extra_group_vars = extra_group_vars,
      settings_override = uploaded_data$settings_override,
      auto_replay_ready = auto_replay_ready
    )
  })
}
