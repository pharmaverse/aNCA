#' NCA Setup Server Module
#'
#' This module handles the server-side logic for the NCA settings, including file uploads,
#' analyte/dose/specimen selection, AUC intervals, NCA parameters, as well as slope selection and
#' their unit specification. Then it integrates them to adnca_data object and returns data
#' that is processed accordingly.
#'
#' @param id ID of the module.
#' @param data Reactive with data table containing raw data uploaded to the app.
#' @param adnca_data Reactive with `PKNCAdata` object including the `adnca_data`.
#'
#' @returns List with three reactive expressions:
#'   * processed_pknca_data - PKNCAdata object with applied settings and slope selections.
#'   * settings - List with raw settings as gathered by the module.
#'   * slope_rules - Data frame with slope inclusions / exclusions provided by slope selector.

nca_setup_ui <- function(id) {
  ns <- NS(id)

  navset_pill_list(
    nav_panel(
      "Settings",
      fluidRow(
        actionButton(
          ns("open_save_settings_modal"),
          label = "Download settings",
          icon = icon("download"),
          class = "btn btn-default"
        )
      ),
      fluidRow(units_table_ui(ns("units_table"))),
      settings_ui(ns("nca_settings")),
      accordion(
        accordion_panel(
          title = "Ratio Calculations",
          ratios_table_ui(ns("ratio_calculations_table"))
        ),
        open = c("General Settings", "Parameter Selection")
      )
    ),
    nav_panel("Parameter Selection", parameter_selection_ui(ns("nca_setup_parameter"))),
    nav_panel("Slope Selector", slope_selector_ui(ns("slope_selector"))),
    nav_panel("General Exclusions", general_exclusions_ui(ns("general_exclusions")))
  )
}

nca_setup_server <- function(id, data, adnca_data, extra_group_vars, settings_override) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    imported_settings <- reactive(settings_override()$settings)
    imported_slopes <- reactive(settings_override()$slope_rules)
    imported_params <- reactive(imported_settings()$parameters$selections)
    imported_ratios <- reactive(imported_settings()$ratio_table)
    general_excl_override <- reactive(imported_settings()$general_exclusions)

    # Gather all settings from the appropriate module
    settings_output <- settings_server(
      "nca_settings",
      data,
      adnca_data,
      imported_settings
    )
    settings <- settings_output$all

    general_exclusions <- general_exclusions_server(
      "general_exclusions",
      processed_pknca_data,
      general_excl_override
    )

    # Lightweight filtered data for parameter selection — only depends on
    # analyte/pcspec, not on method, slopes, flags, or exclusions.
    # Filters conc$data directly instead of building a full PKNCA data object.
    param_selection_data <- reactive({
      req(adnca_data(), settings_output$analyte(), settings_output$pcspec())
      log_trace("Updating parameter selection data.")

      pknca_data <- adnca_data()
      pknca_data$conc$data <- pknca_data$conc$data %>%
        dplyr::filter(
          PARAM %in% settings_output$analyte(),
          PCSPEC %in% settings_output$pcspec(),
          ATPTREF %in% settings_output$profile()
        )
      pknca_data
    })

    parameters_output <- parameter_selection_server(
      "nca_setup_parameter",
      param_selection_data,
      imported_params
    )

    final_settings <- reactive({

      req(settings(), parameters_output$selections(), general_exclusions())

      current_settings <- settings()
      current_settings$general_exclusions <- general_exclusions()
      current_settings$parameters <- list(
        selections = parameters_output$selections(),
        types_df = parameters_output$types_df()
      )
      current_settings
    })

    # Update pknca data object and intervals using summary output
    processed_pknca_data <- reactive({
      req(adnca_data(), settings(),
          parameters_output$selections(), parameters_output$types_df())

      log_trace("Updating PKNCA::data object.")

      base_pknca_data <- PKNCA_update_data_object(
        adnca_data = adnca_data(),
        method = settings()$method,
        selected_analytes = settings()$analyte,
        selected_profile = settings()$profile,
        selected_pcspec = settings()$pcspec,
        should_impute_c0 = settings()$data_imputation$impute_c0,
        hl_adj_rules = slope_rules(),
        exclusion_list = general_exclusions(),
        keep_interval_cols = extra_group_vars()
      )

      # Show bioavailability widget if it is possible to calculate
      if (base_pknca_data$dose$data$std_route %>% unique() %>% length() == 2) {
        shinyjs::show(selector = ".bioavailability-picker")
      } else {
        shinyjs::hide(selector = ".bioavailability-picker")
      }

      # Call the updated function with the direct inputs
      final_data <- update_main_intervals(
        data = base_pknca_data,
        parameter_selections = parameters_output$selections(),
        study_types_df = parameters_output$types_df(),
        int_parameters = settings()$int_parameters,
        impute = settings()$data_imputation$impute_c0,
        blq_imputation_rule = settings()$data_imputation$blq_imputation_rule
      )

      if (nrow(final_data$intervals) == 0) {
        showNotification(
          "All intervals were filtered. Please revise your settings",
          type = "warning",
          duration = 10
        )
      }

      final_data
    })

    # Keep the post processing ratio calculations requested by the user
    ratio_table <- ratios_table_server(
      id = "ratio_calculations_table",
      adnca_data = processed_pknca_data,
      extra_group_vars = extra_group_vars,
      imported_ratios = imported_ratios
    )

    # Automatically update the units table when settings are uploaded.
    observeEvent(imported_settings(), {
      req(imported_settings()$units, nrow(imported_settings()$units) > 0)

      imported_units <- imported_settings()$units
      has_full_units <- all(c("PPORRESU", "conversion_factor") %in% names(imported_units))

      if (has_full_units) {
        session$userData$units_table(imported_units)
      } else {
        # Defaults-only format: wait for data-derived units, then resolve.
        observe({
          req(processed_pknca_data())
          data_units <- processed_pknca_data()$units
          merged <- apply_unit_defaults(imported_units, data_units)

          if (nrow(merged$failed) > 0) {
            msg <- paste0(
              "Could not convert units for: ",
              paste(merged$failed$PPTESTCD, collapse = ", "),
              ". Data defaults will be used for these parameters."
            )
            log_warn(msg)
            showNotification(msg, type = "warning", duration = 10)
          }

          session$userData$units_table(merged$units)
        }) %>%
          bindEvent(processed_pknca_data(), once = TRUE)
      }
    })

    # Parameter unit changes option: Opens a modal message with a units table to edit
    units_table_server("units_table", processed_pknca_data)

    # Collect all half life manual adjustments done in the `Slope Selector` section
    # and controls the half life plots that are displayed
    slope_rules <- slope_selector_server(
      "slope_selector",
      processed_pknca_data,
      imported_slopes
    )

    # Open comment modal before downloading settings
    observeEvent(input$open_save_settings_modal, {
      showModal(modalDialog(
        title = "Save Settings",
        textInput(
          ns("settings_save_comment"),
          label = "Comment (optional)",
          placeholder = "e.g. final NCA, first draft"
        ),
        footer = tagList(
          downloadButton(ns("settings_download"), "Save", class = "btn-primary"),
          modalButton("Cancel")
        ),
        easyClose = TRUE,
        size = "m"
      ))
    })

    output$settings_download <- downloadHandler(
      filename = function() {
        paste0(
          session$userData$project_prefix("_"),
          "settings_", Sys.Date(), ".yaml"
        )
      },
      content = function(con) {
        export_settings <- final_settings()
        if (!is.null(export_settings$units)) {
          export_settings$units <- export_settings$units %>%
            dplyr::filter(!default) %>%
            dplyr::select(-default)
        }
        export_settings$ratio_table <- ratio_table()

        payload <- c(
          export_settings,
          list(
            slope_rules = slope_rules(),
            filters = session$userData$applied_filters
          )
        )

        dataset_name <- session$userData$dataset_filename %||% ""

        active_tab <- tryCatch(
          session$userData$active_tab(),
          error = function(e) ""
        )

        new_version <- create_settings_version(
          settings_data = payload,
          comment = input$settings_save_comment %||% "",
          dataset = dataset_name,
          tab = active_tab
        )

        existing <- tryCatch(
          session$userData$settings_versions(),
          error = function(e) list()
        )
        if (is.null(existing)) existing <- list()

        versions <- add_settings_version(existing, new_version)
        session$userData$settings_versions(versions)

        write_versioned_settings(versions, con)
        removeModal()
      }
    )

    list(
      processed_pknca_data = processed_pknca_data,
      settings = final_settings,
      ratio_table = ratio_table,
      slope_rules = slope_rules
    )
  })
}
