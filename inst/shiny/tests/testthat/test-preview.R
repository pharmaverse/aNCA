describe("Tests for app preview", {
  skip_on_cran()

  it("table appears in preview section", {

    app <- AppDriver$new(name = "app_preview")
    app$click("data-next_step")
    app$click("data-next_step")
    app$wait_for_idle(timeout = 5000)
    app$click("data-next_step")
    app$wait_for_idle(timeout = 5000)
    table <- app$get_value(output = "data-data_processed-table")
    expect_true(jsonlite::validate(table))
  })

  it("runs nca analysis", {
    app <- AppDriver$new(name = "app_preview_run_nca")

    app$click("data-next_step")
    app$click("data-next_step")
    app$wait_for_idle(timeout = 5000)
    app$click("data-next_step")
    app$wait_for_idle(timeout = 5000)

    app$set_inputs("page" = "nca")

    # Wait for settings to be applied and button to be re-enabled
    # The button is disabled for 2750ms after settings change
    app$wait_for_js("!$('#nca-run_nca').prop('disabled')", timeout = 5000)
    app$click("nca-run_nca")

    app$wait_for_value(output = "nca-nca_results-myresults-table", timeout = 45000)
    table <- app$get_value(output = "nca-nca_results-myresults-table")
    expect_true(jsonlite::validate(table))
  })

  it("blocks NCA when required selectors are empty", {
    app <- AppDriver$new(name = "app_preview_empty_nca_selectors")

    app$click("data-next_step")
    app$click("data-next_step")
    app$wait_for_idle(timeout = 5000)
    app$click("data-next_step")
    app$wait_for_idle(timeout = 5000)

    app$set_inputs("page" = "nca")
    app$wait_for_js("!$('#nca-run_nca').prop('disabled')", timeout = 5000)
    app$set_inputs(
      "nca-nca_setup-nca_settings-select_analyte" = character(),
      "nca-nca_setup-nca_settings-select_pcspec" = character(),
      "nca-nca_setup-nca_settings-select_profile" = character()
    )
    app$wait_for_js("!$('#nca-run_nca').prop('disabled')", timeout = 5000)
    app$click("nca-run_nca")

    selector_message <- paste0(
      "Select at least one analyte, specimen, profile before running NCA."
    )
    app$wait_for_js(
      paste0(
        "$('.shiny-notification-error').text().includes('",
        selector_message,
        "')"
      ),
      timeout = 5000
    )
    expect_null(app$get_value(output = "nca-nca_results-myresults-table"))
  })

  it("explains when no suitable parameters are selected", {
    app <- AppDriver$new(name = "app_preview_no_parameters")

    app$click("data-next_step")
    app$click("data-next_step")
    app$wait_for_idle(timeout = 5000)
    app$click("data-next_step")
    app$wait_for_idle(timeout = 5000)

    app$set_inputs("page" = "nca")
    app$wait_for_js(
      "$('#nca-nca_setup-nca_setup_parameter-clear_all').length > 0",
      timeout = 5000
    )
    app$click("nca-nca_setup-nca_setup_parameter-clear_all")
    app$wait_for_js("!$('#nca-run_nca').prop('disabled')", timeout = 5000)
    app$click("nca-run_nca")

    parameter_message <- "No suitable parameters selected for NCA calculation."
    app$wait_for_js(
      paste0(
        "$('.shiny-notification-error').text().includes('",
        parameter_message,
        "')"
      ),
      timeout = 5000
    )
    expect_null(app$get_value(output = "nca-nca_results-myresults-table"))
  })

  it("explains when no valid intervals are available", {
    notifications <- list()
    log_messages <- character()
    mockery::stub(
      .validate_nca_run,
      "showNotification",
      function(ui, type, duration, ...) {
        notifications[[length(notifications) + 1]] <<- list(
          ui = ui, type = type, duration = duration
        )
      }
    )
    mockery::stub(
      .validate_nca_run,
      "log_error",
      function(message, ...) {
        log_messages <<- c(log_messages, message)
      }
    )

    result <- shiny::isolate(
      .validate_nca_run(
        general_settings = list(
          analyte = shiny::reactive("A"),
          pcspec = shiny::reactive("PLASMA"),
          profile = shiny::reactive("NCA")
        ),
        processed_pknca_data = shiny::reactive(
          list(intervals = data.frame())
        ),
        auto_nca_running = shiny::reactive(FALSE),
        session = NULL
      )
    )

    expect_null(result)
    expect_length(notifications, 1)
    expect_match(
      notifications[[1]]$ui,
      paste(
        "NCA cannot run because no valid intervals are available",
        "for the current data and settings.",
        "Review selections, filters, and parameter settings."
      )
    )
    expect_identical(notifications[[1]]$type, "error")
    expect_null(notifications[[1]]$duration)
    expect_identical(log_messages, "No valid NCA intervals available")
  })

})
