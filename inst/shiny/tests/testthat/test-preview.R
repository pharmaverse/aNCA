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

    app$wait_for_value(output = "nca-nca_results-myresults-table", timeout = 60000)
    table <- app$get_value(output = "nca-nca_results-myresults-table")
    expect_true(jsonlite::validate(table))
  })
})
