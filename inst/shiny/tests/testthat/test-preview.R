describe("Test for data preview interface", {
  skip_on_cran()

  it("Data preview appears without errors", {
    app <- AppDriver$new(
      name = "app_preview",
      height = 407,
      width = 348,
      variant = NULL
    )
    app$click("data-next_step")
    app$wait_for_idle()
    app$click("data-next_step")
    app$wait_for_idle()
    app$click("data-next_step")
    app$wait_for_idle()
    app$expect_screenshot()
  })

  it("Data preview table appears when we click the corresponding button", {
    app <- AppDriver$new(name = "app_preview_table")

    app$click("data-next_step")
    app$wait_for_idle()
    app$click("data-next_step")
    app$wait_for_idle()
    app$click("data-next_step")
    app$wait_for_idle()

    app$expect_values(output = "data-data_processed-table")
  })
  
  it("runs nca analysis", {
    app <- AppDriver$new(
      name = "app_preview_run_nca",
      height = 407,
      width = 348,
      variant = NULL
    )

    app$click("data-next_step")
    app$click("data-next_step")
    app$click("data-next_step")

    app$set_inputs("page" = "nca")
  
    # Wait for settings to be applied and button to be re-enabled
    # The button is disabled for 2750ms after settings change
    app$wait_for_js("!$('#nca-nca').prop('disabled')", timeout = 5000)
    app$click("nca-nca")
        
    app$wait_for_value(output = "nca-nca_results-myresults-table", timeout = 45000)
    

    app$set_inputs("nca-ncapanel" = "Results")
    app$wait_for_idle()
    app$wait_for_js("$('#nca-nca_results-myresults-table').length > 0", timeout = 10000)
  
    app$expect_screenshot()
  })
})
