describe("Test for mapping interface", {
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
})
