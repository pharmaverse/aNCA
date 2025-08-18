describe("Test for filter interface", {
  skip_on_cran()
  # NOTE: method app$expect_values cannot be used as it crashes due to some output

  it("filter interface appears without errors", {
    app <- AppDriver$new(
      name = "app_filter",
      height = 407,
      width = 348,
      variant = NULL
    )
    app$click("data-next_step")
    app$wait_for_idle()
    app$expect_screenshot()
  })

  it("filter interface only appears when clicking next button", {
    app <- AppDriver$new(name = "app_filter_output")

    # filter_table not found yet
    expect_null(app$get_value(output = "data-data_filtering-filtered_data_display-table"))
    input_initial_values <- app$get_values(input = TRUE)

    app$click("data-next_step")
    app$wait_for_idle()

    input_filter_values <- app$get_values(input = TRUE)
    expect_true(length(setdiff(
        names(input_filter_values[["input"]]),
        names(input_initial_values[["input"]])
     )) > 0)

    app$expect_values(output = "data-data_filtering-filtered_data_display-table")
  })
})
