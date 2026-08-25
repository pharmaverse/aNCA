describe("Test for mapping interface", {
  skip_on_cran()

  it("mapping interface appears without errors", {
    app <- AppDriver$new(
      name = "app_mapping",
      variant = NULL
    )
    app$click("data-next_step")
    app$wait_for_idle()
    app$click("data-next_step")
    app$wait_for_idle()
    app$expect_values(input = "data-column_mapping-select_STUDYID", screenshot_args = FALSE)
  })


  it("new inputs appear with mapping interface", {
    app <- AppDriver$new(name = "app_mapping_inputs")
    namespace_mapping <- "data-column_mapping"

    # mapping inputs should be NULL at app start
    input_initial_values <- app$get_values(input = TRUE)

    app$click("data-next_step")
    app$wait_for_idle()
    app$click("data-next_step")

    input_mapping_values <- app$get_values(input = TRUE)
    mapping_inputs_set <-
      input_mapping_values[["input"]][grepl(
        namespace_mapping, names(input_mapping_values[["input"]]))] # nolint: indentation linter
    expect_true(length(setdiff(
      names(input_mapping_values[["input"]]),
      names(input_initial_values[["input"]])
    )) > 0)

    # mapping inputs are not null after clicking
    expect_false(any(purrr::map_lgl(mapping_inputs_set, is.null)))
  })

  it("advances past mapping on the default path without hanging (#1420)", {
    # With the default data (no duplicates) and unchanged mappings, submitting
    # the mapping must complete and advance to the Filtering step. Previously the
    # "Processing data mapping..." modal hung forever because the completion
    # callback lived in a reactive keyed only on values that never changed.
    app <- AppDriver$new(name = "app_mapping_advance")

    app$click("data-next_step") # upload -> mapping
    app$wait_for_idle()
    app$click("data-next_step") # mapping -> submit (must reach filtering)
    app$wait_for_idle()

    expect_equal(app$get_value(input = "data-data_navset"), "Filtering")
  })
})
