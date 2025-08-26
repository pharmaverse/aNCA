describe("Test for mapping interface", {
  skip_on_cran()

  it("mapping interface appears without errors", {
    app <- AppDriver$new(
      name = "app_mapping",
      height = 407,
      width = 348,
      variant = NULL
    )
    app$click("data-next_step")
    app$wait_for_idle()
    app$click("data-next_step")
    app$wait_for_idle()
    app$expect_values(input = "data-column_mapping-select_STUDYID")
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
})
