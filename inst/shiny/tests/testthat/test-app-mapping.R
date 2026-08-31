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

  it("dismisses the loading modal and advances past mapping (#1420)", {
    # With the default data (no duplicates) and unchanged mappings, submitting
    # the mapping must dismiss the "Processing data mapping..." loading modal
    # and advance to the Filtering step. Previously the modal hung forever: the
    # submit was triggered in the same flush as showModal(), so the pipeline's
    # removeModal() was batched with the show and dropped mid Bootstrap
    # fade-in. Deferring the submit lets the show paint first so the later hide
    # applies.
    app <- AppDriver$new(name = "app_mapping_advance")

    app$click("data-next_step") # advance from upload to mapping
    app$wait_for_idle()
    app$click("data-next_step") # advance from mapping to submit (must reach filtering)
    app$wait_for_idle()

    # Navigation advanced ...
    expect_equal(app$get_value(input = "data-data_navset"), "Filtering")

    # ... and the loading modal is actually gone from the DOM. This is the part
    # that would fail if removeModal() raced the fade-in: the spinner element
    # would still be present even though processing finished.
    modal_html <- app$get_html(".modal-content")
    expect_null(modal_html)
  })
})
