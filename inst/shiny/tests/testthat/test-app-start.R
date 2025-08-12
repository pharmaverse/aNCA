describe("Test for initial app load", {
  skip_on_cran()
  # NOTE: method app$expect_values cannot be used as it crashes due to some input/output

  it("App ui does not crash and remains constant at start", {
    app <- AppDriver$new(
      name = "app_start",
      height = 407,
      width = 348,
      variant = platform_variant()
    )
    app$wait_for_idle()
    app$expect_screenshot()
  })
  
  it("App starts without JavaScript errors", {
    app <- AppDriver$new(
      name = "app_js_errors",
      height = 407,
      width = 348,
      variant = platform_variant()
    )
    
    # Wait for app to fully load
    app$wait_for_idle()
    
    # Get browser logs
    logs <- app$get_logs()
    
    # Filter for JavaScript errors (SEVERE level)
    js_errors <- logs |>
      dplyr::filter(level == "SEVERE")
    
    # Print any errors found for debugging
    if (nrow(js_errors) > 0) {
      message("JavaScript errors found:\n")
      for (i in seq_len(nrow(js_errors))) {
        message(js_errors$message[i])
      }
    }
    
    # Assert no severe JavaScript errors
    expect_equal(
      nrow(js_errors), 
      0, 
      info = paste("JavaScript errors found:", 
                   paste(js_errors$message, collapse = "\n"))
    )
    
    # Additional check: verify the document is in a complete state
    doc_ready <- app$get_js("document.readyState")
    expect_equal(doc_ready, "complete", 
                 info = "Document not in complete state")
    
    # Check that no obvious error elements are visible in the UI
    error_elements <- app$get_html(".shiny-output-error, .error, .alert-danger")
    expect_equal(length(error_elements), 0, 
                 info = "Error elements found in UI")
  })
})
