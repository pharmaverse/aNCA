test_that("the installed app connects and displays the upload screen", {
  skip_if_not(identical(Sys.getenv("ANCA_RUN_BROWSER_TESTS"), "true"),
              "Set ANCA_RUN_BROWSER_TESTS=true to run the browser smoke test")
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  app <- shinytest2::AppDriver$new(
    app_dir = system.file("shiny", package = "aNCA"),
    name = "cran_startup", load_timeout = 30000, timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle()
  expect_true(app$get_js("document.readyState === 'complete'"))
  expect_true(app$get_js(
    "Shiny.shinyapp.$socket.readyState === 1"
  ))
  expect_true(app$get_js("document.body.innerText.includes('Upload File')"))
  expect_equal(app$get_js("document.querySelectorAll('.shiny-output-error').length"), 0)
})
