library(shinytest2)

test_that("End to End test pass", {
  # based on: https://rstudio.github.io/shinytest2/articles/use-package.html#applications-in-inst
  skip()
  skip_on_cran()
  skip()
  appdir <- system.file(package = "aNCA", "shiny")

  expect_no_error(test_app(appdir))
})
